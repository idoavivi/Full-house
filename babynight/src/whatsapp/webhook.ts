import { Request, Response } from 'express';
import { config } from '../config';
import { isMessageProcessed } from '../db/queries';
import { sendMessage } from './sender';
import { processMessage } from '../ai/brain';
import { exportEventsCSV } from '../db/queries';
import { transcribeVoiceNote } from './transcribe';

// Track in-flight requests per phone to prevent race conditions
const processing = new Set<string>();

/**
 * GET /webhook — WhatsApp verification challenge
 */
export function handleWebhookVerification(req: Request, res: Response): void {
  const mode = req.query['hub.mode'];
  const token = req.query['hub.verify_token'];
  const challenge = req.query['hub.challenge'];

  if (mode === 'subscribe' && token === config.whatsappVerifyToken) {
    console.log('[Webhook] Verification successful');
    res.status(200).send(challenge);
  } else {
    console.warn('[Webhook] Verification failed — token mismatch');
    res.sendStatus(403);
  }
}

/**
 * POST /webhook — Incoming WhatsApp messages
 */
export async function handleWebhookMessage(req: Request, res: Response): Promise<void> {
  // Always respond 200 immediately — WhatsApp retries if it doesn't get a fast ack
  res.sendStatus(200);

  try {
    const body = req.body;

    // Validate it's a WhatsApp message
    if (body?.object !== 'whatsapp_business_account') return;

    const entry = body?.entry?.[0];
    const change = entry?.changes?.[0];
    const value = change?.value;
    const messages = value?.messages;

    if (!messages || messages.length === 0) return;

    for (const message of messages) {
      await handleSingleMessage(message, value?.metadata?.phone_number_id);
    }
  } catch (err) {
    console.error('[Webhook] Unhandled error:', err);
  }
}

async function handleSingleMessage(
  message: Record<string, unknown>,
  _phoneNumberId: string
): Promise<void> {
  const messageType = message.type as string;

  // Only handle text and audio messages
  if (messageType !== 'text' && messageType !== 'audio') {
    console.log(`[Webhook] Ignoring message type: ${messageType}`);
    return;
  }

  const waMessageId = message.id as string;
  const from = message.from as string;

  let text: string;

  if (messageType === 'audio') {
    // Voice note — transcribe it first
    const mediaId = (message.audio as { id: string })?.id;
    if (!mediaId) return;

    // Only process messages from authorised parents (check early for audio too)
    if (!config.parents[from]) {
      console.log(`[Webhook] Audio from unknown number ${from} — ignored`);
      return;
    }

    if (isMessageProcessed(waMessageId)) return;

    if (!process.env.OPENAI_API_KEY) {
      await sendMessage(from, "Voice notes aren't set up yet — please text me for now 🙏");
      return;
    }

    try {
      text = await transcribeVoiceNote(mediaId);
      console.log(`[Webhook] Transcribed voice note from ${from}: "${text}"`);
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err);
      console.error(`[Webhook] Transcription failed: ${msg}`);
      await sendMessage(from, "Sorry, I couldn't understand that voice note — could you type it? 🙏");
      return;
    }
  } else {
    text = (message.text as { body: string })?.body?.trim();
    if (!text) return;
  }

  // Deduplicate: WhatsApp sends webhooks multiple times
  if (isMessageProcessed(waMessageId)) {
    console.log(`[Webhook] Duplicate message ignored: ${waMessageId}`);
    return;
  }

  // Only process messages from authorised parents
  if (!config.parents[from]) {
    console.log(`[Webhook] Message from unknown number ${from} — ignored`);
    return;
  }

  // Prevent concurrent processing of messages from same number
  if (processing.has(from)) {
    console.log(`[Webhook] Already processing message from ${from} — queuing skipped`);
    return;
  }

  processing.add(from);

  try {
    const reportedBy = config.parents[from]?.name || from;
    console.log(`[Webhook] Message from ${reportedBy}: "${text}"`);

    // Handle export command
    if (text.toLowerCase() === 'export') {
      const csv = exportEventsCSV();
      await sendMessage(from, `📊 Here's your data export:\n\n\`\`\`\n${csv.slice(0, 3500)}\n\`\`\``);
      return;
    }

    // Send to AI brain for processing
    const reply = await processMessage({
      phone: from,
      messageText: text,
      waMessageId,
      reportedBy,
    });

    await sendMessage(from, reply);

  } catch (err) {
    console.error('[Webhook] Error processing message:', err);
    // Always acknowledge the parent so they're not left hanging
    try {
      await sendMessage(from, "Got it! Give me just a moment... 🕐");
    } catch (sendErr) {
      console.error('[Webhook] Failed to send error acknowledgement:', sendErr);
    }
  } finally {
    processing.delete(from);
  }
}
