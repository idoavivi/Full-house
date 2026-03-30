import axios from 'axios';
import { config } from '../config';

const WA_API_BASE = 'https://graph.facebook.com/v19.0';
const MAX_MESSAGE_LENGTH = 4096;

/** Truncate a message to WhatsApp's 4096-char limit */
function truncate(text: string): string {
  if (text.length <= MAX_MESSAGE_LENGTH) return text;
  const suffix = '\n\n_(message truncated)_';
  return text.slice(0, MAX_MESSAGE_LENGTH - suffix.length) + suffix;
}

/** Send a text message to a WhatsApp number */
export async function sendMessage(to: string, text: string): Promise<void> {
  if (!config.whatsappPhoneNumberId || !config.whatsappAccessToken) {
    console.log(`[WhatsApp MOCK] To: ${to}\n${text}\n`);
    return;
  }

  const url = `${WA_API_BASE}/${config.whatsappPhoneNumberId}/messages`;

  try {
    await axios.post(
      url,
      {
        messaging_product: 'whatsapp',
        recipient_type: 'individual',
        to,
        type: 'text',
        text: { body: truncate(text), preview_url: false },
      },
      {
        headers: {
          Authorization: `Bearer ${config.whatsappAccessToken}`,
          'Content-Type': 'application/json',
        },
      }
    );
    console.log(`[WhatsApp] Sent to ${to}: ${text.slice(0, 80)}...`);
  } catch (err: unknown) {
    if (axios.isAxiosError(err)) {
      const data = err.response?.data as { error?: { message?: string; code?: number } } | undefined;
      const msg = data?.error?.message || err.message;
      const code = data?.error?.code || err.response?.status;
      console.error(`[WhatsApp] Send failed (${code}): ${msg}`);
    } else {
      console.error('[WhatsApp] Send failed:', err instanceof Error ? err.message : String(err));
    }
    throw err;
  }
}

/** Send a document (CSV file) to a WhatsApp number via URL */
export async function sendDocument(to: string, filename: string, content: string): Promise<void> {
  // WhatsApp document sending requires hosting the file and providing a URL.
  // Since we can't easily host arbitrary files, we'll send the CSV content
  // as a text message split into chunks if necessary.
  const header = `📊 *${filename}*\n\n`;
  const chunkSize = MAX_MESSAGE_LENGTH - header.length - 20;

  if (content.length <= chunkSize) {
    await sendMessage(to, header + '```\n' + content + '\n```');
    return;
  }

  // Send in chunks
  const lines = content.split('\n');
  const headerLine = lines[0];
  let chunk = header + '```\n' + headerLine + '\n';
  let partNum = 1;

  for (let i = 1; i < lines.length; i++) {
    const line = lines[i] + '\n';
    if (chunk.length + line.length > MAX_MESSAGE_LENGTH - 10) {
      await sendMessage(to, chunk + '```');
      chunk = `_(Part ${++partNum})_\n\`\`\`\n${headerLine}\n`;
    }
    chunk += line;
  }

  if (chunk.trim()) {
    await sendMessage(to, chunk + '```');
  }
}

/** Send a message to ALL configured parents */
export async function broadcastToParents(text: string): Promise<void> {
  const phones = Object.keys(config.parents);
  if (phones.length === 0) {
    console.warn('[WhatsApp] No parent phone numbers configured — broadcast skipped');
    return;
  }
  await Promise.all(phones.map(phone => sendMessage(phone, text)));
}
