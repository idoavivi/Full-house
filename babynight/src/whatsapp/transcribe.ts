import axios from 'axios';
import FormData from 'form-data';
import OpenAI from 'openai';
import { config } from '../config';

const WA_API_BASE = 'https://graph.facebook.com/v19.0';

/** Download a WhatsApp media file as a Buffer */
async function downloadWhatsAppMedia(mediaId: string): Promise<{ buffer: Buffer; mimeType: string }> {
  // Step 1: Get the media URL
  const metaRes = await axios.get(`${WA_API_BASE}/${mediaId}`, {
    headers: { Authorization: `Bearer ${config.whatsappAccessToken}` },
  });

  const mediaUrl = metaRes.data.url as string;
  const mimeType = (metaRes.data.mime_type as string) || 'audio/ogg';

  // Step 2: Download the actual file
  const fileRes = await axios.get(mediaUrl, {
    headers: { Authorization: `Bearer ${config.whatsappAccessToken}` },
    responseType: 'arraybuffer',
  });

  return { buffer: Buffer.from(fileRes.data as ArrayBuffer), mimeType };
}

/** Transcribe a WhatsApp voice note using OpenAI Whisper */
export async function transcribeVoiceNote(mediaId: string): Promise<string> {
  if (!process.env.OPENAI_API_KEY) {
    throw new Error('OPENAI_API_KEY not set — cannot transcribe voice notes');
  }

  const { buffer, mimeType } = await downloadWhatsAppMedia(mediaId);

  // Map MIME type to a file extension Whisper accepts
  const ext = mimeType.includes('ogg') ? 'ogg'
    : mimeType.includes('mp4') ? 'mp4'
    : mimeType.includes('mpeg') || mimeType.includes('mp3') ? 'mp3'
    : mimeType.includes('webm') ? 'webm'
    : 'ogg';

  const openai = new OpenAI({ apiKey: process.env.OPENAI_API_KEY });

  // Whisper API requires a File-like object
  const form = new FormData();
  form.append('file', buffer, { filename: `voice.${ext}`, contentType: mimeType });
  form.append('model', 'whisper-1');
  // Hint at both Hebrew and English — Whisper will auto-detect
  form.append('language', 'he');

  const response = await axios.post(
    'https://api.openai.com/v1/audio/transcriptions',
    form,
    {
      headers: {
        Authorization: `Bearer ${process.env.OPENAI_API_KEY}`,
        ...form.getHeaders(),
      },
    }
  );

  const transcript = (response.data as { text: string }).text?.trim();
  if (!transcript) throw new Error('Empty transcription returned');

  console.log(`[Transcribe] Voice note → "${transcript}"`);
  return transcript;
}
