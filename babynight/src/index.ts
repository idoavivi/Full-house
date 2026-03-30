import express from 'express';
import { config } from './config';
import { getDb } from './db/schema';
import { handleWebhookVerification, handleWebhookMessage } from './whatsapp/webhook';
import { scheduleCheckIns } from './scheduler/checkins';

const app = express();

// ── Middleware ──────────────────────────────────────────────
app.use(express.json());

// Trust Railway's proxy so req.ip is correct
app.set('trust proxy', 1);

// ── Health check ────────────────────────────────────────────
app.get('/', (_req, res) => {
  res.json({
    status: 'ok',
    service: 'BabyNight',
    baby: config.baby.name,
    timezone: config.timezone,
    timestamp: new Date().toISOString(),
  });
});

// ── WhatsApp webhook ────────────────────────────────────────
// GET: Meta verification challenge
app.get('/webhook', handleWebhookVerification);

// POST: Incoming messages
app.post('/webhook', handleWebhookMessage);

// ── Start ───────────────────────────────────────────────────
async function start(): Promise<void> {
  // Initialize DB (creates tables if they don't exist)
  getDb();
  console.log('[DB] Database initialized');

  // Start scheduled check-ins
  scheduleCheckIns();

  // Start HTTP server
  app.listen(config.port, () => {
    console.log(`\n🍼 BabyNight is running!`);
    console.log(`   Port:     ${config.port}`);
    console.log(`   Baby:     ${config.baby.name}`);
    console.log(`   Timezone: ${config.timezone}`);
    console.log(`   Parents:  ${Object.values(config.parents).map(p => p.name).join(', ') || '(none configured)'}`);
    console.log(`\n   Webhook URL: POST /webhook`);
    console.log(`   Health:      GET  /\n`);
  });
}

start().catch(err => {
  console.error('Fatal startup error:', err);
  process.exit(1);
});
