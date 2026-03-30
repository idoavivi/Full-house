import dotenv from 'dotenv';
dotenv.config();

function requireEnv(name: string): string {
  const val = process.env[name];
  if (!val) throw new Error(`Missing required environment variable: ${name}`);
  return val;
}

// Parents are loaded from env vars so phone numbers stay out of code
const parents: Record<string, { name: string; role: string }> = {};

if (process.env.PARENT1_PHONE) {
  parents[process.env.PARENT1_PHONE] = {
    name: process.env.PARENT1_NAME || 'Parent 1',
    role: 'parent1',
  };
}
if (process.env.PARENT2_PHONE) {
  parents[process.env.PARENT2_PHONE] = {
    name: process.env.PARENT2_NAME || 'Parent 2',
    role: 'parent2',
  };
}

export const config = {
  // WhatsApp Cloud API
  whatsappPhoneNumberId: process.env.WA_PHONE_NUMBER_ID || '',
  whatsappAccessToken: process.env.WA_ACCESS_TOKEN || '',
  whatsappVerifyToken: process.env.WA_VERIFY_TOKEN || 'babynight_verify',

  // Authorised parents: phone number → info
  parents,

  baby: {
    name: process.env.BABY_NAME || 'the baby',
    birthDate: process.env.BABY_BIRTH_DATE || '2025-10-15',
    feedType: 'formula' as const,
  },

  // Anthropic / Claude
  anthropicApiKey: process.env.ANTHROPIC_API_KEY || '',
  model: 'claude-sonnet-4-20250514',

  // Timing
  timezone: 'Asia/Jerusalem',
  checkInTimes: ['07:00', '09:30', '12:00', '15:00', '18:30', '20:30'],
  dailySummaryTime: '23:00',

  // Server
  port: parseInt(process.env.PORT || '3000', 10),

  // Database path (Railway mounts /data as persistent volume; fall back to local)
  dbPath: process.env.DB_PATH || './babynight.db',
};

/** Returns baby's current age in days / weeks / months as a readable string */
export function getBabyAgeDescription(): string {
  const birth = new Date(config.baby.birthDate);
  const now = new Date();
  const diffMs = now.getTime() - birth.getTime();
  const diffDays = Math.floor(diffMs / (1000 * 60 * 60 * 24));
  const weeks = Math.floor(diffDays / 7);
  const months = Math.floor(diffDays / 30.44);

  if (months >= 2) return `${months} months old (${diffDays} days)`;
  if (weeks >= 4) return `${weeks} weeks old (${diffDays} days)`;
  return `${diffDays} days old`;
}
