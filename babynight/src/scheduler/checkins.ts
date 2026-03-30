import cron from 'node-cron';
import { config } from '../config';
import { generateCheckIn } from '../ai/brain';
import { broadcastToParents } from '../whatsapp/sender';
import { refreshDailySummary, saveSummaryText, getTodaySummary, getWeeklyTrends } from '../db/queries';
import { todayLocal } from '../utils/time';

// Convert HH:MM to a cron expression: "MM HH * * *"
function timeToCron(timeStr: string): string {
  const [hour, minute] = timeStr.split(':');
  return `${parseInt(minute, 10)} ${parseInt(hour, 10)} * * *`;
}

/** Register all scheduled check-ins */
export function scheduleCheckIns(): void {
  const timezone = config.timezone;

  // Register each check-in time
  for (const time of config.checkInTimes) {
    const cronExpr = timeToCron(time);

    cron.schedule(
      cronExpr,
      async () => {
        console.log(`[Scheduler] Running check-in: ${time}`);
        try {
          const message = await generateCheckIn(time);
          await broadcastToParents(message);
          console.log(`[Scheduler] Check-in sent for ${time}`);
        } catch (err) {
          const msg = err instanceof Error ? err.message : String(err);
          console.error(`[Scheduler] Check-in failed for ${time}: ${msg}`);
        }
      },
      { timezone }
    );

    console.log(`[Scheduler] Registered check-in at ${time} (${timezone})`);
  }

  // Daily summary at 23:00
  const summaryCron = timeToCron(config.dailySummaryTime);
  cron.schedule(
    summaryCron,
    async () => {
      console.log('[Scheduler] Generating daily summary...');
      try {
        const today = todayLocal();
        refreshDailySummary(today);

        const summary = getTodaySummary();
        const trends = getWeeklyTrends(7);

        // Ask Claude to write the narrative summary
        const { default: Anthropic } = await import('@anthropic-ai/sdk');
        const client = new Anthropic({ apiKey: config.anthropicApiKey });

        const { buildSystemPrompt } = await import('../ai/context');
        const systemPrompt = buildSystemPrompt();

        const response = await client.messages.create({
          model: config.model,
          max_tokens: 800,
          system: systemPrompt,
          messages: [
            {
              role: 'user',
              content: `Today's data: ${JSON.stringify(summary)}
Weekly context: ${JSON.stringify(trends.slice(0, 5))}

Write a warm end-of-day summary (3-4 sentences max) covering:
1. How today went overall
2. One thing that went well
3. One specific recommendation for tomorrow

Then on a new line write: RECOMMENDATIONS: [2-3 bullet points of specific action items for tomorrow]`,
            },
          ],
        });

        const text = response.content
          .filter(b => b.type === 'text')
          .map(b => (b as { type: 'text'; text: string }).text)
          .join('\n');

        // Parse out summary vs recommendations
        const [summaryPart, recPart] = text.split('RECOMMENDATIONS:');
        saveSummaryText(
          today,
          summaryPart?.trim() || text,
          recPart?.trim() || ''
        );

        const message = `🌙 *Daily Summary — ${today}*\n\n${text}`;
        await broadcastToParents(message);
        console.log('[Scheduler] Daily summary sent');
      } catch (err) {
        console.error('[Scheduler] Daily summary failed:', err);
      }
    },
    { timezone }
  );

  console.log(`[Scheduler] Registered daily summary at ${config.dailySummaryTime} (${timezone})`);
}

/**
 * Trigger a single check-in right now — useful for testing.
 * Usage: call triggerCheckIn('07:00') from your test script.
 */
export async function triggerCheckIn(time: string): Promise<void> {
  console.log(`[Scheduler] Manual trigger: ${time}`);
  const message = await generateCheckIn(time);
  await broadcastToParents(message);
  console.log(`[Scheduler] Manual check-in sent for ${time}`);
}
