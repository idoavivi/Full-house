import Anthropic from '@anthropic-ai/sdk';
import {
  logEvent,
  getTodaySummary,
  getWeeklyTrends,
  getEvents,
  type BabyEvent,
} from '../db/queries';
import { nowISO, todayLocal } from '../utils/time';

// ─────────────────────────────────────────────────────────────
// Tool definitions (sent to Claude)
// ─────────────────────────────────────────────────────────────

export const toolDefinitions: Anthropic.Tool[] = [
  {
    name: 'log_event',
    description:
      'Log a baby event (feed, nap, night sleep, wake-up, mood, or free note). ' +
      'Call this once per event. If the parent describes multiple events in one message, call it multiple times.',
    input_schema: {
      type: 'object' as const,
      properties: {
        type: {
          type: 'string',
          enum: ['feed', 'nap_start', 'nap_end', 'night_sleep_start', 'night_wake', 'night_sleep_end', 'mood_check', 'note'],
          description: 'The type of event.',
        },
        timestamp: {
          type: 'string',
          description: 'ISO 8601 timestamp. If the parent mentions a time ("at 3am", "just now"), use that. Otherwise use current time.',
        },
        value_ml: {
          type: 'number',
          description: 'Milk amount in ml. Only for feed or night_wake events involving feeding.',
        },
        duration_min: {
          type: 'number',
          description: 'Duration in minutes. For nap_end: how long the nap was. For night_sleep_end: total night sleep.',
        },
        wake_reason: {
          type: 'string',
          description: 'Why the baby woke up. One of: hunger, discomfort, habit, unknown — or free text.',
        },
        mood: {
          type: 'string',
          enum: ['calm', 'fussy', 'crying', 'happy'],
          description: 'Baby\'s mood, if mentioned.',
        },
        notes: {
          type: 'string',
          description: 'Any additional context the parent mentioned.',
        },
      },
      required: ['type'],
    },
  },
  {
    name: 'get_today_summary',
    description:
      'Get a summary of today\'s events: total feeds (ml and count), naps (minutes and count), night wakes, etc. ' +
      'Use this when the parent asks "how much has he eaten today?" or similar.',
    input_schema: {
      type: 'object' as const,
      properties: {},
      required: [],
    },
  },
  {
    name: 'get_weekly_trends',
    description:
      'Get the last 7 days of daily summaries to identify patterns in sleep, feeding, and activity. ' +
      'Use when parent asks about trends or week-level insights.',
    input_schema: {
      type: 'object' as const,
      properties: {
        days: {
          type: 'number',
          description: 'Number of days to look back. Default 7.',
        },
      },
      required: [],
    },
  },
  {
    name: 'get_events',
    description:
      'Query raw events with optional filters. Use for specific questions like "when was the last feed?" or "list today\'s naps".',
    input_schema: {
      type: 'object' as const,
      properties: {
        date_from: {
          type: 'string',
          description: 'Start date YYYY-MM-DD. Defaults to today.',
        },
        date_to: {
          type: 'string',
          description: 'End date YYYY-MM-DD. Defaults to today.',
        },
        type: {
          type: 'string',
          description: 'Filter by event type.',
        },
        last_n: {
          type: 'number',
          description: 'Return only the last N events.',
        },
      },
      required: [],
    },
  },
];

// ─────────────────────────────────────────────────────────────
// Tool executor
// ─────────────────────────────────────────────────────────────

export interface ToolCallResult {
  type: 'tool_result';
  tool_use_id: string;
  content: string;
}

export async function executeTool(
  toolName: string,
  toolInput: Record<string, unknown>,
  reportedBy: string,
  rawMessage: string,
  waMessageId?: string
): Promise<string> {
  try {
    switch (toolName) {
      case 'log_event': {
        const event: BabyEvent = {
          type: toolInput.type as string,
          timestamp: (toolInput.timestamp as string) || nowISO(),
          value_ml: (toolInput.value_ml as number) ?? null,
          duration_min: (toolInput.duration_min as number) ?? null,
          wake_reason: (toolInput.wake_reason as string) ?? null,
          mood: (toolInput.mood as string) ?? null,
          notes: (toolInput.notes as string) ?? null,
          reported_by: reportedBy,
          raw_message: rawMessage,
          wa_message_id: waMessageId,
        };
        const id = logEvent(event);
        return JSON.stringify({ success: true, event_id: id, logged: event });
      }

      case 'get_today_summary': {
        const summary = getTodaySummary();
        if (!summary) return JSON.stringify({ message: 'No data recorded today yet.' });
        return JSON.stringify(summary);
      }

      case 'get_weekly_trends': {
        const days = (toolInput.days as number) || 7;
        const trends = getWeeklyTrends(days);
        if (trends.length === 0) return JSON.stringify({ message: 'No historical data available yet.' });
        return JSON.stringify(trends);
      }

      case 'get_events': {
        const today = todayLocal();
        const events = getEvents({
          dateFrom: (toolInput.date_from as string) || today,
          dateTo: (toolInput.date_to as string) || today,
          type: (toolInput.type as string) || undefined,
          lastN: (toolInput.last_n as number) || undefined,
        });
        return JSON.stringify({ count: events.length, events });
      }

      default:
        return JSON.stringify({ error: `Unknown tool: ${toolName}` });
    }
  } catch (err: unknown) {
    const msg = err instanceof Error ? err.message : String(err);
    console.error(`[Tool] Error executing ${toolName}:`, msg);
    return JSON.stringify({ error: msg });
  }
}
