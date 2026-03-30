import { getDb } from './schema';
import { todayLocal, startOfDay, endOfDay, lastNDates, nowISO } from '../utils/time';

// ─────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────

export interface BabyEvent {
  id?: number;
  timestamp: string;
  type: string;
  value_ml?: number | null;
  duration_min?: number | null;
  wake_reason?: string | null;
  mood?: string | null;
  notes?: string | null;
  reported_by: string;
  raw_message?: string | null;
  wa_message_id?: string | null;
}

export interface DailySummary {
  date: string;
  total_feed_ml: number;
  feed_count: number;
  total_nap_min: number;
  nap_count: number;
  night_sleep_start?: string | null;
  night_wake_count: number;
  longest_stretch_min: number;
  night_feeds_ml: number;
  summary_text?: string | null;
  recommendations?: string | null;
}

export interface ConversationMessage {
  role: 'user' | 'assistant';
  content: string;
}

// ─────────────────────────────────────────────────────────────
// Events
// ─────────────────────────────────────────────────────────────

/** Log a single baby event. Returns the inserted row id. */
export function logEvent(event: BabyEvent): number {
  const db = getDb();
  const stmt = db.prepare(`
    INSERT INTO events
      (timestamp, type, value_ml, duration_min, wake_reason, mood, notes, reported_by, raw_message, wa_message_id)
    VALUES
      (@timestamp, @type, @value_ml, @duration_min, @wake_reason, @mood, @notes, @reported_by, @raw_message, @wa_message_id)
  `);
  const result = stmt.run({
    timestamp: event.timestamp || nowISO(),
    type: event.type,
    value_ml: event.value_ml ?? null,
    duration_min: event.duration_min ?? null,
    wake_reason: event.wake_reason ?? null,
    mood: event.mood ?? null,
    notes: event.notes ?? null,
    reported_by: event.reported_by,
    raw_message: event.raw_message ?? null,
    wa_message_id: event.wa_message_id ?? null,
  });
  // After logging, refresh the daily summary
  refreshDailySummary(todayLocal());
  return result.lastInsertRowid as number;
}

/** Check if a WhatsApp message ID was already processed (deduplication) */
export function isMessageProcessed(waMessageId: string): boolean {
  const db = getDb();
  const row = db.prepare('SELECT id FROM events WHERE wa_message_id = ?').get(waMessageId);
  return !!row;
}

/** Get events for a date range, optionally filtered by type */
export function getEvents(opts: {
  dateFrom?: string;  // YYYY-MM-DD
  dateTo?: string;    // YYYY-MM-DD
  type?: string;
  lastN?: number;
}): BabyEvent[] {
  const db = getDb();

  let query = 'SELECT * FROM events WHERE 1=1';
  const params: (string | number)[] = [];

  if (opts.dateFrom) {
    query += ' AND timestamp >= ?';
    params.push(startOfDay(opts.dateFrom));
  }
  if (opts.dateTo) {
    query += ' AND timestamp <= ?';
    params.push(endOfDay(opts.dateTo));
  }
  if (opts.type) {
    query += ' AND type = ?';
    params.push(opts.type);
  }

  query += ' ORDER BY timestamp DESC';

  if (opts.lastN) {
    query += ' LIMIT ?';
    params.push(opts.lastN);
  }

  return db.prepare(query).all(...params) as BabyEvent[];
}

/** Get today's events */
export function getTodayEvents(): BabyEvent[] {
  return getEvents({ dateFrom: todayLocal(), dateTo: todayLocal() });
}

// ─────────────────────────────────────────────────────────────
// Daily Summaries
// ─────────────────────────────────────────────────────────────

/** Recompute and upsert the daily summary for a given date */
export function refreshDailySummary(date: string): void {
  const db = getDb();
  const events = getEvents({ dateFrom: date, dateTo: date });

  const feeds = events.filter(e => e.type === 'feed');
  const total_feed_ml = feeds.reduce((sum, e) => sum + (e.value_ml || 0), 0);
  const feed_count = feeds.length;

  const napStarts = events.filter(e => e.type === 'nap_start');
  const napEnds = events.filter(e => e.type === 'nap_end');

  // Simple pairing: match nap_start with the next nap_end
  let total_nap_min = 0;
  let nap_count = 0;
  for (const start of napStarts) {
    const end = napEnds.find(e => e.timestamp > start.timestamp);
    if (end) {
      const dur = (new Date(end.timestamp).getTime() - new Date(start.timestamp).getTime()) / 60000;
      total_nap_min += Math.round(dur);
      nap_count++;
    } else if (start.duration_min) {
      total_nap_min += start.duration_min;
      nap_count++;
    }
  }

  const nightSleepStart = events.find(e => e.type === 'night_sleep_start');
  const nightWakes = events.filter(e => e.type === 'night_wake');
  const night_wake_count = nightWakes.length;
  const nightFeeds = nightWakes.filter(e => (e.value_ml || 0) > 0);
  const night_feeds_ml = nightFeeds.reduce((sum, e) => sum + (e.value_ml || 0), 0);

  // Longest stretch = time between consecutive night events (or until morning)
  let longest_stretch_min = 0;
  const nightEvents = events
    .filter(e => ['night_sleep_start', 'night_wake', 'night_sleep_end'].includes(e.type))
    .sort((a, b) => a.timestamp.localeCompare(b.timestamp));
  for (let i = 0; i < nightEvents.length - 1; i++) {
    const stretch =
      (new Date(nightEvents[i + 1].timestamp).getTime() -
        new Date(nightEvents[i].timestamp).getTime()) /
      60000;
    if (stretch > longest_stretch_min) longest_stretch_min = Math.round(stretch);
  }

  db.prepare(`
    INSERT INTO daily_summaries
      (date, total_feed_ml, feed_count, total_nap_min, nap_count,
       night_sleep_start, night_wake_count, longest_stretch_min, night_feeds_ml, updated_at)
    VALUES
      (@date, @total_feed_ml, @feed_count, @total_nap_min, @nap_count,
       @night_sleep_start, @night_wake_count, @longest_stretch_min, @night_feeds_ml, datetime('now'))
    ON CONFLICT(date) DO UPDATE SET
      total_feed_ml = excluded.total_feed_ml,
      feed_count = excluded.feed_count,
      total_nap_min = excluded.total_nap_min,
      nap_count = excluded.nap_count,
      night_sleep_start = excluded.night_sleep_start,
      night_wake_count = excluded.night_wake_count,
      longest_stretch_min = excluded.longest_stretch_min,
      night_feeds_ml = excluded.night_feeds_ml,
      updated_at = datetime('now')
  `).run({
    date,
    total_feed_ml,
    feed_count,
    total_nap_min,
    nap_count,
    night_sleep_start: nightSleepStart?.timestamp ?? null,
    night_wake_count,
    longest_stretch_min,
    night_feeds_ml,
  });
}

/** Get today's running summary */
export function getTodaySummary(): DailySummary | null {
  refreshDailySummary(todayLocal());
  const db = getDb();
  return db.prepare('SELECT * FROM daily_summaries WHERE date = ?').get(todayLocal()) as DailySummary | null;
}

/** Get the last N days of daily summaries */
export function getWeeklyTrends(nDays = 7): DailySummary[] {
  const dates = lastNDates(nDays);
  const db = getDb();
  return dates
    .map(d => db.prepare('SELECT * FROM daily_summaries WHERE date = ?').get(d) as DailySummary | null)
    .filter((s): s is DailySummary => s !== null);
}

/** Save a generated summary text + recommendations to the daily summary row */
export function saveSummaryText(date: string, summaryText: string, recommendations: string): void {
  const db = getDb();
  db.prepare(`
    UPDATE daily_summaries
    SET summary_text = ?, recommendations = ?, updated_at = datetime('now')
    WHERE date = ?
  `).run(summaryText, recommendations, date);
}

// ─────────────────────────────────────────────────────────────
// Conversation Context
// ─────────────────────────────────────────────────────────────

/** Save a message to conversation history */
export function saveConversationMessage(phone: string, role: 'user' | 'assistant', content: string): void {
  const db = getDb();
  db.prepare(
    'INSERT INTO conversation_context (phone, role, content) VALUES (?, ?, ?)'
  ).run(phone, role, content);

  // Keep only the last 20 messages per phone to prevent unbounded growth
  db.prepare(`
    DELETE FROM conversation_context
    WHERE phone = ?
      AND id NOT IN (
        SELECT id FROM conversation_context WHERE phone = ? ORDER BY id DESC LIMIT 20
      )
  `).run(phone, phone);
}

/** Get the last N messages for a phone number (oldest first) */
export function getConversationHistory(phone: string, lastN = 10): ConversationMessage[] {
  const db = getDb();
  const rows = db.prepare(`
    SELECT role, content FROM conversation_context
    WHERE phone = ?
    ORDER BY id DESC
    LIMIT ?
  `).all(phone, lastN) as ConversationMessage[];
  return rows.reverse();
}

// ─────────────────────────────────────────────────────────────
// CSV Export
// ─────────────────────────────────────────────────────────────

/** Export all events as a CSV string */
export function exportEventsCSV(): string {
  const db = getDb();
  const events = db.prepare('SELECT * FROM events ORDER BY timestamp ASC').all() as BabyEvent[];

  if (events.length === 0) return 'No events recorded yet.';

  const headers = ['id', 'timestamp', 'type', 'value_ml', 'duration_min', 'wake_reason', 'mood', 'notes', 'reported_by'];
  const rows = events.map(e =>
    headers.map(h => {
      const val = (e as unknown as Record<string, unknown>)[h];
      if (val === null || val === undefined) return '';
      const str = String(val);
      // Escape CSV: wrap in quotes if contains comma/newline/quote
      if (str.includes(',') || str.includes('\n') || str.includes('"')) {
        return `"${str.replace(/"/g, '""')}"`;
      }
      return str;
    }).join(',')
  );

  return [headers.join(','), ...rows].join('\n');
}
