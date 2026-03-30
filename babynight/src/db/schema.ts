import Database from 'better-sqlite3';
import { config } from '../config';

let _db: Database.Database | null = null;

export function getDb(): Database.Database {
  if (!_db) {
    _db = new Database(config.dbPath);
    _db.pragma('journal_mode = WAL');
    _db.pragma('foreign_keys = ON');
    initSchema(_db);
  }
  return _db;
}

function initSchema(db: Database.Database): void {
  db.exec(`
    CREATE TABLE IF NOT EXISTS events (
      id               INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp        TEXT NOT NULL DEFAULT (datetime('now')),
      type             TEXT NOT NULL CHECK(type IN (
                         'feed','nap_start','nap_end',
                         'night_sleep_start','night_wake','night_sleep_end',
                         'mood_check','note'
                       )),
      value_ml         INTEGER,
      duration_min     INTEGER,
      wake_reason      TEXT,
      mood             TEXT,
      notes            TEXT,
      reported_by      TEXT NOT NULL DEFAULT 'unknown',
      raw_message      TEXT,
      wa_message_id    TEXT UNIQUE
    );

    CREATE INDEX IF NOT EXISTS idx_events_timestamp ON events(timestamp);
    CREATE INDEX IF NOT EXISTS idx_events_type      ON events(type);

    -- ----------------------------------------------------------------
    CREATE TABLE IF NOT EXISTS daily_summaries (
      date                  TEXT PRIMARY KEY,
      total_feed_ml         INTEGER DEFAULT 0,
      feed_count            INTEGER DEFAULT 0,
      total_nap_min         INTEGER DEFAULT 0,
      nap_count             INTEGER DEFAULT 0,
      night_sleep_start     TEXT,
      night_wake_count      INTEGER DEFAULT 0,
      longest_stretch_min   INTEGER DEFAULT 0,
      night_feeds_ml        INTEGER DEFAULT 0,
      summary_text          TEXT,
      recommendations       TEXT,
      created_at            TEXT DEFAULT (datetime('now')),
      updated_at            TEXT DEFAULT (datetime('now'))
    );

    -- ----------------------------------------------------------------
    CREATE TABLE IF NOT EXISTS conversation_context (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      phone       TEXT NOT NULL,
      role        TEXT NOT NULL CHECK(role IN ('user','assistant')),
      content     TEXT NOT NULL,
      timestamp   TEXT NOT NULL DEFAULT (datetime('now'))
    );

    CREATE INDEX IF NOT EXISTS idx_ctx_phone ON conversation_context(phone);
  `);
}
