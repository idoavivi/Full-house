"""
Sleep event tracker — persists all events and messages in SQLite.
"""

import sqlite3
from datetime import datetime, timedelta
from pathlib import Path

DB_PATH = Path("sleep_data.db")


def get_db():
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn


def init_db():
    with get_db() as conn:
        conn.execute("""
            CREATE TABLE IF NOT EXISTS events (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp TEXT NOT NULL,
                event_type TEXT NOT NULL,
                notes TEXT,
                duration_minutes INTEGER,
                reporter TEXT
            )
        """)
        conn.execute("""
            CREATE TABLE IF NOT EXISTS messages (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp TEXT NOT NULL,
                sender TEXT NOT NULL,
                content TEXT NOT NULL,
                is_bot INTEGER NOT NULL DEFAULT 0
            )
        """)
        conn.commit()


def log_event(event_type, notes=None, duration_minutes=None, reporter=None):
    with get_db() as conn:
        conn.execute(
            "INSERT INTO events (timestamp, event_type, notes, duration_minutes, reporter) "
            "VALUES (?, ?, ?, ?, ?)",
            (datetime.now().isoformat(), event_type, notes, duration_minutes, reporter),
        )
        conn.commit()


def log_message(sender, content, is_bot=False):
    with get_db() as conn:
        conn.execute(
            "INSERT INTO messages (timestamp, sender, content, is_bot) VALUES (?, ?, ?, ?)",
            (datetime.now().isoformat(), sender, content, int(is_bot)),
        )
        conn.commit()


def get_recent_events(hours=12):
    since = (datetime.now() - timedelta(hours=hours)).isoformat()
    with get_db() as conn:
        rows = conn.execute(
            "SELECT * FROM events WHERE timestamp > ? ORDER BY timestamp ASC",
            (since,),
        ).fetchall()
    return [dict(r) for r in rows]


def get_last_event_of_types(event_types: list):
    """Return the most recent event matching any of the given types."""
    placeholders = ",".join("?" * len(event_types))
    with get_db() as conn:
        row = conn.execute(
            f"SELECT * FROM events WHERE event_type IN ({placeholders}) "
            "ORDER BY timestamp DESC LIMIT 1",
            event_types,
        ).fetchone()
    return dict(row) if row else None


def get_last_wake_time():
    """Return the most recent wake event (wake_for_day, nap_end, or back_to_sleep)."""
    return get_last_event_of_types(["wake_for_day", "nap_end", "back_to_sleep"])


def minutes_since_last_wake():
    last = get_last_wake_time()
    if not last:
        return None
    delta = datetime.now() - datetime.fromisoformat(last["timestamp"])
    return delta.total_seconds() / 60


def nap_started_since(since_iso: str):
    """Check whether a nap_start event has been logged after the given timestamp."""
    with get_db() as conn:
        row = conn.execute(
            "SELECT id FROM events WHERE event_type = 'nap_start' AND timestamp > ? LIMIT 1",
            (since_iso,),
        ).fetchone()
    return row is not None


def format_event_log(hours=12):
    """Return a human-readable string of recent events for Claude's context."""
    events = get_recent_events(hours=hours)
    if not events:
        return "No events logged in the last 12 hours."

    lines = [f"Sleep log — last {hours}h:"]
    for e in events:
        ts = e["timestamp"][:16].replace("T", " ")
        line = f"  [{ts}] {e['event_type']}"
        if e.get("duration_minutes"):
            line += f" ({e['duration_minutes']} min)"
        if e.get("notes"):
            line += f" — {e['notes']}"
        if e.get("reporter"):
            line += f" (by {e['reporter']})"
        lines.append(line)

    return "\n".join(lines)
