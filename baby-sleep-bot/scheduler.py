"""
Proactive scheduler — sends wake-window alerts, bedtime reminders,
and morning summaries to both parents via Twilio WhatsApp.
"""

import os
from datetime import datetime

from twilio.rest import Client

import tracker

twilio_client = Client(
    os.environ.get("TWILIO_ACCOUNT_SID"),
    os.environ.get("TWILIO_AUTH_TOKEN"),
)

TWILIO_FROM = os.environ.get("TWILIO_WHATSAPP_NUMBER", "")   # whatsapp:+14155238886
PARENT_NUMBERS = [
    n.strip()
    for n in os.environ.get("PARENT_WHATSAPP_NUMBERS", "").split(",")
    if n.strip()
]  # comma-separated, e.g. "whatsapp:+1234567890,whatsapp:+0987654321"

# Track which alerts we've already sent this cycle to avoid spam
_sent_flags: dict = {}


def _flag_key(name: str) -> str:
    """Key for dedup flag — resets each calendar day."""
    return f"{name}:{datetime.now().strftime('%Y-%m-%d')}"


def _already_sent(name: str) -> bool:
    return _sent_flags.get(_flag_key(name), False)


def _mark_sent(name: str):
    _sent_flags[_flag_key(name)] = True


def broadcast(message: str):
    """Send a message to all configured parent numbers."""
    for number in PARENT_NUMBERS:
        try:
            twilio_client.messages.create(
                body=message,
                from_=TWILIO_FROM,
                to=number,
            )
        except Exception as exc:
            print(f"[scheduler] Failed to send to {number}: {exc}")


# ---------------------------------------------------------------------------
# Individual check functions (called every few minutes by APScheduler)
# ---------------------------------------------------------------------------


def check_wake_window():
    """Alert parents when Aybe's wake window is approaching or exceeded."""
    last_wake = tracker.get_last_wake_time()
    if not last_wake:
        return

    awake_min = tracker.minutes_since_last_wake()
    if awake_min is None:
        return

    nap_pending = not tracker.nap_started_since(last_wake["timestamp"])

    if nap_pending and 88 <= awake_min <= 98:
        key = f"wake_window_90:{last_wake['timestamp'][:13]}"
        if not _already_sent(key):
            broadcast(
                "⏰ *Nap window!* Aybe has been awake ~90 min.\n"
                "Good time to start winding down — watch for sleepy cues: "
                "rubbing eyes, going quiet, zoning out."
            )
            _mark_sent(key)

    elif nap_pending and 108 <= awake_min <= 118:
        key = f"wake_window_110:{last_wake['timestamp'][:13]}"
        if not _already_sent(key):
            broadcast(
                "😴 *Overtired alert!* Aybe has been awake nearly 2 hours.\n"
                "Overtired babies fight sleep harder — try starting the nap now. "
                "Dark room + white noise helps."
            )
            _mark_sent(key)


def check_bedtime():
    """Remind parents about the 7 PM bedtime window."""
    now = datetime.now()
    if now.hour != 19 or now.minute > 15:
        return

    key = _flag_key("bedtime_reminder")
    if _already_sent(key):
        return

    recent = tracker.get_recent_events(hours=1)
    if any(e["event_type"] == "bedtime" for e in recent):
        return  # already in bed

    broadcast(
        "🌙 *Bedtime window!* It's 7 PM — perfect time for Aybe's bedtime routine.\n"
        "Dim the lights, do a calm feed (try Pantley pull-off!), "
        "then put him down drowsy but awake."
    )
    _mark_sent(key)


def morning_summary():
    """At 7 AM send a summary of the previous night."""
    now = datetime.now()
    if now.hour != 7 or now.minute > 15:
        return

    key = _flag_key("morning_summary")
    if _already_sent(key):
        return

    night_events = tracker.get_recent_events(hours=10)
    wakings = [e for e in night_events if e["event_type"] == "night_wake"]
    feedings = [e for e in night_events if e["event_type"] == "feeding"]

    n_wake = len(wakings)
    n_feed = len(feedings)

    msg = "☀️ *Good morning! Last night:*\n"
    msg += f"  🌙 Night wakings: {n_wake}\n"
    msg += f"  🍼 Feedings: {n_feed}\n"

    if n_wake == 0:
        msg += "\n🌟 Incredible — Aybe slept through! Big win!"
    elif n_wake <= 2:
        msg += "\n✨ That's improvement! Keep up the gentle work."
    else:
        msg += "\n💪 Tough night — you're doing great. Progress isn't always linear, and that's okay."

    broadcast(msg)
    _mark_sent(key)


# ---------------------------------------------------------------------------
# Scheduler setup
# ---------------------------------------------------------------------------


def setup_scheduler():
    from apscheduler.schedulers.background import BackgroundScheduler

    tz = os.environ.get("TZ", "UTC")
    scheduler = BackgroundScheduler(timezone=tz)
    scheduler.add_job(check_wake_window, "interval", minutes=5)
    scheduler.add_job(check_bedtime, "interval", minutes=10)
    scheduler.add_job(morning_summary, "interval", minutes=10)
    scheduler.start()
    print("[scheduler] Started — wake window, bedtime, and morning checks active.")
    return scheduler
