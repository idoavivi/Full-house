"""
Main Flask app — receives WhatsApp messages via Twilio webhook,
processes them through the sleep advisor, and returns a reply.
"""

import os
import re

from flask import Flask, request
from twilio.twiml.messaging_response import MessagingResponse

import tracker
import advisor
import scheduler

app = Flask(__name__)


def extract_sender_name(from_number: str) -> str:
    """
    Try to map the sender's WhatsApp number to a name using env vars.
    Falls back to the number itself (last 4 digits).
    Expects PARENT_NAMES env var like: "+1234567890=Mum,+0987654321=Dad"
    """
    name_map_raw = os.environ.get("PARENT_NAMES", "")
    name_map = {}
    for entry in name_map_raw.split(","):
        parts = entry.strip().split("=")
        if len(parts) == 2:
            number = re.sub(r"\D", "", parts[0].strip())
            name_map[number] = parts[1].strip()

    digits = re.sub(r"\D", "", from_number)
    # Try full number or last 10 digits
    return name_map.get(digits) or name_map.get(digits[-10:]) or f"Parent ({digits[-4:]})"


@app.route("/webhook", methods=["POST"])
def webhook():
    body = request.values.get("Body", "").strip()
    from_number = request.values.get("From", "unknown")

    if not body:
        resp = MessagingResponse()
        resp.message("👋 Hey! Just send me what's happening with Aybe and I'll help.")
        return str(resp)

    sender_name = extract_sender_name(from_number)
    reply = advisor.process_message(body, sender_name=sender_name)

    resp = MessagingResponse()
    resp.message(reply)
    return str(resp)


@app.route("/health", methods=["GET"])
def health():
    return {"status": "ok", "baby": "Aybe 🍼"}, 200


@app.route("/log", methods=["GET"])
def log_view():
    """Quick read-only view of recent events (no auth — keep this on a private URL if needed)."""
    events = tracker.get_recent_events(hours=24)
    lines = []
    for e in events:
        ts = e["timestamp"][:16].replace("T", " ")
        line = f"[{ts}] {e['event_type']}"
        if e.get("duration_minutes"):
            line += f" ({e['duration_minutes']} min)"
        if e.get("notes"):
            line += f" — {e['notes']}"
        if e.get("reporter"):
            line += f" ({e['reporter']})"
        lines.append(line)
    return "<pre>" + "\n".join(lines) + "</pre>"


if __name__ == "__main__":
    tracker.init_db()
    scheduler.setup_scheduler()
    port = int(os.environ.get("PORT", 5000))
    app.run(host="0.0.0.0", port=port, debug=False)
