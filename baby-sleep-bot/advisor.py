"""
Sleep advisor — uses Claude to interpret parent messages, log events,
and give attachment-theory-based sleep guidance for Aybe.
"""

import os
from datetime import datetime

import anthropic

import tracker

ANTHROPIC_API_KEY = os.environ.get("ANTHROPIC_API_KEY", "")

client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)

# ---------------------------------------------------------------------------
# System prompt
# ---------------------------------------------------------------------------

SYSTEM_PROMPT = """\
You are Lumi, a warm and knowledgeable sleep assistant for Aybe — a 5-month-old baby boy.

## Aybe's Situation
- Age: ~5 months (born ~October 2025)
- Feeding: Formula/bottle fed
- Main sleep association: Bottle feeding to sleep — this is the central habit to gently shift
- Current sleep challenges: Short naps (under 45 min), wakes 2-4× per night
- Goal: Help Aybe learn to fall asleep independently; eventually sleeping full nights without feeding

## Parents' Values
- Attachment-theory approach: responsive, gentle, attuned
- NEVER suggest "cry it out" (CIO), extinction, Ferber, or any method that involves leaving
  the baby to cry without response. These are off the table, full stop.
- Approved methods: Fading, Pick Up/Put Down (PUPD), Pantley Pull-Off, chair method

## Sleep Science — 5-Month-Olds
- Wake windows: 1.5–2 hours between sleep periods
- Naps per day: 3–4
- One sleep cycle ≈ 35–45 min. Shorter naps are developmentally normal but we can work on them
- Total sleep need: ~14–15 hours/day (≈10–11 h night + 4–5 h day)
- Ideal bedtime: 7:00–8:00 PM
- Night feeds: 1 feed per night may still be biologically appropriate at this age;
  goal is to gradually reduce unnecessary feeds, not cold-turkey eliminate them

## Methods to Reference
**Pantley Pull-Off** (for bottle association):
  Feed until drowsy → slowly slide the nipple out just before deep sleep
  → if baby stirs, briefly re-insert, wait a few seconds, remove again
  → repeat 3–5 times per sleep onset over 1–2 weeks until he learns to drift off unaided

**Fading** (the gentlest step-by-step):
  Week 1 — feed until drowsy but NOT fully asleep, then put down
  Week 2 — remove bottle when eyelids are heavy (not yet drooping)
  Week 3 — bottle ends a few minutes before put-down
  This takes time — celebrate micro-progress

**Pick Up/Put Down (PUPD)**:
  Put baby down drowsy but awake
  → crying? Pick up and soothe to CALM (not asleep), then put back down
  → repeat until he falls asleep in the cot
  → yes, it's tiring, but it IS fully attachment-friendly and it works

## How to Use the Sleep Log
When an event arrives, call log_sleep_event so it's stored.
Consider the log context when giving advice:
  - How long has Aybe been awake? → suggests readiness for sleep
  - Was the last nap short? → may be overtired or under-tired
  - How many night wakings last night? → trends matter more than one-off nights
  - Time of day? → respects circadian rhythm context

## Tone & Format (IMPORTANT)
- These are tired parents texting on a phone — keep replies SHORT
- 2–4 sentences max unless they explicitly ask for more detail
- Warm, encouraging, zero judgment
- Celebrate small wins ("That's progress!")
- Normalise setbacks ("Completely normal — this isn't linear")
- Use 1–2 emojis per message, not more
- If asked a general question, answer it directly and briefly
- If an event is logged, acknowledge it + give ONE actionable tip
"""

# ---------------------------------------------------------------------------
# Tool definition
# ---------------------------------------------------------------------------

TOOLS = [
    {
        "name": "log_sleep_event",
        "description": (
            "Log a sleep, feeding, or activity event for Aybe. "
            "Call this whenever the parent describes an event happening."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "event_type": {
                    "type": "string",
                    "enum": [
                        "nap_start",
                        "nap_end",
                        "night_wake",
                        "back_to_sleep",
                        "feeding",
                        "crying_start",
                        "crying_end",
                        "bedtime",
                        "wake_for_day",
                        "note",
                    ],
                    "description": "The type of event",
                },
                "notes": {
                    "type": "string",
                    "description": "Any relevant detail (e.g. 'used Pantley pull-off', 'gave 3oz bottle')",
                },
                "duration_minutes": {
                    "type": "integer",
                    "description": "Duration in minutes, if the event has a known duration (e.g. nap length)",
                },
            },
            "required": ["event_type"],
        },
    }
]


# ---------------------------------------------------------------------------
# Core message processing
# ---------------------------------------------------------------------------


def process_message(text: str, sender_name: str = "parent") -> str:
    """
    Process one incoming WhatsApp message.
    Returns the bot's reply string.
    """
    tracker.log_message(sender=sender_name, content=text, is_bot=False)

    system_with_context = (
        SYSTEM_PROMPT
        + f"\n\n## Current Sleep Log\n{tracker.format_event_log(hours=12)}"
        + f"\n\nCurrent time: {datetime.now().strftime('%A %d %b, %I:%M %p')}"
    )

    messages = [{"role": "user", "content": f"[{sender_name}]: {text}"}]

    response = client.messages.create(
        model="claude-haiku-4-5",
        max_tokens=400,
        system=system_with_context,
        tools=TOOLS,
        messages=messages,
    )

    # Agentic loop — Claude may call log_sleep_event before replying
    while response.stop_reason == "tool_use":
        tool_uses = [b for b in response.content if b.type == "tool_use"]
        tool_results = []

        for tu in tool_uses:
            if tu.name == "log_sleep_event":
                inp = tu.input
                tracker.log_event(
                    event_type=inp["event_type"],
                    notes=inp.get("notes"),
                    duration_minutes=inp.get("duration_minutes"),
                    reporter=sender_name,
                )
                tool_results.append({
                    "type": "tool_result",
                    "tool_use_id": tu.id,
                    "content": f"Logged: {inp['event_type']}",
                })

        messages = [
            {"role": "user", "content": f"[{sender_name}]: {text}"},
            {"role": "assistant", "content": response.content},
            {"role": "user", "content": tool_results},
        ]

        response = client.messages.create(
            model="claude-haiku-4-5",
            max_tokens=400,
            system=system_with_context,
            tools=TOOLS,
            messages=messages,
        )

    reply = next(
        (b.text for b in response.content if b.type == "text"),
        "Got it! 👍",
    )

    tracker.log_message(sender="Lumi", content=reply, is_bot=True)
    return reply
