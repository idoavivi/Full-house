# Aybe's Sleep Bot — Setup Guide

A WhatsApp bot that helps you and your partner log Aybe's sleep, feedings, and wakings — and gives you gentle, attachment-theory-based guidance to help him sleep longer.

---

## What the bot does

- **You log events** by texting naturally:
  - `"nap started"` / `"he fell asleep"`
  - `"woke up from nap, 40 min"`
  - `"gave him a bottle"`
  - `"he's been crying 5 min"`
  - `"night waking"` / `"he's up again"`
  - `"woke for the day"`
  - `"going to bed now"`

- **Bot responds** with a confirmation + one actionable tip tailored to the situation

- **Proactive alerts** sent automatically:
  - ⏰ Wake window reminder at 90 min (nap time coming up)
  - 😴 Overtired alert at 110 min (act now!)
  - 🌙 Bedtime reminder at 7 PM
  - ☀️ Morning summary of the night's wakings

---

## Setup — Step by Step

### Step 1: Get a Twilio account

1. Go to [twilio.com](https://www.twilio.com) → Sign up (free)
2. In the Twilio Console, go to **Messaging → Try it out → Send a WhatsApp message**
3. You'll get a sandbox number (looks like `+1 415 523 8886`)
4. Both you and your partner need to activate the sandbox:
   - Save the Twilio sandbox number in your contacts as "Aybe Bot"
   - Send the message **"join [your-sandbox-code]"** to that number on WhatsApp
   - Do this from BOTH phones
5. Note down:
   - **Account SID** (from the Console dashboard)
   - **Auth Token** (from the Console dashboard)

> **Note on "group chat":** WhatsApp Business groups require Meta business verification.
> The sandbox works as individual conversations — both parents text the same bot number
> and both receive proactive alerts. It works exactly like a shared assistant.

---

### Step 2: Get an Anthropic API key

1. Go to [console.anthropic.com](https://console.anthropic.com)
2. Create an account → go to **API Keys** → **Create Key**
3. Copy the key (starts with `sk-ant-`)

> **Cost estimate:** The bot uses Claude Haiku (~$1/million tokens). With ~50 messages/day,
> expect **under $1/month** in API costs.

---

### Step 3: Deploy to Railway (free, no server needed)

1. Go to [railway.app](https://railway.app) → Sign up with GitHub
2. Click **New Project** → **Deploy from GitHub repo**
3. Select `idoavivi/full-house`
4. When asked for the root directory, enter: `baby-sleep-bot`
5. Railway will detect the Python app and deploy it automatically
6. Once deployed, click your service → **Settings** → note the public URL
   (looks like `https://your-app-name.up.railway.app`)

---

### Step 4: Set environment variables in Railway

In Railway → your service → **Variables**, add these:

| Variable | Value |
|----------|-------|
| `ANTHROPIC_API_KEY` | Your Anthropic key (`sk-ant-...`) |
| `TWILIO_ACCOUNT_SID` | From Twilio Console |
| `TWILIO_AUTH_TOKEN` | From Twilio Console |
| `TWILIO_WHATSAPP_NUMBER` | `whatsapp:+14155238886` (sandbox number) |
| `PARENT_WHATSAPP_NUMBERS` | `whatsapp:+YOURNUMBER,whatsapp:+PARTNERNUMBER` |
| `PARENT_NAMES` | `+YOURNUMBER=Mum,+PARTNERNUMBER=Dad` (optional) |

> Replace `YOURNUMBER` with your full number including country code, e.g. `+972501234567`

---

### Step 5: Connect Twilio webhook

1. In Twilio Console → **Messaging → Settings → WhatsApp Sandbox Settings**
2. Set **"When a message comes in"** to:
   ```
   https://your-app-name.up.railway.app/webhook
   ```
3. Method: `HTTP POST`
4. Save

---

### Step 6: Test it!

Send these messages to the bot number from WhatsApp:
- `"Aybe woke up"` → should log wake and reply with next nap timing
- `"nap started"` → should confirm and encourage rest
- `"fed him 4oz"` → should log feeding + tip about Pantley pull-off

Check the event log at: `https://your-app-name.up.railway.app/log`

---

## How to talk to the bot

The bot understands natural language — just describe what's happening:

| What you type | What gets logged |
|--------------|-----------------|
| `"nap started"` / `"he's asleep"` | `nap_start` |
| `"woke up"` / `"nap done, 45 min"` | `nap_end` |
| `"night waking"` / `"he's up again"` | `night_wake` |
| `"gave bottle"` / `"fed him"` | `feeding` |
| `"crying"` / `"he's upset"` | `crying_start` |
| `"calm now"` / `"settled"` | `crying_end` |
| `"bedtime"` / `"going to sleep"` | `bedtime` |
| `"good morning"` / `"woke for the day"` | `wake_for_day` |

You can also just ask questions:
- `"How long has he been awake?"`
- `"Is this normal for 5 months?"`
- `"How do I do the Pantley pull-off?"`
- `"He's fighting sleep so hard tonight, help"`

---

## Sleep methods the bot teaches

All methods are **attachment-friendly** — no leaving him to cry alone:

**Pantley Pull-Off** — removes the bottle-to-sleep association gradually
**Fading** — step-by-step reduction of parental sleep involvement
**Pick Up/Put Down (PUPD)** — fully responsive, just teaches independent sleep onset

---

## Troubleshooting

**Bot not responding?**
- Check Railway logs (your service → **Deployments** → **View logs**)
- Verify the webhook URL in Twilio is correct
- Confirm both parents have activated the sandbox

**Not getting proactive messages?**
- Check `PARENT_WHATSAPP_NUMBERS` is formatted correctly with `whatsapp:` prefix
- Numbers must be in international format: `whatsapp:+972501234567`

**Events not being logged?**
- Visit `https://your-app.up.railway.app/log` to see the event database

---

## Timezone note

The bot's proactive messages (bedtime at 7 PM, morning summary at 7 AM) are based on UTC. If you're in Israel (UTC+3), set these in Railway:

| Variable | Value |
|----------|-------|
| `TZ` | `Asia/Jerusalem` |

This ensures the 7 PM reminder fires at your local 7 PM.
