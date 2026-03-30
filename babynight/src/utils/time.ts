import { config } from '../config';

/** Returns today's date string in YYYY-MM-DD in the configured timezone */
export function todayLocal(): string {
  return new Date().toLocaleDateString('en-CA', { timeZone: config.timezone });
}

/** Returns current time string HH:MM in the configured timezone */
export function nowTimeLocal(): string {
  return new Date().toLocaleTimeString('en-GB', {
    timeZone: config.timezone,
    hour: '2-digit',
    minute: '2-digit',
  });
}

/** Converts a local YYYY-MM-DD HH:MM string to a UTC ISO string */
export function localToISO(localDatetime: string): string {
  // If already an ISO string, return as-is
  if (localDatetime.includes('T') || localDatetime.includes('Z')) {
    return new Date(localDatetime).toISOString();
  }
  // Treat as local time string; new Date() interprets strings without TZ as local
  return new Date(localDatetime).toISOString();
}

/** Returns the ISO string for right now */
export function nowISO(): string {
  return new Date().toISOString();
}

/** Formats an ISO timestamp to a human-friendly time like "3:15 PM" */
export function formatTime(isoString: string): string {
  return new Date(isoString).toLocaleTimeString('en-US', {
    timeZone: config.timezone,
    hour: 'numeric',
    minute: '2-digit',
    hour12: true,
  });
}

/** Returns start-of-day ISO string for a given YYYY-MM-DD date string */
export function startOfDay(dateStr: string): string {
  return new Date(`${dateStr}T00:00:00`).toISOString();
}

/** Returns end-of-day ISO string for a given YYYY-MM-DD date string */
export function endOfDay(dateStr: string): string {
  return new Date(`${dateStr}T23:59:59`).toISOString();
}

/** Returns the last N dates as YYYY-MM-DD strings, newest first */
export function lastNDates(n: number): string[] {
  const dates: string[] = [];
  const now = new Date();
  for (let i = 0; i < n; i++) {
    const d = new Date(now);
    d.setDate(d.getDate() - i);
    dates.push(d.toLocaleDateString('en-CA', { timeZone: config.timezone }));
  }
  return dates;
}
