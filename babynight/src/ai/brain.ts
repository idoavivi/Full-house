import Anthropic from '@anthropic-ai/sdk';
import { config } from '../config';
import { buildSystemPrompt, buildUserContext, getFormattedHistory } from './context';
import { toolDefinitions, executeTool } from './tools';
import { saveConversationMessage } from '../db/queries';

const client = new Anthropic({ apiKey: config.anthropicApiKey });

const MAX_TOOL_ROUNDS = 5; // Safety limit on agentic loop iterations

/**
 * Process a message from a parent through Claude.
 * Handles the full agentic loop: message → tools → response.
 */
export async function processMessage(opts: {
  phone: string;
  messageText: string;
  waMessageId?: string;
  reportedBy: string;
}): Promise<string> {
  const { phone, messageText, waMessageId, reportedBy } = opts;

  // Save the incoming message to conversation history
  saveConversationMessage(phone, 'user', messageText);

  const systemPrompt = buildSystemPrompt();
  const userContext = buildUserContext(phone);
  const history = getFormattedHistory(phone);

  // Build message history (exclude the most recent user message, which we'll add now)
  const historyWithoutLast = history.slice(0, -1); // last entry IS the message we just saved

  // The actual message we send to Claude includes the context block
  const fullUserMessage = `${userContext}\n\nParent message: "${messageText}"`;

  const messages: Anthropic.MessageParam[] = [
    ...historyWithoutLast.map(h => ({ role: h.role as 'user' | 'assistant', content: h.content })),
    { role: 'user', content: fullUserMessage },
  ];

  let response = await client.messages.create({
    model: config.model,
    max_tokens: 1024,
    system: systemPrompt,
    tools: toolDefinitions,
    messages,
  });

  // Agentic loop: keep processing tool calls until Claude returns a final text response
  let rounds = 0;
  while (response.stop_reason === 'tool_use' && rounds < MAX_TOOL_ROUNDS) {
    rounds++;

    // Collect all tool calls in this response
    const toolUseBlocks = response.content.filter(
      (block): block is Anthropic.ToolUseBlock => block.type === 'tool_use'
    );

    // Execute each tool call
    const toolResults: Anthropic.ToolResultBlockParam[] = await Promise.all(
      toolUseBlocks.map(async toolUse => {
        const result = await executeTool(
          toolUse.name,
          toolUse.input as Record<string, unknown>,
          reportedBy,
          messageText,
          // Only pass waMessageId on the first tool call to prevent duplicate-key errors
          rounds === 1 ? waMessageId : undefined
        );
        return {
          type: 'tool_result' as const,
          tool_use_id: toolUse.id,
          content: result,
        };
      })
    );

    // Add Claude's response + tool results back into the conversation
    messages.push({ role: 'assistant', content: response.content });
    messages.push({ role: 'user', content: toolResults });

    // Ask Claude to continue
    response = await client.messages.create({
      model: config.model,
      max_tokens: 1024,
      system: systemPrompt,
      tools: toolDefinitions,
      messages,
    });
  }

  // Extract the final text response
  const textBlocks = response.content.filter(
    (block): block is Anthropic.TextBlock => block.type === 'text'
  );

  const finalResponse = textBlocks.map(b => b.text).join('\n').trim();

  if (!finalResponse) {
    return "Got it! ✓";
  }

  // Save Claude's response to conversation history
  saveConversationMessage(phone, 'assistant', finalResponse);

  return finalResponse;
}

/**
 * Generate a proactive check-in message for the given time slot.
 * This is called by the scheduler — there's no parent message to parse.
 */
export async function generateCheckIn(checkInType: string): Promise<string> {
  const systemPrompt = buildSystemPrompt();

  // Get today's events for context (don't attach to a specific parent)
  const { getTodaySummary, getWeeklyTrends } = await import('../db/queries');
  const todaySummary = getTodaySummary();
  const weeklyTrends = getWeeklyTrends(7);

  const contextBlock = `Today's summary so far: ${JSON.stringify(todaySummary)}
Weekly trends: ${JSON.stringify(weeklyTrends.slice(0, 3))}`;

  const promptMap: Record<string, string> = {
    '07:00': 'Good morning check-in. Ask warmly how the night went — any wake-ups? how many feeds? when did the baby wake up for the day? Keep it short and friendly.',
    '09:30': 'Morning nap check-in. Ask how the morning is going and if the baby has gone down for the first nap yet.',
    '12:00': 'Midday check-in. Ask about feeding progress and nap status. Reference today\'s data if available.',
    '15:00': 'Afternoon nap check-in. Ask how the afternoon nap went or if it\'s still happening.',
    '18:30': 'Evening wind-down check-in. Summarize today\'s intake if data is available. Give a bedtime suggestion based on the last nap.',
    '20:30': 'Bedtime check-in. Ask how the bedtime routine went and if the baby is down. Offer encouragement.',
    '23:00': 'Daily summary time. Generate a warm, data-driven narrative summary of the day and 2-3 specific recommendations for tomorrow.',
  };

  const prompt = promptMap[checkInType] || `Send a ${checkInType} check-in message.`;

  const response = await client.messages.create({
    model: config.model,
    max_tokens: 512,
    system: systemPrompt,
    messages: [
      {
        role: 'user',
        content: `${contextBlock}\n\nTask: ${prompt}`,
      },
    ],
  });

  const textBlocks = response.content.filter(
    (block): block is Anthropic.TextBlock => block.type === 'text'
  );

  return textBlocks.map(b => b.text).join('\n').trim() || "Good morning! How did the night go? 🌅";
}
