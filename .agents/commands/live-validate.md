# Live validation of agent-shell rendering

Run a live agent-shell session in batch mode and verify the buffer output.
This exercises the full rendering pipeline with real ACP traffic — the only
way to catch ordering, marker, and streaming bugs that unit tests miss.

## Prerequisites

- `ANTHROPIC_API_KEY` must be available (via `op run` / 1Password)
- `timvisher_emacs_agent_shell` must be on PATH
- Dependencies (acp.el-plus, shell-maker) in sibling worktrees or
  overridden via env vars

## How to run

```bash
cd "$(git rev-parse --show-toplevel)"
timvisher_agent_shell_checkout=. \
  timvisher_emacs_agent_shell claude --batch \
  1>/tmp/agent-shell-live-stdout.log \
  2>/tmp/agent-shell-live-stderr.log
```

Stderr shows heartbeat lines every 30 seconds.  Stdout contains the
full buffer dump once the agent turn completes.

## What to check in the output

1. **Fragment ordering**: tool call drawers should appear in
   chronological order (the order the agent invoked them), not
   reversed.  Look for `▶` lines — their sequence should match the
   logical execution order.

2. **No duplicate content**: each tool call output should appear
   exactly once.  Watch for repeated blocks of identical text.

3. **Prompt position**: the prompt line (`agent-shell>`) should
   appear at the very end of the buffer, after all fragments.

4. **Notices placement**: `[hook-trace]` and other notice lines
   should appear in a `Notices` section, not interleaved with tool
   call fragments.

## Enabling invariant checking

To run with runtime invariant assertions (catches corruption as it
happens rather than after the fact):

```elisp
;; Add to your init or eval before the session starts:
(setq agent-shell-invariants-enabled t)
```

When an invariant fires, a `*agent-shell invariant*` buffer pops up
with a debug bundle and recommended analysis prompt.

The content-store consistency check is O(N · buffer-size) per
mutation — every notification walks the buffer once for every
content-store entry.  That's fine for live-validate batch runs
but unsuitable for normal interactive use; keep
`agent-shell-invariants-enabled` off outside of debugging.

## Quick validation one-liner

```bash
cd "$(git rev-parse --show-toplevel)" && \
  timvisher_agent_shell_checkout=. \
  timvisher_emacs_agent_shell claude --batch \
  1>/tmp/agent-shell-live.log 2>&1 && \
  grep -n '▶' /tmp/agent-shell-live.log | head -20
```

If the `▶` lines are in logical order and the exit code is 0, the
rendering pipeline is healthy.
