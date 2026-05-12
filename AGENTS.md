# Agent Shell - Project Guidelines

## Communication norms

PR and issue conversations are human relationships. The maintainer prefers
talking directly to humans.

When contributing:

- Write your own PR descriptions and issue comments. Don't have AI generate them.
- If you used AI to research something, summarize the findings in your own words
  and give your level of endorsement rather than pasting AI output verbatim.
  Concise, human-written summaries save the maintainer from having to parse
  lengthy generated text.
- Review all code in your PR yourself and vouch for its quality.

## Contributing

This is an Emacs Lisp project. See [CONTRIBUTING.org](CONTRIBUTING.org) for style guidelines, code checks, and testing. Please adhere to these guidelines.

## Development workflow

When adding or changing features:

1. **Run `bin/test`.** Set `acp_root` and `shell_maker_root` if the
   deps aren't in sibling worktrees. This runs byte-compilation, ERT
   tests, dependency DAG check, and checks that `README.org` was
   updated when code changed. Requires `yq` (`brew install yq`) — the
   script parses `.github/workflows/ci.yml` to derive the same emacs
   invocations CI uses.
2. **Keep the README features list current.** The "Features on top of
   agent-shell" section in `README.org` must be updated whenever code
   changes land. Both `bin/test` and CI enforce this — changes to `.el`
   or `tests/` files without a corresponding `README.org` update will
   fail.
3. **Live-validate rendering changes.** For changes to the rendering
   pipeline (fragment insertion, streaming, markers, UI), run a live
   batch session to verify fragment ordering and buffer integrity.
   See `.agents/commands/live-validate.md` for details. The key command:
   ```bash
   timvisher_agent_shell_checkout=. timvisher_emacs_agent_shell claude --batch \
     1>/tmp/agent-shell-live.log 2>&1
   ```
