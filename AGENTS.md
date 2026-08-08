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

## Local checkout notes

This checkout is managed by straight.el under
`/home/marek/.emacs.d/straight/repos/agent-shell`.

Local fixes for Codex ACP startup/output are preserved on branch
`agent/fix-codex-acp-auth-output` and pushed to Marek's fork:
`https://github.com/mrychlik/agent-shell.git`.

The upstream draft PR is:
`https://github.com/xenodium/agent-shell/pull/761`.

Normal straight.el package updates should not silently overwrite this branch's
committed changes while the checkout remains on
`agent/fix-codex-acp-auth-output`. Switching/resetting the repo to `main`, or
using a destructive straight.el rebuild/reclone/reset operation, can make the
local fix inactive or discard local checkout state. Before updating, check:

```sh
git -C /home/marek/.emacs.d/straight/repos/agent-shell branch --show-current
```

The expected branch is `agent/fix-codex-acp-auth-output`.
