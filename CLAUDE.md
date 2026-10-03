# Project instructions for AI agents

## Licensing (mandatory)

This repository is licensed under the **Boost Software License 1.0 (BSL-1.0)**.
The authoritative source is the `LICENSE` file at the repo root, issued in the
initial commit by the copyright holder.

Rules for every agent, in every session:

1. Before writing or editing any license text, copyright line, `License :`
   header, `license:` cabal field, README badge, or any other license
   reference, read `LICENSE` and `assertica.cabal` first.
2. Never write MIT, Apache, GPL, or any license other than BSL-1.0.
   Never choose a license from a template or default. Copy it from `LICENSE`.
3. Header format for every `.hs` file, exactly:
   ```
   Copyright   : (c) 2026 Ahmad Ali Parr
   License     : BSL-1.0
   ```
   The copyright holder is Ahmad Ali Parr. Do not invent other names.
4. Do not change `LICENSE`, the cabal `license:` field, or the copyright
   holder unless the user explicitly asks.
5. If a header, doc, or badge disagrees with `LICENSE`, stop and report the
   mismatch to the user instead of picking one silently.
6. Before finishing any task that adds files, run:
   `grep -rnwiE "MIT|apache|gpl" . --exclude-dir=.git` and confirm that
   nothing conflicts with BSL-1.0.

## Background

On 2026-10-03 an agent stamped `License : MIT` on all source files, the cabal
file, and the docs, contradicting the BSL-1.0 `LICENSE`. It was corrected in
PR #4. That must not recur.
