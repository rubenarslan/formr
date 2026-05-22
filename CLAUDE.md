# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

R package `formr` (CRAN), the client-side companion to the formr.org survey framework. The same package is *also* loaded server-side inside formr.org via openCPU, so exported functions are called both from local user R sessions and from within running formr studies — avoid changes that silently break either context.

## Common commands

```r
# from an R session at the repo root
devtools::load_all()                              # iterate without reinstall
devtools::document()                              # regenerate man/ + NAMESPACE from roxygen (do NOT hand-edit NAMESPACE)
devtools::test()                                  # run full testthat suite
devtools::test(filter = "shorthands")             # run tests/testthat/test-shorthands.R only
devtools::test_active_file("tests/testthat/test-connect_to_formr.R")
devtools::check()                                 # R CMD check equivalent — run before PRs
lintr::lint_package()                             # uses .lintr (indentation linter disabled; tabs + spaces are both tolerated)
pkgdown::build_site()                             # rebuild docs/ site
```

GitHub Actions (`.github/workflows/`) run `pkgdown` and `test-coverage` only — there is no R CMD check workflow, so run `devtools::check()` locally.

## Architecture

Two entirely separate authentication/data paths exist; they must not be conflated:

- **Cookie/session path** (`R/connect_to_formr.R`) — `formr_connect()` POSTs email/password (+ optional TOTP via the `otp` package) to `/admin/account/login` and relies on httr's persistent cookie jar. All `formr_results()`, `formr_raw_results()`, `formr_run_structure()`, `formr_backup_*`, `formr_upload_items()`, `formr_user_*` calls reuse that cookie against the host stored by `formr_last_host()` (a closure-based singleton; calling it with an argument mutates global state for the rest of the session). This is the historical, admin-UI-scraping path and is what `formr_store_keys()` feeds.
- **OAuth client-credentials path** (`R/formr_api.R`) — `formr_api_access_token()` hits `https://api.formr.org/oauth/access_token`, stores the URL + token in the `.formr_current_session` closure, and `formr_api_results()` / `formr_api_session()` read from there. This is a separate, much narrower surface.

Credentials for the cookie path are expected to live in the system keyring via `formr_store_keys(account_name, email, secret_2fa)`. `formr_connect(keyring = "…")` looks up both the password (`username = email`) and the 2FA seed (`username = paste(email, "2FA")`) under the same service name; keep that dual-key convention when touching key handling.

Other modules:
- `R/data_wrangling.R` — `reverse_labelled_values()` and friends; operates on `haven::labelled` vectors and is the only place label-preserving numeric reversal lives.
- `R/feedback_plotting.R` — `qplot_on_normal/bar/polar` produce per-participant feedback graphics, typically called *from inside a formr study* (server-side) via openCPU.
- `R/shorthands.R` — R helpers intended to be called from formr survey showif/value expressions (`current()`, `expired()`, `finished()`, `in_time_window()`, `%contains%`, …). Small, cheap, side-effect-free — the formr expression engine evaluates them repeatedly.
- `R/rmarkdown_options.R` — custom `commonmark`/`rmarkdown` renderers (`formr_render_commonmark`, `markdown_custom_options`, `formr_inline_render`) used to render participant-facing markdown inside studies.
- `R/text_message.R` — thin wrappers for Twilio / Clickatell / massenversand SMS APIs used by formr runs.

## Conventions to preserve

- Documentation is roxygen2 with `Roxygen: list(markdown = TRUE)`. Edit the roxygen blocks in `R/*.R`, then run `devtools::document()` — never hand-edit `NAMESPACE` or files under `man/`.
- **Every new exported function or object must also be added to `_pkgdown.yml` under the appropriate `reference:` section** (e.g. `Setup & Authentication`, `API - Runs & Sessions`, `Helpers & Shorthands`). pkgdown errors out the GitHub Actions build with "topic missing from index" otherwise. If a symbol is genuinely internal, mark it with `@keywords internal` instead so pkgdown skips it.
- `testthat` edition 3 (see `DESCRIPTION`); use `expect_*` style, not the legacy `expect_that(..., equals(...))` form.
- `formr_last_host()` is authoritative for the active host across cookie-path functions; new functions that talk to formr.org should default `host = formr_last_host()` rather than hardcoding a URL.
- Indentation in existing files mixes tabs and spaces on purpose (the `indentation_linter` is disabled in `.lintr`). When editing a file, match whatever that file already uses instead of reflowing it.
