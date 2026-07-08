# Changelog

## formr 1.2.0

- `keyring` and `otp` moved from Imports to Suggests so that formr can
  be installed on WebAssembly/webR (`keyring` needs a system credential
  store and has no wasm build; all remaining hard dependencies are
  available as wasm binaries). Both packages are only needed for the
  credential-storage convenience path. Functions that need a suggested
  package
  ([`formr_store_keys()`](https://rubenarslan.github.io/formr/reference/formr_store_keys.md),
  `formr_connect(keyring = ...)`,
  [`formr_overview_sankey()`](https://rubenarslan.github.io/formr/reference/formr_overview_sankey.md),
  [`formr_render_commonmark()`](https://rubenarslan.github.io/formr/reference/formr_render_commonmark.md))
  now check for it via
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html):
  in interactive sessions you are offered to install the package on
  first use, otherwise they error with an informative message.
  [`formr_connect()`](https://rubenarslan.github.io/formr/reference/formr_connect.md)
  falls back to prompting for the 2FA code manually when `otp` is
  unavailable.

- [`formr_render()`](https://rubenarslan.github.io/formr/reference/formr_render.md)
  and
  [`formr_inline_render()`](https://rubenarslan.github.io/formr/reference/formr_inline_render.md)
  now pick their write directory automatically, reconciling the
  rforms.org/OpenCPU integration with CRAN policy. Inside an
  OpenCPU/formr session — detected because the `opencpu` namespace is
  loaded on the server, or because rforms.org populated the per-request
  `.formr` environment — they keep writing `knit.Rmd`/`knit.html` to the
  (ephemeral, per-request) working directory that the server reads via
  `getFiles("knit.html")`, exactly as in 1.1.2. In ordinary R sessions
  they write to [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  instead, as in 1.1.1, so the package no longer touches the user’s
  working directory by default. Override the detection with
  `options(formr.in_opencpu = TRUE/FALSE)` or pass the new `dir`
  argument explicitly.

## formr 1.1.2

- Hotfix:
  [`formr_render()`](https://rubenarslan.github.io/formr/reference/formr_render.md)
  and
  [`formr_inline_render()`](https://rubenarslan.github.io/formr/reference/formr_inline_render.md)
  again write their output to a file named `knit.html` in the working
  directory. In 1.1.1 the CRAN review changes routed rendering through
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) with a random
  filename, which broke rforms.org/OpenCPU — the server serves the
  rendered page via `getFiles("knit.html")` and so could no longer find
  it. If you are on 1.1.1, upgrade to 1.1.2 (or pin to 1.1.0).

## formr 1.1.1

- [`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md)
  (via
  [`formr_api_recognise()`](https://rubenarslan.github.io/formr/reference/formr_api_recognise.md))
  no longer corrupts `calculate` items. They are now **always returned
  as strings**: a `calculate` item can legitimately hold non-numeric
  text — e.g. a CSV blob read from a file that merely *starts* with a
  number, like `"6136,63,50,woman,man"` — which the old code
  force-coerced with
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html), silently
  turning every row into `NA`. `number`/`range` items remain numeric but
  are now coerced only when lossless
  (<https://github.com/rubenarslan/formr/issues/45>).

## formr 1.1.0

CRAN release: 2026-06-16

- **CRAN resubmission fixes (addressing the 1.0.0 review):**

  - Every exported function and method now documents its return value
    with `\value`, describing the class/structure and meaning of the
    output.
  - Functions no longer write informational output with
    [`cat()`](https://rdrr.io/r/base/cat.html)/[`print()`](https://rdrr.io/r/base/print.html).
    Progress and status messages now use
    [`message()`](https://rdrr.io/r/base/message.html) and can be
    silenced with a new `verbose` argument; the warnings before
    destructive actions use
    [`warning()`](https://rdrr.io/r/base/warning.html). The confirmation
    prompt now only appears in interactive sessions:
    destructive/overwriting calls
    ([`formr_api_delete_run()`](https://rubenarslan.github.io/formr/reference/formr_api_delete_run.md),
    [`formr_api_delete_survey()`](https://rubenarslan.github.io/formr/reference/formr_api_delete_survey.md),
    [`formr_api_delete_all_files()`](https://rubenarslan.github.io/formr/reference/formr_api_delete_all_files.md),
    [`formr_api_backup_run()`](https://rubenarslan.github.io/formr/reference/formr_api_backup_run.md),
    [`formr_api_pull_project()`](https://rubenarslan.github.io/formr/reference/formr_api_pull_project.md))
    **error** in a non-interactive session rather than proceeding
    unattended — pass `prompt = FALSE` to confirm in scripts.
  - New
    [`formr_default_dir()`](https://rubenarslan.github.io/formr/reference/formr_default_dir.md)
    sets a session-wide default output directory. The writing helpers
    ([`formr_backup_study()`](https://rubenarslan.github.io/formr/reference/formr_backup_study.md),
    [`formr_backup_surveys()`](https://rubenarslan.github.io/formr/reference/formr_backup_surveys.md),
    [`formr_backup_files()`](https://rubenarslan.github.io/formr/reference/formr_backup_files.md),
    [`formr_api_backup_run()`](https://rubenarslan.github.io/formr/reference/formr_api_backup_run.md),
    [`formr_api_pull_project()`](https://rubenarslan.github.io/formr/reference/formr_api_pull_project.md),
    [`formr_api_push_project()`](https://rubenarslan.github.io/formr/reference/formr_api_push_project.md))
    no longer default to the working directory: set
    [`formr_default_dir()`](https://rubenarslan.github.io/formr/reference/formr_default_dir.md)
    once, or pass `dir`/`save_path` explicitly. In examples, vignettes
    and tests these write only to
    [`tempdir()`](https://rdrr.io/r/base/tempfile.html).
  - Vignettes now execute code — API calls are replayed offline from
    bundled `vcr` cassettes, and the reverse/aggregate pipeline runs on
    bundled example data — so users can run them and CRAN can test them.
  - `\dontrun{}` examples now begin with a one-line comment explaining
    why they are not run.

- **[`formr_api_fetch_results()`](https://rubenarslan.github.io/formr/reference/formr_api_fetch_results.md)
  now defaults `run_name` to `.formr$run_name`**, matching
  [`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md)
  and
  [`formr_overview_sankey()`](https://rubenarslan.github.io/formr/reference/formr_overview_sankey.md).
  Code running inside an OpenCPU session on rforms.org can omit the
  argument; outside, the function errors with a clear message if the run
  name is unset.

- **Vignette rework: `run-r-inside-your-study.Rmd`** gains end-to-end
  walkthroughs for the v1 API’s cross-session data path – a participant
  counter, real-time group norms, dynamic group balancing, and a
  waiting-room synchronisation pattern. Smaller clarifications in the
  fetch-and-process-results, manage-your-sessions, and
  manage-your-surveys vignettes.

- **`docs/` is no longer tracked.** Deployment to the pkgdown site is
  already handled by `.github/workflows/pkgdown.yaml` pushing to
  `gh-pages`, so the in-tree mirror was redundant and went stale on
  every PR.

## formr 1.0.0

Initial CRAN release. Released alongside rforms.org server v1.0.0. Major
version bump tracks the formr v1 RESTful API surface stabilising —
`formr_api_*` is now the supported entry point for new code; the legacy
[`formr_results()`](https://rubenarslan.github.io/formr/reference/formr_results.md)
/
[`formr_raw_results()`](https://rubenarslan.github.io/formr/reference/formr_raw_results.md)
(“Classic”) path continues to work but is no longer the recommended
starting point in the vignette.

- **[`formr_api_unit_sessions()`](https://rubenarslan.github.io/formr/reference/formr_api_unit_sessions.md)**
  wraps the new `GET /v1/runs/{name}/unit_sessions` endpoint — one row
  per (participant × unit × iteration), ordered by
  `(session, created, unit_session_id)` so consecutive rows per
  participant form trajectory edges. Useful for drop-off analytics and
  debugging stuck participants. Filters: `session_codes`, `testing`,
  `since`; pagination via `limit` / `offset`. Scope: `session:read`.

- **[`formr_overview_sankey()`](https://rubenarslan.github.io/formr/reference/formr_overview_sankey.md)**
  is the higher-level helper that the rforms.org default
  OverviewScriptPage now uses. Pulls the unit-session history, collapses
  re-iteration to one node per position so the Sankey stays acyclic
  (diary / longitudinal designs would otherwise draw cycles), and
  surfaces the average per-participant visit count as an “avg N visits”
  label suffix when it exceeds 1. Top-to-bottom orientation by default;
  pass `orientation = "h"` for left-to-right.

- **Vignette walks through the new multi-credential account page.**
  Server v0.26.x lets a user hold several labelled OAuth credentials
  side by side (each with its own scopes + run allowlist). On the wire
  nothing changed —
  [`formr_store_keys()`](https://rubenarslan.github.io/formr/reference/formr_store_keys.md)
  /
  [`formr_api_authenticate()`](https://rubenarslan.github.io/formr/reference/formr_api_authenticate.md)
  already accepted an `account` parameter that namespaces credentials in
  the keyring. The vignette now nudges users to pass the server-side
  label as `account` so the local store and the server’s credential page
  line up by name.

- **[`formr_api_session()`](https://rubenarslan.github.io/formr/reference/formr_api_session.md)
  now exposes the granted OAuth `scope`.** After
  [`formr_api_authenticate()`](https://rubenarslan.github.io/formr/reference/formr_api_authenticate.md)
  returns, `formr_api_session()$scope` holds the space-delimited scope
  string the server stamped on the token. `NA_character_` when the auth
  path can’t introspect (direct access-token authentication, or older
  server). The auth success message surfaces the granted scopes inline,
  and an empty scope string (a credential with no scopes selected at
  `admin/account#api`) emits a warning at auth time so users don’t debug
  blind 403s.

- **Actionable error messages on scoping-aware 403s.** When the v1 API
  returns `Insufficient permissions: '<scope>' scope required`, the
  package’s error appends a hint pointing at the credential page and
  prints the currently-granted scopes. Same for the per-credential
  run-allowlist failure (`not authorized for run`), the survey-via-run
  failure (`not authorized for survey`), and the new-survey-create guard
  for run-restricted credentials. The 403 body is preserved so
  programmatic callers can still pattern-match.

## formr 0.11.1

- Update formr_store_keys to accept secret and email as arguments.

## formr 0.11.0

- enable 2FA for formr_store_keys/formr_login
- backup entire studies with one function using formr_backup_study
- download many surveys at once with formr_backup_surveys
- removed cruft unrelated to formr (ls_by_class, n_missing, loadRDS,
  crosstabs, props)
- added tests
