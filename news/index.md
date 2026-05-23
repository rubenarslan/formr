# Changelog

## formr 1.0.0

Initial CRAN release. Released alongside formr.org server v1.0.0. Major
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
  is the higher-level helper that the formr.org default
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
