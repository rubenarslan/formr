# formr 1.1.0

* **CRAN resubmission fixes (addressing the 1.0.0 review):**
  * Every exported function and method now documents its return value with
    `\value`, describing the class/structure and meaning of the output.
  * Functions no longer write informational output with `cat()`/`print()`.
    Progress and status messages now use `message()` and can be silenced with a
    new `verbose` argument; the warnings before destructive actions use
    `warning()` and only prompt when `interactive()`.
  * New `formr_default_dir()` sets a session-wide default output directory.
    The writing helpers (`formr_backup_study()`, `formr_backup_surveys()`,
    `formr_backup_files()`, `formr_api_backup_run()`, `formr_api_pull_project()`,
    `formr_api_push_project()`) no longer default to the working directory: set
    `formr_default_dir()` once, or pass `dir`/`save_path` explicitly. In
    examples, vignettes and tests these write only to `tempdir()`.
  * Vignettes now execute code — API calls are replayed offline from bundled
    `vcr` cassettes, and the reverse/aggregate pipeline runs on bundled example
    data — so users can run them and CRAN can test them.
  * `\dontrun{}` examples now begin with a one-line comment explaining why they
    are not run.

* **`formr_api_fetch_results()` now defaults `run_name` to `.formr$run_name`**,
  matching `formr_api_results()` and `formr_overview_sankey()`. Code running
  inside an OpenCPU session on rforms.org can omit the argument; outside,
  the function errors with a clear message if the run name is unset.

* **Vignette rework: `run-r-inside-your-study.Rmd`** gains end-to-end
  walkthroughs for the v1 API's cross-session data path -- a participant
  counter, real-time group norms, dynamic group balancing, and a
  waiting-room synchronisation pattern. Smaller clarifications in the
  fetch-and-process-results, manage-your-sessions, and manage-your-surveys
  vignettes.

* **`docs/` is no longer tracked.** Deployment to the pkgdown site is
  already handled by `.github/workflows/pkgdown.yaml` pushing to
  `gh-pages`, so the in-tree mirror was redundant and went stale on every
  PR.

# formr 1.0.0

Initial CRAN release. Released alongside rforms.org server v1.0.0.
Major version bump tracks the formr v1 RESTful API surface
stabilising — `formr_api_*` is now the supported entry point for new
code; the legacy `formr_results()` / `formr_raw_results()` ("Classic")
path continues to work but is no longer the recommended starting
point in the vignette.

* **`formr_api_unit_sessions()`** wraps the new
  `GET /v1/runs/{name}/unit_sessions` endpoint — one row per
  (participant × unit × iteration), ordered by `(session, created,
  unit_session_id)` so consecutive rows per participant form trajectory
  edges. Useful for drop-off analytics and debugging stuck
  participants. Filters: `session_codes`, `testing`, `since`;
  pagination via `limit` / `offset`. Scope: `session:read`.

* **`formr_overview_sankey()`** is the higher-level helper that the
  rforms.org default OverviewScriptPage now uses. Pulls the unit-session
  history, collapses re-iteration to one node per position so the
  Sankey stays acyclic (diary / longitudinal designs would otherwise
  draw cycles), and surfaces the average per-participant visit count
  as an "avg N visits" label suffix when it exceeds 1. Top-to-bottom
  orientation by default; pass `orientation = "h"` for left-to-right.

* **Vignette walks through the new multi-credential account page.** Server
  v0.26.x lets a user hold several labelled OAuth credentials side by
  side (each with its own scopes + run allowlist). On the wire nothing
  changed — `formr_store_keys()` / `formr_api_authenticate()` already
  accepted an `account` parameter that namespaces credentials in the
  keyring. The vignette now nudges users to pass the server-side label
  as `account` so the local store and the server's credential page line
  up by name.

* **`formr_api_session()` now exposes the granted OAuth `scope`.** After
  `formr_api_authenticate()` returns, `formr_api_session()$scope` holds
  the space-delimited scope string the server stamped on the token.
  `NA_character_` when the auth path can't introspect (direct
  access-token authentication, or older server). The auth success
  message surfaces the granted scopes inline, and an empty scope string
  (a credential with no scopes selected at `admin/account#api`) emits
  a warning at auth time so users don't debug blind 403s.

* **Actionable error messages on scoping-aware 403s.** When the v1 API
  returns `Insufficient permissions: '<scope>' scope required`, the
  package's error appends a hint pointing at the credential page and
  prints the currently-granted scopes. Same for the per-credential
  run-allowlist failure (`not authorized for run`), the survey-via-run
  failure (`not authorized for survey`), and the new-survey-create
  guard for run-restricted credentials. The 403 body is preserved so
  programmatic callers can still pattern-match.

# formr 0.11.1

* Update formr_store_keys to accept secret and email as arguments.

# formr 0.11.0

* enable 2FA for formr_store_keys/formr_login
* backup entire studies with one function using formr_backup_study
* download many surveys at once with formr_backup_surveys
* removed cruft unrelated to formr (ls_by_class, n_missing, loadRDS, crosstabs, props)
* added tests
