# formr 0.13.0 (development)

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