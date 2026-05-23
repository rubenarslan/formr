## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local Ubuntu 24.04, R 4.3.3
* R-hub v2 (GitHub Actions): linux, macos, windows, r-devel-linux-x86_64-debian-clang
* GitHub Actions (planned post-release): macOS, Windows, Ubuntu --
  release / oldrel / devel

## Notes on possibly-flagged items

* The `https://CRAN.R-project.org/package=formr` URL in `README.md`
  currently returns 404 because the package has not yet been
  released; it will resolve once this submission is accepted.

* The `https://api.rforms.org` URL appears in two vignettes inside
  example code blocks (`formr_api_authenticate(host =
  "https://api.rforms.org", ...)`). It is the canonical API endpoint
  and intentionally returns `403 Forbidden` for an unauthenticated
  request; the URL itself is valid.

* All `formr_api_*` and `formr_*` examples that contact a live formr
  server are wrapped in `\dontrun{}` because they require a personal
  account and credentials. Non-network examples (helpers,
  data-wrangling, plotting) run during `R CMD check`.

* Network-touching code paths in tests use the `vcr` package with
  pre-recorded cassettes (in `tests/fixtures/vcr_cassettes/`), so the
  test suite does not require internet access.

* `plotly` is in Suggests because only `formr_overview_sankey()` uses
  it. That function is intended to run server-side inside an
  `OverviewScriptPage` on a rforms.org instance and gates on
  `requireNamespace("plotly")`.
