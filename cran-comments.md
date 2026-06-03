## Resubmission

This is a resubmission. Thank you for the review of 1.0.0. We have addressed
every point raised:

* **Missing `\value` in .Rd files.** Every exported function and method now has
  a `\value` (roxygen `@return`) tag describing the class/structure of the
  result and what it means. Functions called only for their side effects say so
  explicitly (e.g. "No return value, called for side effects" / "Invisibly
  `TRUE`; called for its side effect of ..."). (Re-exported objects in
  `reexports.Rd` use `\docType{import}` and the exported environment `.formr`
  uses `\format`, neither of which takes `\value`.)

* **Vignettes did not execute code.** All eight vignettes now run code. API
  calls are replayed offline from pre-recorded `vcr` cassettes shipped in
  `inst/extdata/vcr_cassettes/`, the reverse/aggregate pipeline runs on bundled
  example data in `inst/extdata/`, and the helper functions run on small
  in-vignette examples. Each vignette degrades gracefully (the network chunks
  are guarded by `requireNamespace("vcr")`) so it still builds without error if
  Suggests are unavailable.

* **`\dontrun{}` examples.** Kept (they need a live formr server / credentials),
  and each now starts with a one-line comment explaining why it is not run,
  e.g. `# Not run: needs a live formr server and an authenticated session.`

* **`print()`/`cat()` writing to the console.** Removed. Informational and
  progress output now uses `message()` (so it can be suppressed with
  `suppressMessages()`) and is additionally gated behind a new `verbose`
  argument on the functions that emit it. Warnings before destructive actions
  use `warning()` and only prompt when `interactive()`. `cat()` now appears only
  inside `print`/`summary` methods, where it is the rendering mechanism.

* **Writing to the user's home filespace.** No function writes to the working
  directory or home filespace by default. A new `formr_default_dir()` sets an
  in-memory, session-wide default output directory; until the user sets it (or
  passes `dir`/`save_path` explicitly) the writing helpers error rather than
  writing anywhere. All examples, vignettes and tests write only to
  `tempdir()`.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission (new submission to CRAN).

## Test environments

* local Ubuntu 24.04, R 4.3.3; macOS Sequoia 15.7.4, R 4.5.1
* R-hub v2 (GitHub Actions): linux, macos, windows, r-devel-linux-x86_64-debian-clang
* GitHub Actions: macOS, Windows, Ubuntu -- release / oldrel / devel

## Notes on possibly-flagged items

* The `https://CRAN.R-project.org/package=formr` URL in `README.md` currently
  returns 404 because the package has not yet been released; it will resolve
  once this submission is accepted.

* The `https://api.rforms.org` URL appears in vignette example code blocks. It
  is the canonical API endpoint and intentionally returns `403 Forbidden` for an
  unauthenticated request; the URL itself is valid.

* Network-touching code paths in tests and vignettes use the `vcr` package with
  pre-recorded cassettes, so neither the test suite nor the vignettes require
  internet access.

* `plotly` is in Suggests because only `formr_overview_sankey()` uses it; that
  function runs server-side on a rforms.org instance and gates on
  `requireNamespace("plotly")`.
