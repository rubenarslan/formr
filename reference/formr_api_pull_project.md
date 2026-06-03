# Pull Project from Server Scaffolds folder structure if missing, then overwrites local files with Server state.

Pull Project from Server Scaffolds folder structure if missing, then
overwrites local files with Server state.

## Usage

``` r
formr_api_pull_project(run_name, dir = NULL, prompt = TRUE, verbose = TRUE)
```

## Arguments

- run_name:

  Name of the run.

- dir:

  Local directory to scaffold and write into. Defaults to
  [`formr_default_dir()`](https://rubenarslan.github.io/formr/reference/formr_default_dir.md);
  set that (or pass `dir`) since formr never writes to the working
  directory by default.

- prompt:

  Logical. If TRUE (default), asks for confirmation before overwriting
  when run interactively (unless `dir` is empty); in a non-interactive
  session it errors instead of overwriting unattended. Pass
  `prompt = FALSE` to overwrite without confirmation (e.g. in scripts).

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly `NULL` on success (or `FALSE` if the user declines the
overwrite prompt); called for its side effect of scaffolding `dir` and
writing the run's structure, settings, surveys and files from the
server.
