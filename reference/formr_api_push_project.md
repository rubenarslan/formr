# Push Project to Server

Uploads local project files (surveys, assets, settings) to the formr
server. Optionally monitors the directory for subsequent changes
(Watcher mode).

## Usage

``` r
formr_api_push_project(
  run_name,
  dir = NULL,
  watch = FALSE,
  background = TRUE,
  interval = 2,
  verbose = TRUE
)
```

## Arguments

- run_name:

  Name of the run.

- dir:

  Local directory to push from. Defaults to
  [`formr_default_dir()`](https://rubenarslan.github.io/formr/reference/formr_default_dir.md);
  set that (or pass `dir`) since formr never writes to the working
  directory by default.

- watch:

  Logical. If TRUE, keeps the connection open and uploads changes
  immediately when files are saved.

- background:

  Logical. If TRUE (default), launches watcher as an RStudio Job.

- interval:

  Seconds between checks (default 2).

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly `TRUE` when the watcher is launched as a background RStudio
job; otherwise invisibly `NULL`. Called for its side effect of uploading
the local project in `dir` to the server (optionally starting a
file-watcher).
