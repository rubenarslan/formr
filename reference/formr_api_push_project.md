# Push Project to Server

Uploads local project files (surveys, assets, settings) to the formr
server. Optionally monitors the directory for subsequent changes
(Watcher mode).

## Usage

``` r
formr_api_push_project(
  run_name,
  dir = ".",
  watch = FALSE,
  background = TRUE,
  interval = 2
)
```

## Arguments

- run_name:

  Name of the run.

- dir:

  Local directory (default ".").

- watch:

  Logical. If TRUE, keeps the connection open and uploads changes
  immediately when files are saved.

- background:

  Logical. If TRUE (default), launches watcher as an RStudio Job.

- interval:

  Seconds between checks (default 2).
