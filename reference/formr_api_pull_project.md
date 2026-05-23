# Pull Project from Server Scaffolds folder structure if missing, then overwrites local files with Server state.

Pull Project from Server Scaffolds folder structure if missing, then
overwrites local files with Server state.

## Usage

``` r
formr_api_pull_project(run_name, dir = ".", prompt = TRUE)
```

## Arguments

- run_name:

  Name of the run.

- dir:

  Local directory (default ".").

- prompt:

  Logical. If TRUE (default), asks for confirmation before overwriting
  (unless dir is empty).
