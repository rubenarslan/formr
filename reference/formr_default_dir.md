# Get or set the default directory for downloads and backups

formr's file-writing functions (e.g.
[`formr_backup_study()`](https://rubenarslan.github.io/formr/reference/formr_backup_study.md),
[`formr_api_backup_run()`](https://rubenarslan.github.io/formr/reference/formr_api_backup_run.md),
[`formr_api_pull_project()`](https://rubenarslan.github.io/formr/reference/formr_api_pull_project.md))
never write to your working directory by default. Instead they default
their destination to the value stored here, which is unset (`NULL`)
until you choose one — so nothing is ever written until you opt in. Call
this function with a path to set a session-wide default, or without
arguments to read the current value. The path is held in memory for the
current R session only; nothing is written to disk to persist it. There
is no separate reset: the value lasts until you overwrite it with
another path or your R session ends.

## Usage

``` r
formr_default_dir(dir = NULL)
```

## Arguments

- dir:

  a single directory path to use as the default. If `NULL` (the default)
  the stored value is returned unchanged. The directory itself is
  created on first write if it does not yet exist.

## Value

The current default directory as a length-1 character string, or `NULL`
when none has been set.

## Examples

``` r
formr_default_dir(tempdir())
#> [1] "/tmp/RtmpAoCG5N"
formr_default_dir()
#> [1] "/tmp/RtmpAoCG5N"
```
