# Download run structure from formr

After connecting to formr using
[`formr_connect()`](https://rubenarslan.github.io/formr/reference/formr_connect.md)
you can download the study/run structure using this command.

## Usage

``` r
formr_run_structure(run_name, host = formr_last_host())
```

## Arguments

- run_name:

  case-sensitive name of a run your account owns

- host:

  defaults to
  [`formr_last_host()`](https://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://rforms.org

## Value

A list (parsed JSON) describing the run structure (its `units`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a live formr server and an authenticated session.
formr_run_structure(run_name = 'training_diary' )
} # }
```
