# Get result from formr

After obtaining a token from formr, use this request

## Usage

``` r
formr_api_results(request = NULL, token = NULL)
```

## Arguments

- request:

  parameter (see example, API docs)

- token:

  defaults to last used token

## Examples

``` r
if (FALSE) { # \dontrun{
request <- 
  list(
    "run[name]" = 'widgets',
    "run[sessions]" = 
      'PJ_nACjFQDEBhx7pMUfZQz3mV-OtetnpEdqT88aiY8eXE4-HegFI7Sri4yifxPXO',
    "surveys[all_widgets]" = "abode, yourstory, mc_god"
)
formr_api_results(request)
} # }
```
