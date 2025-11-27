# Connect to formr

Connects to formr using your normal login and the httr library which
supports persistent session cookies. Calling this function will persist
the specified host (by default https://formr.org) in further formr\_
function calls. You can change this by calling
[`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md)

## Usage

``` r
formr_connect(
  email = NULL,
  password = NULL,
  host = formr_last_host(),
  keyring = NULL
)
```

## Arguments

- email:

  your registered email address

- password:

  your password

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

- keyring:

  a shorthand for the account you're using

## Examples

``` r
if (FALSE) { # \dontrun{
formr_connect(keyring = "formr_diary_study_account" )
} # }
```
