# Disconnect from formr

Disconnects from formr if connected.

## Usage

``` r
formr_disconnect(host = formr_last_host())
```

## Arguments

- host:

  defaults to
  [`formr_last_host()`](https://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://rforms.org

## Value

Invisibly `TRUE` on a successful logout; called to log out and clear the
active session.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a live formr server and an authenticated session.
formr_disconnect()
} # }
```
