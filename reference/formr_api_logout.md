# Revoke Access Token (Logout)

Invalidates the current access token on the server and clears the local
session state.

## Usage

``` r
formr_api_logout(verbose = TRUE)
```

## Arguments

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly `TRUE` on success (or `FALSE` if there was no active session);
called to revoke the access token on the server and clear the local
session.
