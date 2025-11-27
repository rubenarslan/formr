# Connect to formr API

Connects to formr using your client_id and client_secret (OAuth 2.0
grant type: client_credentials).

## Usage

``` r
formr_api_access_token(
  client_id,
  client_secret,
  host = "https://api.formr.org/"
)
```

## Arguments

- client_id:

  your client_id

- client_secret:

  your client_secret

- host:

  defaults to https://formr.org

## Examples

``` r
if (FALSE) { # \dontrun{
formr_api_access_token(client_id = 'your_id', client_secret = 'your_secret' )
} # }
```
