# Get token expiry information

Returns information about when the current token expires.

## Usage

``` r
formr_api_token_expiry()
```

## Value

A list with:

- `expires_at`: POSIXct of expiry time (or NULL if unknown)

- `seconds_left`: Seconds until expiry (or NA if unknown)

- `is_expired`: TRUE if token has expired
