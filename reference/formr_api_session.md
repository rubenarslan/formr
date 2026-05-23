# Get Current API session

Returns the current session object or NULL if not authenticated.

## Usage

``` r
formr_api_session()
```

## Value

A list, or NULL if not authenticated:

- `base_url`: parsed URL (httr style).

- `token`: the bearer access token.

- `scope`: space-delimited string of scopes granted to this token.
  `NA_character_` when the auth path couldn't introspect (direct
  access-token authentication, or older server). `""` means the
  credential was issued with no scopes — every API call will 403 until
  the user picks scopes at admin/account#api.

- `expires_at`: POSIXct of token expiry (or NULL).
