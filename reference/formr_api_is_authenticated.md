# Check if currently authenticated

Checks if there is a valid, non-expired session. Does NOT verify token
validity with the server (use formr_api_session() for that).

## Usage

``` r
formr_api_is_authenticated()
```

## Value

TRUE if authenticated and token not expired, FALSE otherwise.
