# pass in the url to the RDS representation of a openCPU session object, get the object

useful to programmatically access openCPU session object stored in
character variables etc.

## Usage

``` r
get_opencpu_rds(session_url, local = TRUE)
```

## Arguments

- session_url:

  the session url, e.g.
  https://public.opencpu.org/ocpu/tmp/x02a93ec/R/.val/rds

- local:

  defaults to FALSE, if true, will assume that the session is not on
  another server, and do some not-very-smart substitution to load it via
  the file system instead of HTTP/HTTPS

## Examples

``` r
if (FALSE) { # \dontrun{
get_opencpu_rds('https://public.opencpu.org/ocpu/tmp/x02a93ec/R/.val/rds')
} # }
```
