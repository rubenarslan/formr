# Download processed, aggregated results from formr

After connecting to formr using
[`formr_connect()`](http://rubenarslan.github.io/formr/reference/formr_connect.md)
you can download data and process it. This approach calls the following
functions in the right sequence:
[`formr_raw_results()`](http://rubenarslan.github.io/formr/reference/formr_raw_results.md)
[`formr_items()`](http://rubenarslan.github.io/formr/reference/formr_items.md),
[`formr_item_displays()`](http://rubenarslan.github.io/formr/reference/formr_item_displays.md)
and
[`formr_post_process_results()`](http://rubenarslan.github.io/formr/reference/formr_post_process_results.md).
So, results are downloaded, metadata on items (labels etc.) is added,
normal and missing values are labelled. In the end, items like
bfi_extra_3R are reversed in place (maintaining labels but changing
underlying numbers), and scales are aggregated (bfi_extra_1,
bfi_extra_2, bfi_extra_3R become bfi_extra)

## Usage

``` r
formr_results(survey_name, host = formr_last_host(), ...)
```

## Arguments

- survey_name:

  case-sensitive name of a survey your account owns

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

- ...:

  passed to
  [`formr_post_process_results()`](http://rubenarslan.github.io/formr/reference/formr_post_process_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
formr_results(survey_name = 'training_diary' )
} # }
```
