# Fetch & Process Results

``` r

library(formr)

# So this vignette can run offline, the API calls below are replayed from
# pre-recorded responses (vcr "cassettes" shipped with the package). With a
# real server you would simply call formr_api_authenticate() with your own
# host and credentials instead of this block.
.formr_vcr <- requireNamespace("vcr", quietly = TRUE) &&
  nzchar(system.file("extdata/vcr_cassettes", package = "formr"))

if (.formr_vcr) {
  vcr::vcr_configure(
    dir = system.file("extdata/vcr_cassettes", package = "formr"),
    filter_sensitive_data = list(
      "formr-client-id-redacted"     = "dummy_client_id",
      "formr-client-secret-redacted" = "dummy_client_secret",
      "formr-host-redacted"          = "api.localhost"
    )
  )
  vcr::use_cassette("formr_api_authenticate", {
    formr_api_authenticate(host = "http://api.localhost",
      client_id = "dummy_client_id", client_secret = "dummy_client_secret",
      verbose = FALSE)
  })
}
```

The new `formr` API (V1) provides a streamlined pipeline for importing
data. The API strictly separates **fetching** (getting data from the
server) from **wrangling** (processing types, reversals, and scales),
but the package provides a powerful convenience wrapper,
[`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md),
to handle the entire process in one step.

## 1. Authentication

Before fetching data, ensure you are authenticated. If you have stored
your keys (see [Getting
Started](https://rubenarslan.github.io/formr/articles/getting-started.md)),
this is automatic.

``` r

# On a real server, authenticate with your own host/account:
formr_api_authenticate(host = "https://api.rforms.org", account = "dashboard")
```

## 2. The One-Stop Solution (`formr_api_results`)

The primary function is
[`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md).
By default, it performs a “Smart Fetch” that downloads every survey in
the run, recognises data types, reverses items ending in `R`, aggregates
scales, and joins everything into one wide tibble (one row per session).

``` r

# Replayed from a recorded response for a run called "test-run":
vcr::use_cassette("formr_api_results_fetch_single", {
  df <- formr_api_results("test-run", verbose = FALSE)
})
class(df)   # a "formr_results" tibble, ready for analysis
#> [1] "formr_results" "tbl_df"        "tbl"           "data.frame"
```

### What does `formr_api_results` do by default?

1.  **Downloads** results and metadata for all surveys in the run.
2.  **Recognises** data types (converts “2023-01-01” to Dates, JSON
    arrays to lists, etc.).
3.  **Reverses** items ending in `R` (e.g., `neuro_3R`) based on item
    choices.
4.  **Aggregates** scales (calculates means for items with a shared
    stem).
5.  **Joins** all surveys into a single “wide” format (one row per
    session).

### Customizing the Fetch

You can toggle these behaviors using arguments if you want raw data or
need to handle processing manually:

``` r

# Get processed data, but strictly separated by survey (no join)
list_of_dfs <- formr_api_results("daily_diary", join = FALSE)

# Get raw data (types recognized, but NO reversing or scoring)
raw_data <- formr_api_results("daily_diary", compute_scales = FALSE)
```

------------------------------------------------------------------------

## 3. Advanced: The Manual Pipeline

If you set `compute_scales = FALSE`, or you are debugging why a scale
isn’t calculating, you can call the lower-level processing functions
yourself. They take the raw results plus the survey’s item table
(metadata).

To show this running offline, we use a small example dataset bundled
with the package instead of a live download. On a real server you would
get `results` from `formr_api_results(..., compute_scales = FALSE)` and
`item_list` from
[`formr_api_survey_structure()`](https://rubenarslan.github.io/formr/reference/formr_api_survey_structure.md).

``` r

# Raw results: one row per session, items not yet reversed or scored.
results <- jsonlite::fromJSON(
  system.file("extdata/gods_example_results.json", package = "formr"))

# Item metadata: a table with (at least) a `name` and a `choices` column —
# exactly what formr_api_survey_structure() returns from the server.
items_raw <- jsonlite::fromJSON(
  system.file("extdata/gods_example_items.json", package = "formr"),
  simplifyVector = FALSE)
item_list <- data.frame(
  name = vapply(items_raw$items, function(it) it$name, character(1)),
  stringsAsFactors = FALSE)
item_list$choices <- lapply(items_raw$items, function(it) it$choices)

head(results[, c("session", "religiousness_2R", "religiousness_3")])
#>   session religiousness_2R religiousness_3
#> 1       1                3               2
#> 2       2                5               1
#> 3       3                5               1
#> 4       4                4               3
#> 5       5                3               2
#> 6       6                3               4
```

### B. Reverse Coding (`formr_api_reverse`)

This function automatically reverses items whose name ends in `R`
(e.g. `religiousness_2R`). It uses each item’s defined choices to
compute the reversal: $`New = (Max + Min) - Old`$.

``` r

df_reversed <- formr_api_reverse(results = results, item_list = item_list)

# religiousness_2R is reverse-keyed — its values are now flipped:
data.frame(before = results$religiousness_2R,
           after  = df_reversed$religiousness_2R)
#>    before after
#> 1       3     2
#> 2       5     0
#> 3       5     0
#> 4       4     1
#> 5       3     2
#> 6       3     2
#> 7       2     3
#> 8       5     0
#> 9       4     1
#> 10      3     2
#> 11      5     0
#> 12      1     4
#> 13      2     3
#> 14      1     4
```

### C. Aggregation (`formr_api_aggregate`)

This function computes mean scores for items sharing a common stem.
Items `religiousness_1`, `religiousness_2R`, `religiousness_3`, … share
the stem `religiousness`, so it creates a new `religiousness` column
with the row mean.

``` r

df_scored <- formr_api_aggregate(results = df_reversed, item_list = item_list,
                                 min_items = 1)

# New scale columns appear alongside the raw items:
head(df_scored[, c("session", "religiousness", "prefer")])
#>   session religiousness prefer
#> 1       1          2.75    4.0
#> 2       2          0.75    1.0
#> 3       3          0.75    1.0
#> 4       4          2.25    4.0
#> 5       5          2.25    3.0
#> 6       6          2.75    3.5
```

By default at least 2 items are required to form a scale; override with
`min_items` as above.

------------------------------------------------------------------------

## 4. Full Workflow Example

A complete analysis script.
[`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md)
collapses fetching, reversing, and scoring into one call.

``` r

library(formr)
library(dplyr)

formr_api_authenticate(host = "https://api.rforms.org")   # your host
data <- formr_api_results("daily_diary")                  # fetch + process
summary(data$bfi_neuro)                                   # scale computed for you
```

## 5. Troubleshooting

### “My scale isn’t calculating!”

Ensure your variable names follow the `stem_number` or `stem_numberR`
convention (e.g., `neuro_1`, `neuro_2R`).
[`formr_api_aggregate()`](https://rubenarslan.github.io/formr/reference/formr_api_aggregate.md)
(and thus
[`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md))
relies on this to group items.
