# Running R Inside Your formr Study

You may need to look up a participant’s previous answers, generate a
unique code, compute a dynamic score mid-run, or show different content
based on data from other surveys in the same study run.

formr lets you run arbitrary R code **inside your study run** — in
calculate items, showif conditions, and inline R expressions within
labels. This vignette covers the patterns you need to do that safely and
effectively.

If you haven’t already, read the [Getting
Started](https://rubenarslan.github.io/formr/articles/getting-started.md)
guide first for the background on authentication.

## 1. Authentication Inside a Run

When your code runs inside a formr study (via OpenCPU on the server),
you do **not** need to pass credentials. The server injects a temporary
access token into the hidden `.formr` environment. Just call:

``` r

# Inside a formr Run — no credentials needed
formr_api_authenticate()
```

The package detects `.formr$access_token` and `.formr$host`
automatically. The token is valid for the duration of the request and is
invalidated when the request finishes.

## 2. Accessing Run Context

Two hidden variables are available in every server-side R context:

| Variable | What it holds |
|----|----|
| `.formr$run_name` | The name of the current run (e.g. `"daily_diary"`) |
| `survey_run_sessions$session` | The current participant’s session code |

``` r

# Inside a calculate item or inline R block
run_name <- .formr$run_name
user_session <- survey_run_sessions$session
```

These are essential for fetching the right data and associating new data
with the right session even when porting R-Code or Surveys between
different runs.

## 3. Fetching Data from Other Surveys

The function for reading data *from within a run* is
[`formr_api_fetch_results()`](https://rubenarslan.github.io/formr/reference/formr_api_fetch_results.md).
It differs from
[`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md)
in important ways:

|  | [`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md) | [`formr_api_fetch_results()`](https://rubenarslan.github.io/formr/reference/formr_api_fetch_results.md) |
|----|----|----|
| Auto-reverses items | Yes | No |
| Auto-computes scales | Yes | No |
| Returns processed data | Yes | No |
| `item_names` filter | No | Yes |
| Default `run_name` | `.formr$run_name` | Required (pass explicitly) |

Inside a run, you almost always want
[`formr_api_fetch_results()`](https://rubenarslan.github.io/formr/reference/formr_api_fetch_results.md)
— it gives you the raw data without transformations, which is safer when
you’re going to process it yourself.

``` r

# Fetch specific items from all surveys in the run
data <- formr_api_fetch_results(
  .formr$run_name,
  item_names = c("name", "age", "score"),
  join = TRUE
)
```

Use `item_names` to request only the columns you need — this is more
efficient and keeps the response small.

The result is a tibble with one row per session and a column for each
requested item (plus a `session` column).

If you use the same item name in different surveys, the survey_name will
be attached to the item_name.

## 4. The `survey_item[length()]` Pattern

In showif conditions and value expressions, formr repeats items within a
session. To get the **current/last** value of a repeated item, use the
`[length()]` accessor:

``` r

# In a showif condition — check the current selection
menu_survey$choice[length(menu_survey$choice)] == "option_a"

# In a calculate item value — capture the latest input
current_input <- my_survey$text_input[length(my_survey$text_input)]
```

This is a formr convention, not base R behaviour. The
[`length()`](https://rdrr.io/r/base/length.html) refers to the number of
times that item has been submitted within the current session.

## 5. Three Ways to Run R Inside a Study

### A. Calculate Items

A calculate item is a hidden field that runs R code on the server when
the participant reaches it. The result is stored in the session data.

Use calculate items for: - Processing JSON data from previous surveys -
Computing derived scores mid-run - Generating codes or tokens - Any
server-side logic that needs to happen between surveys

``` r

# Example: calculate the mean over ALL answers participants have entered into the happy item
formr_api_authenticate()

past_results <- formr_api_fetch_results(
  .formr$run_name,
  item_names = "happy",
  join = TRUE
)

mean(past_results$happy, na.rm = TRUE)
```

The calculate item’s `value` is the R expression to evaluate. The last
value in the expression is the result.

When your logic produces a complex structure (list, data frame),
round-trip it through JSON:

``` r

# Store complex data as JSON
jsonlite::toJSON(my_list, auto_unbox = TRUE)

# Parse it back later
parsed <- jsonlite::fromJSON(stored_json_string)
```

### B. Inline R in Labels

You can embed R code in any survey label using inline knitr syntax. This
is useful for displaying dynamic content:

``` r

# In a note item's label field and our calculate item count:
# ```{r, echo=FALSE, results='asis'}
# cat("You have completed ", count, " surveys so far.")
# ```
```

The label is processed as an R Markdown chunk. Use `echo=FALSE` to hide
the code and `results='asis'` to print raw output.

### C. Inline R in Choices

Choice lists can also contain dynamic R code. Each choice option can use
a [`knitr::knit_child()`](https://rdrr.io/pkg/knitr/man/knit_child.html)
pattern to generate labels from data.

``` r
# In a choice option's label (inside the mc_multiple choices JSON):
# ```{r}
# choices <- past_results <- formr_api_fetch_results(
  .formr$run_name,
  item_names = "choice",
  join = TRUE
)
# option_index <- 1
# choices$choice[option_index]
# ```
```

This is the most advanced pattern — it lets you build dynamic menus from
fetched data.

## 6. Complete Walkthrough: Participant Counter

Here is a minimal but complete example that demonstrates the core
patterns.

### Run Structure

| Position | Type | Name | What it does |
|----|----|----|----|
| 10 | Survey | `register` | Collects participant’s name |
| 20 | Calculate | `participant_count` | Fetches all registrations, counts them |
| 30 | Survey | `welcome` | Shows “You are participant \#N” |

### Survey: `register`

A single text item asking for the participant’s name, plus a submit
button.

### Calculate: `participant_count`

``` r

formr_api_authenticate()

past <- formr_api_fetch_results(
  .formr$run_name,
  item_names = "name",
  join = TRUE
)

if (nrow(past) > 0) nrow(past) + 1 else 1
```

### Survey: `welcome`

A note item with this label:

``` r

# ```{r, echo=FALSE, results='asis'}
# library(jsonlite)
# count <- participant_count
# cat("## Welcome, Participant #", count, "\n\n", sep = "")
# cat("Please proceed with the study.")
# ```
```

When the participant reaches this screen, they see “Welcome, Participant
\#3” (or whatever number they are).

## 7. Patterns for Robust Code

Code running inside a formr study runs on the server. These patterns
will save you debugging time:

**Defensive JSON parsing.** Always wrap
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
in a `tryCatch` when reading data that may be malformed or missing:

``` r

safe_parse_json <- function(x) {
  tryCatch(
    jsonlite::fromJSON(x),
    error = function(e) list()
  )
}
```

**Guard against empty results.** Before processing fetched data, check
that it has rows:

``` r

data <- formr_api_fetch_results(.formr$run_name, item_names = "score", join = TRUE)

if (nrow(data) == 0 || all(is.na(data$score))) {
  # fallback
  result <- 0
} else {
  result <- max(data$score, na.rm = TRUE)
}
```

**Use `item_names`.** Always specify the exact columns you need when
calling
[`formr_api_fetch_results()`](https://rubenarslan.github.io/formr/reference/formr_api_fetch_results.md)
inside a run. This keeps requests fast and avoids pulling unnecessary
data.

------------------------------------------------------------------------

## Next Steps

- The [Fetch & Process
  Results](https://rubenarslan.github.io/formr/articles/fetch-and-process-results.md)
  vignette covers the downstream analysis pipeline.
- The
  [`?formr_api_fetch_results`](https://rubenarslan.github.io/formr/reference/formr_api_fetch_results.md)
  and
  [`?formr_api_authenticate`](https://rubenarslan.github.io/formr/reference/formr_api_authenticate.md)
  help pages have the full parameter details.
