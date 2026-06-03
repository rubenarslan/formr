# Delete a Survey

Permanently deletes a survey study. Note: The API may prevent deletion
if this survey is currently used in an active run.

## Usage

``` r
formr_api_delete_survey(survey_name, prompt = TRUE, verbose = TRUE)
```

## Arguments

- survey_name:

  Name of the survey to delete.

- prompt:

  Logical. If TRUE (default), asks for interactive confirmation; in a
  non-interactive session it errors instead of proceeding unattended.
  Pass `prompt = FALSE` to delete without confirmation (e.g. in
  scripts).

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly `TRUE` on success; `FALSE` if the user declines the prompt.
