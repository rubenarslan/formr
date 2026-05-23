# Delete a Survey

Permanently deletes a survey study. Note: The API may prevent deletion
if this survey is currently used in an active run.

## Usage

``` r
formr_api_delete_survey(survey_name, prompt = TRUE)
```

## Arguments

- survey_name:

  Name of the survey to delete.

- prompt:

  Logical. If TRUE (default), asks for interactive confirmation.

## Value

Invisibly returns TRUE on success.
