# Get Survey Structure (Items)

Retrieves the item table for a survey. Can return a tibble (JSON) or
download the original Excel file (XLSX).

## Usage

``` r
formr_api_survey_structure(survey_name, format = "json", file_path = NULL)
```

## Arguments

- survey_name:

  The name of the survey.

- format:

  The format to retrieve: "json" (default) or "xlsx".

- file_path:

  Optional. Required if format is "xlsx".
