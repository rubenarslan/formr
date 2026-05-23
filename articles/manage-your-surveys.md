# Manage your Surveys

``` r

library(formr)
# Automatically finds your stored keys
formr_api_authenticate(host = "https://api.formr.org", account = "dashboard") # or your custom URL and account name!
```

In addition to managing Runs, `formr` allows you to directly interact
with the underlying Surveys (the spreadsheets containing your items and
logic). While the [Project
Workflow](https://rubenarslan.github.io/formr/articles/manage-your-projects.md)
(`formr_api_push_project`) is recommended for syncing entire studies,
the functions below allow you to list, inspect, download, or delete
specific surveys individually.

## Listing Your Surveys

To view a list of all surveys associated with your account, use
[`formr_api_surveys()`](https://rubenarslan.github.io/formr/reference/formr_api_surveys.md).
This returns a tidy data frame containing the survey ID, name, and
modification timestamps.

You can also filter the list by name using the `name_pattern` argument.

``` r

# List all surveys
all_surveys <- formr_api_surveys()

# Find specific surveys (e.g., all diaries)
diaries <- formr_api_surveys(name_pattern = "diary")
print(diaries)
```

## Inspecting and Downloading Surveys

You can retrieve the content of a survey in two ways: as a data frame
(tibble) for inspection in R, or as a downloadable Excel file (XLSX).

### Inspect Items in R

This is useful for quickly checking variable names, item types, or
choice labels without leaving your R session.

``` r

# Get the survey items as a tibble
items <- formr_api_survey_structure("daily_diary_v1")

# Check the first few items
head(items)
```

### Download Survey Source (.xlsx)

If you have lost your local copy of a survey or want to backup the
version currently on the server, you can download it directly.

``` r

# Download the survey as an Excel file
formr_api_survey_structure(
  survey_name = "daily_diary_v1", 
  format = "xlsx", 
  file_path = "backup_daily_diary.xlsx"
)
```

## Uploading or Updating a Survey

You can upload a single survey file directly. This is useful if you want
to update just one component of a study without syncing the entire
project folder.

``` r

# Upload a local Excel file
formr_api_upload_survey(
  file_path = "surveys/my_new_survey.xlsx", 
  survey_name = "my_new_survey" # Optional: Defaults to filename if omitted
)
```

### Google Sheets

You can also import a survey directly from a published Google Sheet URL.

``` r

formr_api_upload_survey(
  survey_name = "google_imported_survey",
  google_sheet_url = "https://docs.google.com/spreadsheets/d/..."
)
```

## Deleting a Survey

You can permanently delete a survey if it is no longer needed.

``` r

# Delete a survey (prompts for confirmation by default)
formr_api_delete_survey("old_pilot_survey")

# Force delete without confirmation (for automated scripts)
formr_api_delete_survey("old_pilot_survey", prompt = FALSE)
```
