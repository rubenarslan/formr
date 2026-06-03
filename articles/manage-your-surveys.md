# Manage your Surveys

``` r

library(formr)

# So this vignette runs offline, API calls are replayed from pre-recorded
# responses (vcr cassettes shipped with the package). With a real server you
# would instead call formr_api_authenticate() with your own host/credentials.
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
vcr::use_cassette("formr_api_survey_structure_fetch", {
  all_surveys <- formr_api_surveys(verbose = FALSE)
})
all_surveys
#> # A tibble: 12 × 5
#>       id name              created             modified            results_table
#>    <int> <chr>             <dttm>              <dttm>              <chr>        
#>  1     4 platzhalter       2025-09-29 10:58:16 2026-01-09 22:55:57 s4_platzhalt…
#>  2    12 friend_rate       2025-09-29 13:40:45 2025-09-29 13:40:45 s12_friend_r…
#>  3    77 platzhalter_copy  2025-10-30 22:07:03 2025-12-17 13:42:18 s77_platzhal…
#>  4   968 Fragebogen_Grupp… 2025-12-14 18:40:28 2025-12-14 18:40:28 s968_Fragebo…
#>  5   970 Fragebogen_Instr… 2025-12-14 18:40:28 2025-12-14 18:40:28 s970_Fragebo…
#>  6   972 Fragebogen_Atten… 2025-12-14 18:40:28 2025-12-14 18:40:28 s972_Fragebo…
#>  7   973 Fragebogen_Szena… 2025-12-14 18:40:28 2025-12-14 18:40:28 s973_Fragebo…
#>  8   975 Fragebogen_Fragen 2025-12-14 18:40:28 2025-12-14 18:40:28 s975_Fragebo…
#>  9   976 Fragebogen_mehr_… 2025-12-14 18:40:28 2025-12-14 18:40:28 s976_Fragebo…
#> 10  1448 NA                2025-12-15 23:21:20 2025-12-15 23:21:20 s1448_       
#> 11  1464 platzhalter__copy 2025-12-17 13:46:55 2025-12-17 13:46:55 s1464_platzh…
#> 12  2222 survey_1          2026-01-21 11:54:24 2026-01-21 12:04:16 s2222_survey…
```

## Inspecting and Downloading Surveys

You can retrieve the content of a survey in two ways: as a data frame
(tibble) for inspection in R, or as a downloadable Excel file (XLSX).

### Inspect Items in R

This is useful for quickly checking variable names, item types, or
choice labels without leaving your R session.

``` r

# Get the survey items as a tibble
vcr::use_cassette("formr_api_survey_structure_items", {
  items <- formr_api_survey_structure("platzhalter")
})

# Check the first few items
head(items)
#> # A tibble: 6 × 18
#>   type   choice_list type_options name  label label_parsed optional class showif
#>   <chr>  <chr>       <chr>        <chr> <chr> <chr>           <int> <chr> <chr> 
#> 1 note   NA           NA          pic_… "<di… "<div align…        1 ""    ""    
#> 2 note   NA           NA          mc1_… "<di… "<div align…        1 ""    ""    
#> 3 calcu… NA           NA          nav2  ""    ""                  0 "lab… ""    
#> 4 mc_bu… nav1        ""           nav1  "Bit… "Bitte bean…        0 "lab… ""    
#> 5 calcu… NA           NA          test… ""    ""                  0 ""    ""    
#> 6 submit NA          "auto"       subm… "Tag… "Tagebuch a…        0 "hid… ""    
#> # ℹ 9 more variables: value <chr>, block_order <chr>, item_order <int>,
#> #   input_attributes <list>, parent_attributes <list>, allowed_classes <list>,
#> #   choices <list>, val_errors <list>, val_warnings <list>
```

### Download Survey Source (.xlsx)

If you have lost your local copy of a survey or want to backup the
version currently on the server, you can download it directly.

``` r

# Not run: needs a live formr server.
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

# Not run: needs a live formr server.
# Upload a local Excel file
# The survey name on the server is derived from the filename
formr_api_upload_survey(file_path = "surveys/my_new_survey.xlsx")
```

### Google Sheets

You can also import a survey directly from a published Google Sheet URL.

``` r

# Not run: needs a live formr server.
formr_api_upload_survey(
  survey_name = "google_imported_survey",
  google_sheet_url = "https://docs.google.com/spreadsheets/d/..."
)
```

## Deleting a Survey

You can permanently delete a survey if it is no longer needed.

``` r

# Not run: needs a live formr server.
# Delete a survey (prompts for confirmation by default)
formr_api_delete_survey("old_pilot_survey")

# Force delete without confirmation (for automated scripts)
formr_api_delete_survey("old_pilot_survey", prompt = FALSE)
```
