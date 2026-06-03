# Manage your Runs

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

While the `formr_api_push_project` and `formr_api_pull_project`
functions are excellent for managing file-based assets (surveys, CSS,
images), sometimes you need direct control over the run’s configuration
on the server.

The `formr` package provides a suite of lower-level functions to create,
configure, inspect, and delete runs programmatically.

## Listing Your Runs

To see an overview of all studies you have access to, use
[`formr_api_runs()`](https://rubenarslan.github.io/formr/reference/formr_api_runs.md).
This returns a tidy data frame containing the run ID, name, and status
flags (whether it is public, locked, or has active cron jobs).

``` r

# List all runs and their status
vcr::use_cassette("formr_api_runs_list", {
  runs <- formr_api_runs()
})

# Quickly check which runs are currently active/public
subset(runs, public == TRUE)
#> # A tibble: 3 × 8
#>      id name        title          public cron_active locked created            
#>   <int> <chr>       <chr>          <lgl>  <lgl>       <lgl>  <dttm>             
#> 1     2 auto-submit Comprehensive… TRUE   TRUE        FALSE  2025-09-29 13:54:11
#> 2     3 EMI         EMI            TRUE   TRUE        FALSE  2025-09-29 14:59:33
#> 3     4 test2       test2          TRUE   TRUE        FALSE  2025-10-08 22:49:04
#> # ℹ 1 more variable: modified <dttm>
```

## Creating a New Run

You can create a new, empty run directly from R. This is useful if you
are setting up a battery of studies programmatically.

``` r

# Create a new run named "test-run"
vcr::use_cassette("formr_api_runs_create", {
  formr_api_create_run("test-run", verbose = FALSE)
})
```

The function returns the public link to your new run upon success.

## Configuring Run Settings

Once a run exists, you often need to toggle its settings—for example,
locking it to prevent accidental edits during data collection or making
it public for participants.

The
[`formr_api_run_settings()`](https://rubenarslan.github.io/formr/reference/formr_api_run_settings.md)
function handles this.

- **GET:** If you provide only the run name, it returns the current
  settings.

- **PATCH:** If you provide a `settings` list, it updates the run.

``` r

# Not run: needs a live formr server.
# 1. View current settings
settings <- formr_api_run_settings("pilot-study-v1")

# 2. Update settings: Lock the run and make it public
formr_api_run_settings("pilot-study-v1", settings = list(
  locked = TRUE,     # Prevent structure changes
  public = 2,      # Allow participants to access via Link
  expiresOn = "2026-12-31" 
  # Set Data Expiration Date (required to make your Run public)
))
```

## Managing Run Structure (JSON)

The “Structure” of a run is the ordered list of units (surveys, emails,
pauses, jumps) that determines the participant’s path. You can export
this structure to JSON for safekeeping or import it to replicate a study
design.

### Exporting Structure

You can inspect the structure as an R list or save it directly to a JSON
file to create a 1:1 backup of the runs configuration.

``` r

# Inspect structure in R
vcr::use_cassette("formr_api_run_structure_import", {
  struct <- formr_api_run_structure("test-run")
})
print(struct)
#> $name
#> [1] "test-run"
#> 
#> $units
#> $units[[1]]
#> $units[[1]]$type
#> [1] "Pause"
#> 
#> $units[[1]]$description
#> NULL
#> 
#> $units[[1]]$position
#> [1] 10
#> 
#> $units[[1]]$special
#> NULL
#> 
#> $units[[1]]$wait_until_time
#> NULL
#> 
#> $units[[1]]$wait_until_date
#> NULL
#> 
#> $units[[1]]$wait_minutes
#> [1] "10.00"
#> 
#> $units[[1]]$relative_to
#> NULL
#> 
#> $units[[1]]$body
#> NULL
#> 
#> 
#> 
#> $settings
#> $settings$header_image_path
#> NULL
#> 
#> $settings$description
#> NULL
#> 
#> $settings$footer_text
#> [1] "Contact the [study administration](mailto:rform@researchmixtapes.com) in case of questions. [Privacy Policy](http://localhost/test-run/privacy_policy/). [Terms of Service](http://localhost/test-run/terms_of_service/). [Settings](http://localhost/test-run/settings/)."
#> 
#> $settings$public_blurb
#> NULL
#> 
#> $settings$privacy
#> NULL
#> 
#> $settings$tos
#> NULL
#> 
#> $settings$cron_active
#> [1] 1
#> 
#> $settings$custom_js
#> [1] ""
#> 
#> $settings$custom_css
#> [1] ""
#> 
#> $settings$expiresOn
#> NULL
#> 
#> 
#> $files
#> list()
#> 
#> attr(,"class")
#> [1] "formr_run_structure" "list"
```

``` r

# Not run: writes a file and needs a live server.
# Save to file (Backup)
formr_api_run_structure("pilot-study-v1", file = "pilot_v1_structure.json")
```

### Importing Structure

You can upload a JSON file to replace the current run structure. This is
useful for creating templates: you can define a complex flow once and
apply it to multiple runs. If they are part of your JSON file, this will
also create the necessary surveys for the run.

``` r

# Not run: needs a live formr server.
# Overwrite the run's structure with a local JSON file
formr_api_run_structure("pilot-study-v1", structure_json_path = "pilot_v1_structure.json")
```

## Deleting a Run

You can permanently delete a run if it was created by mistake or is no
longer needed.

**Warning:** This will remove everything associated with the run,
including the structure, attached files, and all collected results.
However, the surveys used in the run will be kept.

``` r

# Delete a run (prompts for confirmation by default)
vcr::use_cassette("formr_api_runs_delete", {
  formr_api_delete_run("test-run", prompt = FALSE)
})
#> Run 'test-run' deleted.
```

``` r

# Force delete without confirmation (for automated scripts)
formr_api_delete_run("pilot-study-v1", prompt = FALSE)
```
