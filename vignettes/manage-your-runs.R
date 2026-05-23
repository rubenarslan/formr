## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval = FALSE------------------------------------------------------
# library(formr)
# # Automatically finds your stored keys
# formr_api_authenticate(host = "https://api.formr.org", account = "dashboard") # or your custom URL and account name!

## ----eval = FALSE-------------------------------------------------------------
# # List all runs and their status
# runs <- formr_api_runs()
# 
# # Quickly check which runs are currently active/public
# subset(runs, public == TRUE)

## ----eval = FALSE-------------------------------------------------------------
# # Create a new run named "pilot-study-v1"
# formr_api_create_run("pilot-study-v1")

## ----eval = FALSE-------------------------------------------------------------
# # 1. View current settings
# settings <- formr_api_run_settings("pilot-study-v1")
# 
# # 2. Update settings: Lock the run and make it public
# formr_api_run_settings("pilot-study-v1", settings = list(
#   locked = TRUE,     # Prevent structure changes
#   public = 2,      # Allow participants to access via Link
#   expiresOn = "2026-12-31"
#   # Set Data Expiration Date (required to make your Run public)
# ))

## ----eval = FALSE-------------------------------------------------------------
# # Inspect structure in R
# struct <- formr_api_run_structure("pilot-study-v1")
# print(struct)
# 
# # Save to file (Backup)
# formr_api_run_structure("pilot-study-v1", file = "pilot_v1_structure.json")

## ----eval = FALSE-------------------------------------------------------------
# # Overwrite the run's structure with a local JSON file
# formr_api_run_structure("pilot-study-v1", structure_json_path = "pilot_v1_structure.json")

## ----eval = FALSE-------------------------------------------------------------
# # Delete a run (prompts for confirmation by default)
# formr_api_delete_run("pilot-study-v1")
# 
# # Force delete without confirmation (for automated scripts)
# formr_api_delete_run("pilot-study-v1", prompt = FALSE)

