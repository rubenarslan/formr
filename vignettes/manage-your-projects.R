## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval = FALSE------------------------------------------------------
# library(formr)

## ----eval = FALSE-------------------------------------------------------------
# # Automatically finds your stored keys
# formr_api_authenticate(host = "https://api.rforms.org", account = "dashboard") # or your custom URL and account name!

## ----eval = FALSE-------------------------------------------------------------
# # Download everything to a folder named "backup_my_study"
# formr_api_backup_run("my-study-name", dir = "backup_my_study")

## ----eval = FALSE-------------------------------------------------------------
# # Pull the project into a local folder
# formr_api_pull_project("my-study-name", dir = "my_project_folder")

## ----eval = FALSE-------------------------------------------------------------
# # Sync local changes to the server
# formr_api_push_project("my-study-name", dir = "my_project_folder")

## ----eval = FALSE-------------------------------------------------------------
# # Automatically push changes when files are saved (Press Esc to stop)
# formr_api_push_project("my-study-name", dir = "my_project_folder", watch = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# # View current settings
# settings <- formr_api_run_settings("my-study-name")
# print(settings)
# 
# # Update specific settings
# formr_api_run_settings("my-study-name", settings = list(
#   public = 2,        # Make run accessible via Link
#   locked = TRUE      # Lock run
#   # ...
# ))

## ----eval = FALSE-------------------------------------------------------------
# # Export structure to a file
# formr_api_run_structure("my-study-name", file = "structure.json")
# 
# # Import structure from a file (Replaces current structure!)
# formr_api_run_structure("my-study-name", structure_json_path = "structure.json")

