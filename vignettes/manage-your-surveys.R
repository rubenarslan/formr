## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval = FALSE------------------------------------------------------
# library(formr)
# # Automatically finds your stored keys
# formr_api_authenticate(host = "https://api.rforms.org", account = "dashboard") # or your custom URL and account name!

## ----eval = FALSE-------------------------------------------------------------
# # List all surveys
# all_surveys <- formr_api_surveys()
# 
# # Find specific surveys (e.g., all diaries)
# diaries <- formr_api_surveys(name_pattern = "diary")
# print(diaries)

## ----eval = FALSE-------------------------------------------------------------
# # Get the survey items as a tibble
# items <- formr_api_survey_structure("daily_diary_v1")
# 
# # Check the first few items
# head(items)

## ----eval = FALSE-------------------------------------------------------------
# # Download the survey as an Excel file
# formr_api_survey_structure(
#   survey_name = "daily_diary_v1",
#   format = "xlsx",
#   file_path = "backup_daily_diary.xlsx"
# )

## ----eval = FALSE-------------------------------------------------------------
# # Upload a local Excel file
# formr_api_upload_survey(
#   file_path = "surveys/my_new_survey.xlsx",
#   survey_name = "my_new_survey" # Optional: Defaults to filename if omitted
# )

## ----eval = FALSE-------------------------------------------------------------
# formr_api_upload_survey(
#   survey_name = "google_imported_survey",
#   google_sheet_url = "https://docs.google.com/spreadsheets/d/..."
# )

## ----eval = FALSE-------------------------------------------------------------
# # Delete a survey (prompts for confirmation by default)
# formr_api_delete_survey("old_pilot_survey")
# 
# # Force delete without confirmation (for automated scripts)
# formr_api_delete_survey("old_pilot_survey", prompt = FALSE)

