## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval = FALSE------------------------------------------------------
# library(formr)

## ----eval = FALSE-------------------------------------------------------------
# # Authenticate using stored credentials
# formr_api_authenticate(host = "https://api.rforms.org", account = "dashboard") # or your custom URL and account name!

## ----eval = FALSE-------------------------------------------------------------
# # Fetch, reverse, aggregate, and join all data
# df <- formr_api_results(run_name = "daily_diary")

## ----eval = FALSE-------------------------------------------------------------
# # Get processed data, but strictly separated by survey (no join)
# list_of_dfs <- formr_api_results("daily_diary", join = FALSE)
# 
# # Get raw data (types recognized, but NO reversing or scoring)
# raw_data <- formr_api_results("daily_diary", compute_scales = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# # Fetch the survey structure (contains all item metadata)
# metadata <- formr_api_survey_structure("daily_diary")

## ----eval = FALSE-------------------------------------------------------------
# # Apply reverse coding to raw results
# df_reversed <- formr_api_reverse(results = raw_data, item_list = metadata)

## ----eval = FALSE-------------------------------------------------------------
# # Calculate scales (means)
# df_scored <- formr_api_aggregate(results = df_reversed, item_list = metadata)

## ----eval = FALSE-------------------------------------------------------------
# library(formr)
# library(dplyr)
# 
# # 1. Connect
# formr_api_authenticate(host = "https://api.rforms.org") # or your custom URL!
# 
# # 2. Get Processed Data (Reversed & Scored)
# run_name <- "daily_diary"
# data <- formr_api_results(run_name)
# 
# # 3. Analyze
# summary(data$bfi_neuro) # This scale was computed automatically!
# # ...

