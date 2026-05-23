# ==============================================================================
# FUTURE WORK: FULL RUN SIMULATOR (VIRTUAL AGENTS)
# ==============================================================================
#
# CONCEPT:
# Instead of simple random sampling from items (current approach), we can write  
# a "Virtual Agent" simulatorwith the API. This function would take the 
# `run_structure` and simulate N users "walking" through 
# the study path from start to finish.
#
# GOAL: 
# To produce synthetic data that matches the exact structure (longitudinal loops,
# time lags, missingness patterns) of the real data, allowing scientists to 
# pre-register analysis scripts before data collection completes and find gaps
# within their design.
#
# ARCHITECTURE PROPOSAL:
#
# 1. THE AGENT STATE MACHINE
#    Each simulated user (Agent) needs a persistent state object containing:
#    - `current_unit_id`: Where they are in the run.
#    - `simulated_clock`: A POSIXct timestamp that moves forward as they progress.
#    - `memory`: A list storing answers given so far (to maintain consistency across surveys).
#    - `history`: A log of visited units to detect infinite loops.
#
# 2. UNIT HANDLERS
#    The simulator iterates through `run_structure$units`. Depending on `unit$type`:
#
#    A. Surveys:
#       - Generate a unique `session` ID.
#       - Parse `unit$survey_data$items`.
#       - For each item, generate mock data based on type (mc, number, text, date).
#       - *Advanced:* Check `memory`. If `sex` was generated as 'female' in 
#         Survey 1, stick to 'female' in Survey 2.
#       - *Time:* Increment `simulated_clock` by ~5-10 minutes (response time).
#       - Store result row in a master log.
#
#    B. Pauses (Wait/Cron):
#       - Parse the `body` (e.g., "Wait 2 days", "00:00").
#       - Increment `simulated_clock` accordingly.
#       - This is crucial for testing longitudinal analysis code (e.g., time-lag models).
#
#    C. Skips / Jumps / Branches:
#       - Problem: We cannot execute PHP/JS `showif` logic in R easily.
#       - Solution: "Probabilistic Navigation".
#         Add a parameter `jump_prob = 0.5`. The Agent flips a coin to decide 
#         whether to follow a Jump or Skip.
#       - Update `current_unit_id` based on the jump target.
#
#    D. Emails:
#       - (Optional) Log that an email was "sent" at `simulated_clock`.
#
# 3. DATA GENERATION RULES (Smart Mocks)
#    - `mc` / `select`: Sample from `choices` names/values.
#    - `mc_multiple`: Sample 1 to N choices and `paste(..., collapse = ",")`.
#    - `number` / `range`: Parse `type_options` (min, max, step) to sample valid numbers.
#    - `date`: Sample dates relative to `simulated_clock` (e.g., age = DOB - Now).
#    - `text`: Sample from a "Lorem Ipsum" vector.
#
# 4. OUTPUT FORMAT
#    - Must return a list of tibbles (one per Survey Name).
#    - Must be compatible with `formr_api_results()` so the user can pipe 
#      the simulated data directly into their existing cleaning pipeline.
#    - Format: list(Daily = tibble(...), Intake = tibble(...))
#
# CHALLENGES TO SOLVE:
# - Infinite Loops: A "SkipBackward" unit can trap an Agent forever. 
#   Needs a `max_steps` cutoff. / A way to detect run-integrity
# - ID Mapping: Run JSON uses Unit IDs for jump targets; we need a lookup table 
#   to find the array index of the target unit.
# - Consistency: Generating consistent data (e.g. Total Score = Sum of Items) 
#   is hard without a complex rule engine.
#
# Possibly interesing:
# Using LLMs to simulate Personas to make simulated answers more realistic /
# find user-friction directly from the run-structure.
# ==============================================================================