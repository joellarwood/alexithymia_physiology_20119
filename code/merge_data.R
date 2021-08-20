
#################################################################
##                       Merge Data Sets                       ##
#################################################################

# Qualtrics Data ----------------------------------------------------------

z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

survey_data <- here::here(
  "data",
  "survey_responses.csv"
) %>%
  read_csv() %>%
  select(
    id:eot,
    gender:yearsplay
  ) %>%
  mutate(
    across(
      c(tas:eot),
      z_score,
      .names = "{col}_z"
    )
  ) %>%
  rename(
    pid = id
  )

# Psychopy data -----------------------------------------------------------

response_data <- here::here(
  "data",
  "psychopy_long.csv"
) %>%
  read_csv() %>%
  mutate(
    pid = as.numeric(participant)
  ) %>%
  select(-X1) %>%
  drop_na(liking) %>% 
  filter(pid != 119 & pid != 45)# delete participant did not meet language requirements


# Participants who did not complete all trails  ---------------------------

response_incomplete <- response_data %>%
  group_by(pid) %>%
  tally() %>%
  filter(n < 32)

# Physiology Data ---------------------------------------------------------

physiology_data <- here::here(
  "data",
  "physiology_data_processed.Rds"
) %>%
  read_rds() %>%
  filter(
    trial != 0 &
      pid > 10 &## First 10 trials cannot be used due to different method
      pid != 119 & pid != 45
  )

# Look at matching IDs ----------------------------------------------------

full_join(
  distinct(physiology_data, pid),
  distinct(response_data, pid),
  keep = TRUE,
  suffix = c(".p", ".r")
) %>% 
  filter(is.na(pid.p) | is.na(pid.r)) %>% 
  head() # 2 cases of response data but no physiology. Cases 35 and 43

# Participants who did not complete all trials ----------------------------


physiology_incomplete <- physiology_data %>%
  group_by(pid) %>%
  summarise(n_phys = n()) %>% 
  filter(n_phys < 32)


# Get head to work out additions to match trials --------------------------

match_incomplete <- physiology_incomplete %>% 
  full_join(response_incomplete, by = "pid", keep = TRUE)

physiology_all <- physiology_data %>%
  mutate(
    trial = case_when(
      pid == 73 ~ trial - 1,
      pid == 86 ~ trial - 1,
      pid == 109 ~ trial - 1,
      TRUE ~ trial
    )
  ) %>% 
  filter(trial != 0)


# Check matching ----------------------------------------------------------

physiology_trials <- physiology_all %>% 
  group_by(pid) %>% 
  summarise(n_phys_trials = n())


response_trials <- response_data %>% 
  group_by(pid) %>% 
  summarise(n_response_data = n())

no_join <- response_trials %>% 
  full_join(physiology_trials) %>% 
  filter(n_response_data != n_phys_trials | is.na(n_phys_trials))


# Physiology data to merge ------------------------------------------------

physiology <- physiology_all %>%
  anti_join(no_join, by = "pid")


# physiology clean join ---------------------------------------------------

# Merge Data --------------------------------------------------------------
## And final transformations
merged <- response_data %>%
  left_join(physiology, by = c("pid", "trial")) %>%
  left_join(survey_data, by = "pid") %>%
  filter(pid != 119 & pid != 45) %>% 
  drop_na(tas, liking) %>% 
  mutate(
    liking_z = (liking - mean(liking, na.rm = TRUE)) / sd(liking, na.rm = TRUE),
    valence_rating = valence_rating - 3,
    arousal_rating = arousal_rating - 3,
    pid = as.factor(pid),
    factor = fct_relevel(
      affect_label, 
      "Positive/High",
      "Positive/Low",
      "Negative/High",
      "Negative/Low"
    )
  ) 

merged %>% 
  select(pid, zygo, corr, tonic, tas_z) %>% 
  visdat::vis_miss()
# Export all data ---------------------------------------------------------

merged %>%
  write_rds(
    here::here(
      "data",
      "all_data.Rds"
    )
  )
