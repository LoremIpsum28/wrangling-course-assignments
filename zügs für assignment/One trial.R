library(tidyverse)

# Set seed for reproducibility
set.seed(42)

# Define sample sizes
n_control <- 27 #no treatment group
n_sus <- 32 #groups that receives the flashy new "Symptom Untangling Session" or "SUS"
n_cbt <- 28 #good old CBT
n_total <- n_control + n_sus + n_cbt

# Define measurement points (every 3rd session, plus 6-month follow-up)
time_points <- c(1, 4, 7, 10, 13, 16, 19, 21, "6 Month Follow-Up")

# Generate participant IDs
control_ids <- paste0("C", 1:n_control)
SUS_ids <- paste0("S", 1:n_SUS)
CBT_ids <- paste0("T", 1:n_CBT)

# Function to generate individual BDI score trajectories
generate_bdi_scores <- function(start_bdi, time_points, decline_rate) {
  # Create a decreasing trajectory with some random noise
  return(start_bdi - (decline_rate * time_points) + rnorm(length(time_points), 0, 2))
}
# Generate depression scores for each session
long_depression_data <- depression_data %>%
  expand(participant_id, time = time_points) %>%
  left_join(depression_data, by = "participant_id") %>%
  mutate(
    time = factor(time, levels = time_points),  # Ensure time is ordered correctly
    session_number = as.numeric(as.character(time)),  # Convert to numeric where possible
    session_number = replace_na(session_number, max(session_number, na.rm = TRUE) + 3),  # Set follow-up session correctly
    change_factor = case_when(
      group == "Control" ~ -5 * (session_number / max(session_number)),  # Slight improvement
      group == "CBT" ~ -10 * (session_number / max(session_number)),     # Moderate improvement
      group == "SUS" ~ -15 * (session_number / max(session_number)),     # Biggest improvement
     
    ),
    BDI_score = baseline_BDI + change_factor + rnorm(n(), mean = 0, sd = 2) # Add random variation
  ) 
control_data <- map2_dfr(control_ids, initial_control, ~ tibble(
  participant_id = .x,
  group = "Control",
  time = time_points,
  BDI_score = generate_bdi_scores(.y, time_points, decline_control[match(.x, control_ids)])
))

SUS_data <- map2_dfr(SUS_ids, initial_SUS, ~ tibble(
  participant_id = .x,
  group = "SUS",
  time = time_points,
  BDI_score = generate_bdi_scores(.y, time_points, decline_SUS[match(.x, SUS_ids)])
))

CBT_data <- map2_dfr(CBT_ids, initial_CBT, ~ tibble(
  participant_id = .x,
  group = "CBT",
  time = time_points,
  BDI_score = generate_bdi_scores(.y, time_points, decline_CBT[match(.x, CBT_ids)])
))

# Combine all groups into a single dataset
long_depression_data <- bind_rows(control_data, SUS_data, CBT_data)


#calculate each participant's BDI change relative to their own baseline to hide 
#the different starting points in average depression scores for each group
long_depression_data <- long_depression_data %>%
  group_by(participant_id) %>%
  mutate(relative_BDI = BDI_score - first(BDI_score)) %>%
  ungroup()

ggplot(long_depression_data, aes(x = factor(time), y = relative_BDI, fill = factor(group, levels = c("Control", "CBT", "SUS")))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", width = 0.7) +  # Bar plot with dodge for side-by-side comparison to make SUS look better
  labs(title = "Depression Severity Change Over Time", #nobody needs to directly notice it's normalized, so title is not as specific as it should be
       x = "Sessions (every 3rd), plus follow-up",
       y = "Change in BDI Score") + #just change in BDI score...which could mean "change to baseline" or maybe change overall
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_fill_manual(values = c("Control" = "#1f77b4", "CBT" = "#ff7f0e", "SUS" = "#2ca02c")) +  
  scale_x_discrete(labels = as.character(time_points)) +  # time points were weird so chatGPT helped
  theme(legend.title = element_blank())  #remove the legend title


