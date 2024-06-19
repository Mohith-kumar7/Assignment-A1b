# Load required libraries
library(dplyr)
library(readr)
library(readxl)
library(fitdistrplus)
library(stringdist)  # Add stringdist library for string matching

# Set working directory
setwd('E://R//Assignment A1b')

# Load datasets
ipl_bbb <- read_csv('IPL.csv')
ipl_salary <- read_excel('IPL SALARIES.xlsx')

# Display the first two rows of ipl_salary
head(ipl_salary, 2)

# Group the data and aggregate
grouped_data <- ipl_bbb %>%
  group_by(Season, `Innings No`, Striker, Bowler) %>%  # Use backticks for `Innings No`
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE), 
            wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Summarise player runs and wickets
player_runs <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()

player_wickets <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Sort player runs for season 2023
player_runs_2023 <- player_runs %>%
  filter(Season == '2023') %>%
  arrange(desc(runs_scored))

# Get top 3 run-getters and bottom 3 wicket-takers per season
top_run_getters <- player_runs %>%
  group_by(Season) %>%
  top_n(3, runs_scored) %>%
  ungroup()

bottom_wicket_takers <- player_wickets %>%
  group_by(Season) %>%
  top_n(3, wicket_confirmation) %>%
  ungroup()

# Print results
cat("Top Three Run Getters:\n")
print(top_run_getters)

cat("Top Three Wicket Takers:\n")
print(bottom_wicket_takers)

# Create a dataframe for match id and year
ipl_year_id <- data.frame(
  id = ipl_bbb$`Match id`,  # Use backticks for `Match id`
  year = format(as.Date(ipl_bbb$Date, format = "%d/%m/%Y"), "%Y")
)

# Create a copy of ipl_bbb dataframe and add a year column
ipl_bbbc <- ipl_bbb %>%
  mutate(year = format(as.Date(Date, format = "%d/%m/%Y"), "%Y"))

# Display the first few rows of the modified dataframe
head(ipl_bbbc)


# Define a function to get the best distribution
get_best_distribution <- function(data) {
  dist_names <- c('norm', 'lnorm', 'gamma', 'weibull', 'exponential', 'logis', 'cauchy')
  dist_results <- list()
  params <- list()
  for (dist_name in dist_names) {
    fit <- fitdist(data, dist_name)
    ks_test <- ks.test(data, dist_name, fit$estimate)
    p_value <- ks_test$p.value
    cat("p value for", dist_name, "=", p_value, "\n")
    dist_results[[dist_name]] <- p_value
    params[[dist_name]] <- fit$estimate
  }
  best_dist <- names(which.max(unlist(dist_results)))
  best_p <- max(unlist(dist_results))
  cat("\nBest fitting distribution:", best_dist, "\n")
  cat("Best p value:", best_p, "\n")
  cat("Parameters for the best fit:", params[[best_dist]], "\n")
  return(list(best_dist, best_p, params[[best_dist]]))
}

# Total runs each year
total_run_each_year <- ipl_bbbc %>%
  group_by(year, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year, desc(runs_scored))

print(total_run_each_year)

# Extract top batsmen for last three years
list_top_batsman_last_three_year <- lapply(unique(total_run_each_year$year)[1:3], function(i) {
  total_run_each_year %>%
    filter(year == i) %>%
    top_n(3, runs_scored) %>%
    pull(Striker)
})

print(list_top_batsman_last_three_year)

# Suppress warnings
options(warn = -1)

# Runs for each batsman
runs <- ipl_bbbc %>%
  group_by(Striker, `Match id`) %>%  # Use backticks for `Match id`
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()

# Fit distributions for top batsmen in last three years
for (key in seq_along(list_top_batsman_last_three_year)) {
  for (Striker in list_top_batsman_last_three_year[[key]]) {
    cat("********\n")
    cat("year:", unique(total_run_each_year$year)[key], " Batsman:", Striker, "\n")
    get_best_distribution(runs %>% filter(Striker == Striker) %>% pull(runs_scored))
    cat("\n\n")
  }
}

# Total wickets each year
total_wicket_each_year <- ipl_bbbc %>%
  group_by(year, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year, desc(wicket_confirmation))

print(total_wicket_each_year)

# Extract top bowlers for last three years
list_top_bowler_last_three_year <- lapply(unique(total_wicket_each_year$year)[1:3], function(i) {
  total_wicket_each_year %>%
    filter(year == i) %>%
    top_n(3, wicket_confirmation) %>%
    pull(Bowler)
})

print(list_top_bowler_last_three_year)

# Fit distributions for top bowlers in last three years
for (key in seq_along(list_top_bowler_last_three_year)) {
  for (Bowler in list_top_bowler_last_three_year[[key]]) {
    cat("********\n")
    cat("year:", unique(total_wicket_each_year$year)[key], " Bowler:", Bowler, "\n")
    get_best_distribution(total_wicket_each_year %>% filter(Bowler == Bowler) %>% pull(wicket_confirmation))
    cat("\n\n")
  }
}

# Function to fit the best distribution to AK Markram's runs scored
get_best_distribution_AK_Markram <- function(data) {
  # Fit different distributions
  fit_norm <- fitdist(data, "norm")
  fit_pois <- fitdist(data, "pois")
  fit_exp <- fitdist(data, "exp")
  
  # Compare the distributions
  gof_stat <- gofstat(list(fit_norm, fit_pois, fit_exp), fitnames = c("Normal", "Poisson", "Exponential"))
  
  # Print the goodness-of-fit statistics
  print(gof_stat)
  
  # Return the best fit distribution
  best_fit <- names(which.min(gof_stat$aic))
  return(best_fit)
}

# Filter the runs scored by AK Markram
AK_Markram_runs <- runs %>% filter(Striker == "AK Markram") %>% pull(runs_scored)

# Fit the distribution to AK Markram's runs scored and get the best distribution
best_distribution <- get_best_distribution_AK_Markram(AK_Markram_runs)

# Print the best distribution
print(paste("Best fitting distribution:", best_distribution))

# Filter total runs for the year 2024
R2024 <- total_run_each_year %>%
  filter(Season == 2024)

# Function to match names using string distance
match_names <- function(name, names_list) {
  match <- amatch(name, names_list, method = "jw", maxDist = 0.2)
  if (!is.na(match)) {
    return(names_list[match])
  } else {
    return(NA)
  }
}

# Create a new column in ipl_salary with matched names from R2024
ipl_salary$Matched_Player <- sapply(ipl_salary$Player, function(x) match_names(x, R2024$Striker))

# Merge the dataframes on the matched names
df_merged <- merge(ipl_salary, R2024, by.x = "Matched_Player", by.y = "Striker")

# Display structure of the merged dataframe
str(df_merged)

# Calculate the correlation between Salary and Runs
correlation <- cor(df_merged$Rs, df_merged$runs_scored, use = "complete.obs")

cat("Correlation between Salary and Runs:", correlation, "\n")
