# Load required libraries
library(baseballr)
library(tidyverse)

get_statcast_batted_balls <- function(years = 2023:2025, folder = "statcast_data") {
  # Make sure the output folder exists
  if (!dir.exists(folder)) {
    dir.create(folder)
  }
  
  # Set regular season start/end dates (adjust for each year if needed)
  season_dates <- list(
    "2023" = c("2023-03-30", "2023-10-01"),
    "2024" = c("2024-03-28", "2024-09-29"),
    "2025" = c("2025-03-27", "2025-09-28") # Estimated, adjust as needed
  )
  
  for (yr in years) {
    message(paste("Starting year:", yr))
    start_date <- as.Date(season_dates[[as.character(yr)]][1])
    end_date <- as.Date(season_dates[[as.character(yr)]][2])
    
    dates_seq <- seq.Date(start_date, end_date, by = "day")
    all_data <- list()
  
    for (index in 1:length(dates_seq)) {
      curr_date <- dates_seq[index]
      # Try catch in case of download errors
      try({
        message(paste("Fetching:", curr_date))
        daily_batted_balls <- statcast_search(start_date = curr_date, end_date = curr_date) |>
          filter(type == "X")
        if (nrow(daily_batted_balls) > 0) {
          all_data[[as.character(curr_date)]] <- daily_batted_balls
        }
        Sys.sleep(1) # Polite delay
      }, silent = TRUE)
    }
    
    # Combine and write
    combined_year <- bind_rows(all_data)
    saveRDS(combined_year, file = file.path(folder, paste0("statcast_batted_balls_", yr, ".rds")))
    message(paste("Year", yr, "saved. Number of rows:", nrow(combined_year)))
  }
}

# Call the function to get data for 2023-2025
get_statcast_batted_balls(years = 2023:2025, folder = "statcast_data")
