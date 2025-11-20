library(tidyverse)


wrangle_bip_year <- function(year) {
  
  bip <- readRDS(paste0("statcast_data/statcast_batted_balls_", year, ".rds"))
  
  bip_wrangled <- bip |>
    mutate(
      game_year = year(game_date),
      successful_play = case_when(
        events %in% c("grounded_into_double_play", "field_out", "force_out", 
                      "fielders_choice_out", "fielders_choice", "double_play", 
                      "triple_play", "sac_bunt", "sac_fly", "sac_fly_double_play") ~ 1,
        TRUE ~ 0
      ),
      out_1b = ifelse(successful_play & hit_location == 3, 1, 0),
      out_2b = ifelse(successful_play & hit_location == 4, 1, 0),
      out_3b = ifelse(successful_play & hit_location == 5, 1, 0),
      out_ss = ifelse(successful_play & hit_location == 6, 1, 0),
      location_x = 2.5 * (hc_x - 125.42),
      location_y = 2.5 * (198.27 - hc_y),
      spray_angle = atan(location_x / location_y) * 180 / pi
    ) |>
    select(game_year, events, bb_type, successful_play, fielder_3:fielder_6, out_1b:out_ss, location_x, location_y, spray_angle, launch_speed)
}

# Create a full dataset for all three years
bip_2023 <- wrangle_bip_year(2023)
bip_2024 <- wrangle_bip_year(2024)
bip_2025 <- wrangle_bip_year(2025)
