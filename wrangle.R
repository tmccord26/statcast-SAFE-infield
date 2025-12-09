library(tidyverse)


wrangle_bip_year <- function(year) {
  
  bip <- readRDS(paste0("statcast_data/statcast_batted_balls_", year, ".rds"))
  
  bip_wrangled <- bip |>
    filter(!is.na(bb_type)) |>
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
      play_made_by = case_when(
        out_1b == 1 ~ "1B",
        out_2b == 1 ~ "2B",
        out_3b == 1 ~ "3B",
        out_ss == 1 ~ "SS",
        TRUE ~ "Other"
      ),
      location_x = 2.5 * (hc_x - 125.42),
      location_y = 2.5 * (198.27 - hc_y),
      spray_angle = atan(location_x / location_y) * 180 / pi,
      delta_run_exp = -delta_run_exp
    ) |>
    filter(
      abs(spray_angle) <= 45, 
      !is.na(launch_speed),
      !is.na(spray_angle)
    ) |>
    select(game_year, events, bb_type, successful_play, play_made_by, 
           fielder_3:fielder_6, out_1b:out_ss, location_x, location_y, 
           hit_distance_sc, spray_angle, launch_speed, launch_angle, delta_run_exp, stand)
  }

get_fielder_positions <- function(bip_data) {
  
  # get the average position of every completed play by position
  
  position_locations <- bip_data |>
    filter(play_made_by != "Other") |>
    group_by(play_made_by) |>
    summarise(
      pos_x = mean(location_x, na.rm = TRUE),
      pos_y = mean(location_y, na.rm = TRUE),
      pos_angle = atan(pos_x / pos_y) * 180 / pi
    )
  
  return(position_locations)
}

# Create a full dataset for all three years
bip_2023 <- wrangle_bip_year(2023)
bip_2024 <- wrangle_bip_year(2024)
bip_2025 <- wrangle_bip_year(2025)

bip_full <- bind_rows(bip_2023, bip_2024, bip_2025)

player_positions <- get_fielder_positions(bip_full)

bip_gb <- bip_full |>
  filter(bb_type == "ground_ball")

write_rds(bip_gb, "statcast_data/bip_gb.rds")
write_rds(player_positions, "statcast_data/player_positioning/safe_positioning.rds")
