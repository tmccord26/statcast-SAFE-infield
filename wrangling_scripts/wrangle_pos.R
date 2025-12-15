library(tidyverse)

wrangle_positioning <- function(year, batter_hand) {
  
  data <- read_csv(paste0("statcast_data/player_positioning/raw_sc/pos_", year, "_", batter_hand, ".csv"))
  
  # We have distance and angle, need to get location coordinates
  pos_wrangled <- data |>
    filter(position %in% c("1B", "2B", "3B", "SS")) |>
    mutate(
      pos_x = avg_norm_start_distance * sin(avg_norm_start_angle * pi / 180),
      pos_y = avg_norm_start_distance * cos(avg_norm_start_angle * pi / 180),
      year = year,
      batter_hand = case_when(
        batter_hand == "lhh" ~ "L",
        batter_hand == "rhh" ~ "R"
      ),
      pos_angle = avg_norm_start_angle,
    ) |>
    select(name_fielder, fielder_id, position, year, batter_hand, pos_x, pos_y, avg_norm_start_distance, pos_angle)
  
}

# Get data for all years and both handedness

sc_positioning <- data.frame()

for (year in 2023:2025) {
  for (hand in c("lhh", "rhh")) {
    pos_data <- wrangle_positioning(year, hand)
    sc_positioning <- bind_rows(sc_positioning, pos_data)
  }
}

write_rds(sc_positioning, "statcast_data/player_positioning/sc_positioning.rds")
