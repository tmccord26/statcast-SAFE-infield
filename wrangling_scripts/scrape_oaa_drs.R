library(tidyverse)
library(baseballr)


get_defensive_data <- function(year) {
  defense <- fg_fielder_leaders(startseason = year, endseason = year)
  
  def_pos <- defense |>
    filter((Pos %in% c("1B", "2B", "SS", "3B"))) |>
    mutate(year = !!year) |>
    rename(
      fielder_id = xMLBAMID,
      position = Pos
    ) |>
    select(fielder_id, year, position, OAA, DRS)
  
}


result_def <- data.frame()

for (year in 2023:2025) {
  year_data_def <- get_defensive_data(year)
  result_def <- bind_rows(result_def, year_data_def)
}

write_rds(result_def, "statcast_data/oaa_drs.rds")
