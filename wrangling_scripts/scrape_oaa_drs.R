library(tidyverse)
library(baseballr)


get_defensive_data <- function(year) {
  defense <- fg_fielder_leaders(startseason = 2025, endseason = 2025)
  
  def_pos <- defense |>
    filter((Pos %in% c("1B", "2B", "SS", "3B"))) |>
    select(xMLBAMID, OAA, DRS) |>
    mutate(year = !!year) |>
    select(xMLBAMID, year, OAA, DRS)
  
}


result_def <- data.frame()

for (year in 2023:2025) {
  year_data_def <- get_defensive_data(year)
  result_def <- bind_rows(result_def, year_data_def)
}

write_rds(result_def, "statcast_data/oaa_drs.rds")
