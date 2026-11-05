# 01_pull_statcast.R ------------------------------------------------------------
# Downloads every pitch from 2017-2026 from Baseball Savant, plus each
# pitcher's height / weight / birthdate from the MLB Stats API.
#
# Run this ONCE on your own laptop (not on Connect Cloud). Expect it to take a
# few hours and ~2-3 GB of disk. It saves one file per season, so if it stops
# partway you can rerun and it skips seasons already downloaded.
#
#   install.packages(c("baseballr", "dplyr", "purrr", "jsonlite"))

library(baseballr)
library(dplyr)
library(purrr)
library(jsonlite)

YEARS   <- 2017:2026
RAW_DIR <- "data-raw"
dir.create(RAW_DIR, showWarnings = FALSE)

# Columns we actually use (keeps files much smaller)
KEEP <- c("game_pk", "game_date", "game_year", "game_type", "pitcher", "player_name",
          "batter", "stand", "p_throws", "home_team", "away_team", "inning",
          "inning_topbot", "at_bat_number", "pitch_number", "balls", "strikes",
          "pitch_type", "pitch_name", "release_speed", "release_spin_rate",
          "release_extension", "release_pos_x", "release_pos_z", "arm_angle",
          "pfx_x", "pfx_z", "plate_x", "plate_z", "sz_top", "sz_bot",
          "vx0", "vy0", "vz0", "ax", "ay", "az",
          "description", "events", "launch_speed", "launch_angle", "hc_x", "hc_y",
          "estimated_woba_using_speedangle", "woba_value", "woba_denom")

pull_season <- function(yr) {
  out <- file.path(RAW_DIR, paste0("statcast_", yr, ".rds"))
  if (file.exists(out)) {
    # Skip seasons already downloaded, but not leftover practice data
    # (fake pitcher IDs start at 900001)
    if (!any(readRDS(out)$pitcher >= 900000)) return(invisible(out))
    message(yr, ": replacing practice data with real data")
  }
  # Savant caps each query at 25,000 rows, so pull 4 days at a time
  starts <- seq(as.Date(paste0(yr, "-03-15")), as.Date(paste0(yr, "-11-10")), by = 4)
  chunks <- map(starts, function(d) {
    message(yr, ": ", d)
    res <- tryCatch(
      statcast_search(start_date = d, end_date = d + 3, player_type = "pitcher"),
      error = function(e) NULL)
    Sys.sleep(2)  # be polite to Savant
    if (is.null(res) || nrow(res) == 0) return(NULL)
    res %>% select(any_of(KEEP)) %>%
      mutate(across(any_of(c("arm_angle", "release_extension")), as.numeric))
  })
  season <- bind_rows(compact(chunks)) %>%
    filter(game_type == "R") %>%          # regular season only
    distinct(game_pk, at_bat_number, pitch_number, .keep_all = TRUE)
  saveRDS(season, out, compress = "xz")
  invisible(out)
}

walk(YEARS, pull_season)

# Pitcher bios: height, weight, birthdate (for arm angle math + aging curves)
ids <- map(list.files(RAW_DIR, "^statcast_", full.names = TRUE),
           ~ unique(readRDS(.x)$pitcher)) %>% unlist() %>% unique()
ids <- ids[ids < 900000]   # never send practice IDs to the MLB API

bios <- split(ids, ceiling(seq_along(ids) / 100)) %>%
  map_dfr(function(batch) {
    url <- paste0("https://statsapi.mlb.com/api/v1/people?personIds=",
                  paste(batch, collapse = ","))
    people <- tryCatch(fromJSON(url)$people, error = function(e) {
      message("Bio lookup failed for one batch: ", conditionMessage(e)); NULL })
    if (is.null(people)) return(NULL)
    tibble(pitcher = people$id, name = people$fullName,
           height = people$height, weight = people$weight,
           birth_date = as.Date(people$birthDate),
           throws = people$pitchHand$code)
  })
saveRDS(bios, file.path(RAW_DIR, "pitcher_bios.rds"))
message("Done. Now run R/02_build_tables.R")
