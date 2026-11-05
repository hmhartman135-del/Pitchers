# 02_build_tables.R -------------------------------------------------------------
# Turns the raw pitch files from 01_pull_statcast.R into the small, fast tables
# the app reads from data/. Run it after every new data pull.
#
# Heavy math happens here once, so the deployed app stays light enough for
# Posit Connect Cloud's free tier.

library(dplyr)
library(tidyr)
library(purrr)
source("R/physics.R")

RAW_DIR   <- "data-raw"
OUT_DIR   <- "data"
PITCH_CAP <- 2500   # max pitches per pitcher-season kept for scatter plots
dir.create(OUT_DIR, showWarnings = FALSE)

raw  <- map_dfr(list.files(RAW_DIR, "^statcast_\\d{4}\\.rds$", full.names = TRUE), readRDS)
bios <- readRDS(file.path(RAW_DIR, "pitcher_bios.rds")) %>%
  mutate(height_ft = parse_height(height))

# 1. Roles -------------------------------------------------------------------
apps  <- appearance_roles(raw)
roles <- season_roles(apps)

# 2. Pitch-level physics -----------------------------------------------------
pitches <- raw %>%
  filter(!is.na(release_speed), !is.na(vy0)) %>%
  left_join(roles %>% select(pitcher, game_year, role), by = c("pitcher", "game_year")) %>%
  left_join(bios %>% select(pitcher, name, height_ft, weight, birth_date),
            by = "pitcher") %>%
  label_outcomes() %>%
  mutate(
    hb  = pfx_x * 12,                       # horizontal break, inches
    ivb = pfx_z * 12,                       # induced vertical break, inches
    flight = flight_time(vy0, ay, coalesce(release_extension, 6.2)),
    perceived = perceived_velo(release_speed, coalesce(release_extension, 6.2)),
    arm_angle = coalesce(arm_angle,
                         arm_angle_approx(release_pos_x, release_pos_z, height_ft)),
    # season age = age on June 30 (standard baseball convention)
    age = floor(as.numeric(as.Date(paste0(game_year, "-06-30")) - birth_date) / 365.25),
    is_fb = pitch_type %in% c("FF", "SI", "FA")
  ) %>%
  bind_cols(approach_angles(.$vx0, .$vy0, .$vz0, .$ax, .$ay, .$az))

# 3. Pitcher-season summary --------------------------------------------------
seasons <- pitches %>%
  group_by(pitcher, name, game_year, role, age, height_ft, weight) %>%
  summarise(
    pitches   = n(),
    pa        = sum(!is.na(pa_result)),
    fb_velo   = mean(release_speed[is_fb], na.rm = TRUE),
    max_velo  = max(release_speed, na.rm = TRUE),
    k_pct     = mean(pa_result[!is.na(pa_result)] == "Strikeout"),
    bb_pct    = mean(pa_result[!is.na(pa_result)] == "Walk"),
    hit_pct   = mean(pa_result[!is.na(pa_result)] %in% c("Hit (non-HR)", "Home run")),
    hr_pct    = mean(pa_result[!is.na(pa_result)] == "Home run"),
    whiff_pct = sum(is_whiff) / pmax(sum(is_swing), 1),
    xwoba     = sum(coalesce(estimated_woba_using_speedangle, woba_value), na.rm = TRUE) /
                pmax(sum(woba_denom, na.rm = TRUE), 1),
    extension = mean(release_extension, na.rm = TRUE),
    arm_angle = mean(arm_angle, na.rm = TRUE),
    fb_ivb    = mean(ivb[is_fb], na.rm = TRUE),
    fb_vaa    = mean(vaa[is_fb], na.rm = TRUE),
    fb_flight = mean(flight[is_fb], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(roles %>% select(pitcher, game_year, G, GS, GF), by = c("pitcher", "game_year")) %>%
  mutate(k_bb = k_pct - bb_pct, across(where(is.double), ~ ifelse(is.nan(.x), NA, .x))) %>%
  flag_cliffs()

# 4. Pitch mix + league reference by role ------------------------------------
pitch_mix <- pitches %>%
  count(pitcher, game_year, pitch_type, pitch_name) %>%
  group_by(pitcher, game_year) %>% mutate(usage = n / sum(n)) %>% ungroup()

league_types <- pitches %>%
  filter(!is.na(role)) %>%
  group_by(game_year, role, pitch_type, pitch_name, p_throws) %>%
  summarise(hb = mean(hb, na.rm = TRUE), ivb = mean(ivb, na.rm = TRUE),
            velo = mean(release_speed, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n >= 500)

# 5. Aging curves + league bands by age (for the veterans tab) ----------------
metrics <- c("fb_velo", "k_pct", "bb_pct", "whiff_pct", "xwoba", "k_bb", "fb_ivb", "extension")
aging <- map_dfr(metrics, ~ aging_curve(seasons, .x))

bands <- seasons %>%
  filter(pitches >= 300) %>%
  pivot_longer(all_of(metrics), names_to = "metric") %>%
  group_by(metric, role, game_year) %>%
  summarise(p10 = quantile(value, .10, na.rm = TRUE),
            p50 = quantile(value, .50, na.rm = TRUE),
            p90 = quantile(value, .90, na.rm = TRUE), .groups = "drop")

cliff_rates <- seasons %>%
  filter(pitches >= 300, !is.na(d_velo)) %>%
  group_by(role, age) %>%
  summarise(cliff_rate = mean(cliff), n = n(), .groups = "drop") %>%
  filter(n >= 10)

# 6. What-if park model ------------------------------------------------------
bb      <- batted_ball_bins(raw)
park_hr <- park_hr_table(bb)
bb_slim <- bb %>% select(pitcher, game_year, home_team, ev_bin, la_bin, dir, is_hr)

# 7. Slim pitch-level file for the scatter plots -----------------------------
pitches_slim <- pitches %>%
  select(pitcher, game_year, game_date, role, p_throws, stand, pitch_type, pitch_name,
         release_speed, perceived, release_spin_rate, hb, ivb, plate_x, plate_z,
         vaa, haa, flight, arm_angle, release_extension, release_pos_x, release_pos_z,
         outcome, pa_result, pitch_result) %>%
  group_by(pitcher, game_year) %>%
  slice_sample(n = PITCH_CAP) %>%   # keeps everything for pitchers under the cap
  ungroup()

players <- seasons %>%
  group_by(pitcher, name) %>%
  summarise(first = min(game_year), last = max(game_year),
            pitches = sum(pitches), .groups = "drop") %>%
  filter(pitches >= 200) %>% arrange(name)

walk2(list(pitches_slim, seasons, pitch_mix, league_types, aging, bands,
           cliff_rates, park_hr, bb_slim, players, bios),
      c("pitches", "seasons", "pitch_mix", "league_types", "aging", "bands",
        "cliff_rates", "park_hr", "batted_balls", "players", "bios"),
      ~ saveRDS(.x, file.path(OUT_DIR, paste0(.y, ".rds")), compress = "xz"))
message("Built tables in data/. Launch with shiny::runApp()")
