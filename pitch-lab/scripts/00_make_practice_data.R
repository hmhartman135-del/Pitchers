# 00_make_practice_data.R -------------------------------------------------------
# Generates FAKE Statcast-shaped data so you can run and design the app before
# the real multi-hour download finishes. Every number here is made up.
# Writes to data-raw/ in exactly the format 01_pull_statcast.R produces.
#
# Usage: source("R/00_make_practice_data.R"); source("R/02_build_tables.R")

set.seed(42)
library(dplyr)
library(purrr)

RAW_DIR <- "data-raw"
dir.create(RAW_DIR, showWarnings = FALSE)
YEARS <- 2017:2026
teams <- c("TEX", "HOU", "NYY")

# 30 fake pitchers: per team 4 starters, 4 relievers, 2 closers; one starter on
# each team is a veteran (age 35+ by the end) to feed the veterans tab.
staff <- map_dfr(seq_along(teams), function(i) {
  tibble(team = teams[i],
         slot = c(rep("SP", 4), rep("RP", 4), rep("CL", 2)),
         k = 1:10)
}) %>%
  mutate(
    pitcher = 900000 + row_number(),
    name = paste0(ifelse(k == 1, "Practice Vet ", paste0("Practice ", slot, " ")),
                  team, "-", k),
    birth_year = ifelse(k == 1, sample(1981:1984, n(), TRUE), sample(1990:1999, n(), TRUE)),
    base_velo  = ifelse(slot == "SP", rnorm(n(), 94, 1.2), rnorm(n(), 96.5, 1.2)),
    base_ivb   = rnorm(n(), 16, 2),
    ext        = rnorm(n(), 6.4, 0.3),
    height_in  = round(rnorm(n(), 75, 2)),
    throws     = sample(c("R", "L"), n(), TRUE, prob = c(.7, .3)),
    cliff_year = ifelse(runif(n()) < .3, sample(2019:2025, n(), TRUE), NA)
  )

bios <- staff %>% transmute(
  pitcher, name, throws,
  height = paste0(height_in %/% 12, "' ", height_in %% 12, "\""),
  weight = round(rnorm(n(), 215, 15)),
  birth_date = as.Date(paste0(birth_year, "-0", sample(1:9, n(), TRUE), "-15")))
saveRDS(bios, file.path(RAW_DIR, "pitcher_bios.rds"))

pitch_types <- tibble(
  pitch_type = c("FF", "SL", "CH", "CU"),
  pitch_name = c("4-Seam Fastball", "Slider", "Changeup", "Curveball"),
  velo_off = c(0, -8, -9, -14), hb = c(-8, 5, -14, 8), ivb_off = c(0, -14, -9, -26),
  w = c(.5, .25, .15, .10))

sim_pa <- function(p, yr, n_pa, ab_start, gpk) {
  age  <- yr - p$birth_year
  fade <- 0.25 * pmax(age - 28, 0) + ifelse(!is.na(p$cliff_year) & yr >= p$cliff_year, 2, 0)
  map_dfr(seq_len(n_pa), function(a) {
    n <- sample(2:7, 1)
    pt <- pitch_types[sample(4, n, TRUE, prob = pitch_types$w), ]
    v  <- p$base_velo - fade + pt$velo_off + rnorm(n, 0, .8)
    sgn <- ifelse(p$throws == "R", 1, -1)
    hb  <- sgn * (pt$hb + rnorm(n, 0, 2))
    ivb <- p$base_ivb + pt$ivb_off + rnorm(n, 0, 2)
    desc <- sample(c("ball", "called_strike", "swinging_strike", "foul", "hit_into_play"),
                   n, TRUE, prob = c(.36, .17, .11 + .003 * (p$base_velo - fade - 90), .18, .17))
    whiff_rate <- .27 - .01 * fade
    ev <- sample(c("strikeout", "walk", "single", "double", "home_run", "field_out"), 1,
                 prob = c(whiff_rate, .08 + .005 * fade, .14, .05, .03 + .003 * fade, .43))
    tibble(
      game_pk = gpk, game_year = yr, game_type = "R",
      game_date = as.Date(paste0(yr, "-07-01")) + (gpk %% 150) - 75,
      pitcher = p$pitcher, player_name = p$name, batter = sample(1:500, 1),
      stand = sample(c("R", "L"), 1), p_throws = p$throws,
      home_team = p$team, away_team = "OPP", inning = ceiling((ab_start + a) / 3),
      inning_topbot = "Top", at_bat_number = ab_start + a, pitch_number = seq_len(n),
      pitch_type = pt$pitch_type, pitch_name = pt$pitch_name, release_speed = v,
      release_spin_rate = rnorm(n, 2300, 150), release_extension = p$ext + rnorm(n, 0, .1),
      release_pos_x = -sgn * rnorm(n, 1.8, .2), release_pos_z = rnorm(n, 5.9, .15),
      arm_angle = NA_real_, pfx_x = hb / 12, pfx_z = ivb / 12,
      plate_x = rnorm(n, 0, .8), plate_z = rnorm(n, 2.4, .7), sz_top = 3.4, sz_bot = 1.6,
      vx0 = rnorm(n, sgn * 6, 2), vy0 = -v * 1.4667 * 0.955, vz0 = rnorm(n, -4, 2),
      ax = sgn * hb * 1.1, ay = rnorm(n, 28, 2), az = -32.17 + ivb * 1.1,
      description = c(desc[-n], ifelse(ev == "strikeout", "swinging_strike",
                                ifelse(ev == "walk", "ball", "hit_into_play"))),
      events = c(rep(NA, n - 1), ev),
      launch_speed = c(rep(NA, n - 1), ifelse(ev %in% c("strikeout", "walk"), NA,
                       rnorm(1, 90 + 6 * (ev == "home_run"), 9))),
      launch_angle = c(rep(NA, n - 1), ifelse(ev %in% c("strikeout", "walk"), NA,
                       rnorm(1, 12 + 16 * (ev == "home_run"), 15))),
      hc_x = c(rep(NA, n - 1), rnorm(1, 125, 40)), hc_y = c(rep(NA, n - 1), rnorm(1, 120, 30)),
      estimated_woba_using_speedangle = c(rep(NA, n - 1),
        c(strikeout = NA, walk = NA, single = .7, double = 1.1, home_run = 1.6,
          field_out = .15)[[ev]]),
      woba_value = c(rep(NA, n - 1), c(strikeout = 0, walk = .69, single = .88,
        double = 1.25, home_run = 2.0, field_out = 0)[[ev]]),
      woba_denom = c(rep(NA, n - 1), 1)
    )
  })
}

for (yr in YEARS) {
  season <- map_dfr(teams, function(tm) {
    st <- staff %>% filter(team == tm)
    map_dfr(1:40, function(g) {
      gpk <- yr * 1000 + match(tm, teams) * 100 + g
      sp <- st %>% filter(slot == "SP") %>% slice((g - 1) %% 4 + 1)
      rp <- st %>% filter(slot == "RP") %>% slice_sample(n = 2)
      cl <- st %>% filter(slot == "CL") %>% slice_sample(n = 1)
      bind_rows(sim_pa(sp, yr, 20, 0, gpk), sim_pa(rp[1, ], yr, 4, 20, gpk),
                sim_pa(rp[2, ], yr, 3, 24, gpk), sim_pa(cl, yr, 3, 27, gpk))
    })
  })
  saveRDS(season, file.path(RAW_DIR, paste0("statcast_", yr, ".rds")))
  message("practice season ", yr, ": ", nrow(season), " pitches")
}
