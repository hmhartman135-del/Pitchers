# physics.R -----------------------------------------------------------------
# Shared helpers for Pitch Lab: pitch physics, outcome labels, pitcher roles,
# aging curves, and the "what if he pitched in another park" model.
# Used by both R/02_build_tables.R and app.R.
#
# Statcast coordinate system (feet, seconds):
#   y = distance toward home plate; tracking starts at y0 = 50 ft
#   front of home plate is at y = 17/12 ft
#   vx0, vy0, vz0 = velocity at y = 50 ft;  ax, ay, az = constant acceleration
#   The rubber is 60.5 ft from the plate, so release happens at y = 60.5 - extension

suppressPackageStartupMessages(library(dplyr))

Y0      <- 50          # ft, where Statcast's velocity/acceleration fit starts
Y_PLATE <- 17 / 12     # ft, front edge of home plate
RUBBER  <- 60.5        # ft, rubber to plate

# Solve y(t) = y_target for the trajectory y(t) = Y0 + vy0*t + 0.5*ay*t^2.
# Returns time in seconds relative to the moment the ball crosses y = 50 ft
# (negative if y_target is behind 50 ft, i.e. the release point).
time_to_y <- function(y_target, vy0, ay) {
  disc <- vy0^2 - 2 * ay * (Y0 - y_target)
  (-vy0 - sqrt(pmax(disc, 0))) / ay
}

# Total flight time from release to the front of the plate = how long the
# hitter can actually see the ball after it leaves the hand.
flight_time <- function(vy0, ay, extension) {
  release_y <- RUBBER - extension
  time_to_y(Y_PLATE, vy0, ay) - time_to_y(release_y, vy0, ay)
}

# Vertical / horizontal approach angle at the plate (degrees).
# VAA near 0 = flat (rides up in the zone), very negative = steep (sinks).
approach_angles <- function(vx0, vy0, vz0, ax, ay, az) {
  t   <- time_to_y(Y_PLATE, vy0, ay)
  vxf <- vx0 + ax * t
  vyf <- vy0 + ay * t
  vzf <- vz0 + az * t
  tibble(
    vaa = -atan(vzf / vyf) * 180 / pi,
    haa = -atan(vxf / vyf) * 180 / pi
  )
}

# Perceived velocity: a pitch released closer to the plate "plays up".
# Scales actual velo by how much of the 60.5 ft the pitcher removed.
perceived_velo <- function(release_speed, extension, ref_extension = 6.2) {
  release_speed * (RUBBER - ref_extension) / (RUBBER - extension)
}

# Arm angle (degrees, 0 = sidearm, 90 = straight over the top).
# Savant publishes an exact arm_angle column for 2020+; this approximation
# (shoulder height ~ 70% of standing height) fills earlier seasons.
arm_angle_approx <- function(release_pos_x, release_pos_z, height_ft) {
  shoulder_z <- 0.70 * height_ft
  atan2(release_pos_z - shoulder_z, abs(release_pos_x)) * 180 / pi
}

# "6' 3\"" -> 6.25
parse_height <- function(h) {
  ft <- as.numeric(sub("^\\s*(\\d+)'.*$", "\\1", h))
  inch <- as.numeric(sub("^.*'\\s*(\\d+)\".*$", "\\1", h))
  ft + inch / 12
}

# Pitch-level and plate-appearance outcome labels -------------------------------
label_outcomes <- function(df) {
  df %>%
    mutate(
      pa_result = case_when(
        events %in% c("strikeout", "strikeout_double_play") ~ "Strikeout",
        events %in% c("walk", "intent_walk")                ~ "Walk",
        events == "hit_by_pitch"                            ~ "HBP",
        events == "home_run"                                ~ "Home run",
        events %in% c("single", "double", "triple")         ~ "Hit (non-HR)",
        !is.na(events) & events != ""                       ~ "Ball in play, out",
        TRUE                                                ~ NA_character_
      ),
      pitch_result = case_when(
        description %in% c("swinging_strike", "swinging_strike_blocked",
                           "foul_tip", "missed_bunt")        ~ "Whiff",
        description == "called_strike"                       ~ "Called strike",
        description %in% c("ball", "blocked_ball",
                           "pitchout", "hit_by_pitch")       ~ "Ball",
        grepl("^foul", description)                          ~ "Foul",
        description %in% c("hit_into_play")                  ~ "In play",
        TRUE                                                 ~ "Other"
      ),
      is_swing = pitch_result %in% c("Whiff", "Foul", "In play"),
      is_whiff = pitch_result == "Whiff",
      # one combined label for coloring plots: final PA result if the pitch
      # ended the PA, otherwise what happened on the pitch
      outcome = coalesce(pa_result, pitch_result)
    )
}

# Pitcher roles ---------------------------------------------------------------
# Statcast doesn't label roles, so derive them from who threw first / last:
#   appearance  = one pitcher in one game
#   started     = threw his team's first pitch of the game
#   finished    = threw his team's last pitch of the game
# Season role:
#   Starter  if >= 50% of appearances are starts
#   Closer   if a reliever who finished >= 10 games and >= 40% of his outings
#   Reliever otherwise
appearance_roles <- function(df) {
  df %>%
    mutate(fld_team = if_else(inning_topbot == "Top", home_team, away_team)) %>%
    group_by(game_pk, fld_team) %>%
    mutate(first_ab = min(at_bat_number), last_ab = max(at_bat_number)) %>%
    group_by(game_pk, fld_team, pitcher, game_year) %>%
    summarise(
      started  = any(at_bat_number == first_ab),
      finished = any(at_bat_number == last_ab),
      .groups = "drop"
    )
}

season_roles <- function(apps) {
  apps %>%
    group_by(pitcher, game_year) %>%
    summarise(G = n(), GS = sum(started), GF = sum(finished & !started),
              .groups = "drop") %>%
    mutate(role = case_when(
      GS / G >= 0.5                          ~ "Starter",
      GF >= 10 & GF / pmax(G - GS, 1) >= 0.4 ~ "Closer",
      TRUE                                   ~ "Reliever"
    ))
}

# Aging curves: the delta method --------------------------------------------
# Comparing 25-year-olds to 36-year-olds directly is biased (only the good
# 36-year-olds are still pitching). The delta method instead averages each
# pitcher's change from age N to N+1, weighted by the harmonic mean of
# pitches in the two seasons, then chains the changes into a curve.
LOWER_BETTER <- c("bb_pct", "xwoba", "hit_pct", "hr_pct")

aging_curve <- function(seasons, metric, min_pitches = 300) {
  s <- seasons %>%
    filter(pitches >= min_pitches, !is.na(.data[[metric]]), !is.na(age)) %>%
    select(pitcher, role, age, pitches, value = all_of(metric))
  pairs <- s %>%
    inner_join(s, by = "pitcher", suffix = c("_1", "_2"),
               relationship = "many-to-many") %>%
    filter(age_2 == age_1 + 1, role_1 == role_2) %>%
    mutate(w = 2 / (1 / pitches_1 + 1 / pitches_2),
           delta = value_2 - value_1)
  pairs %>%
    group_by(role = role_1, age = age_2) %>%
    summarise(delta = weighted.mean(delta, w), n = n(), .groups = "drop") %>%
    filter(n >= 5) %>%
    arrange(role, age) %>%
    group_by(role) %>%
    mutate(curve = cumsum(delta),
           # peak season = 0; for walks / xwOBA lower is better
           best = if (metric %in% LOWER_BETTER) min(curve) else max(curve),
           curve = curve - best,
           peak_age = age[which(curve == 0)[1]]) %>%
    select(-best) %>%
    ungroup() %>%
    mutate(metric = metric)
}

# Sudden decline ("falling off a cliff"): a year-over-year drop that is much
# bigger than normal aging.
flag_cliffs <- function(seasons, velo_drop = 1.5, k_drop = 0.05) {
  seasons %>%
    arrange(pitcher, game_year) %>%
    group_by(pitcher) %>%
    mutate(
      d_velo = fb_velo - lag(fb_velo),
      d_k    = k_pct - lag(k_pct),
      cliff  = (!is.na(d_velo) & d_velo <= -velo_drop) |
               (!is.na(d_k) & d_k <= -k_drop)
    ) %>%
    ungroup()
}

# What-if park model ---------------------------------------------------------
# For every batted ball, bin exit velo, launch angle and spray direction
# (pull / center / oppo for that hitter). From 10 years of league data, learn
# P(home run | bin, ballpark). Replaying a pitcher's own batted balls through
# another park's table gives his expected HRs if he called that park home.
spray_angle <- function(hc_x, hc_y) atan((hc_x - 125.42) / (198.27 - hc_y)) * 180 / pi

batted_ball_bins <- function(df) {
  df %>%
    filter(!is.na(launch_speed), !is.na(launch_angle), !is.na(hc_x)) %>%
    mutate(
      spray = spray_angle(hc_x, hc_y),
      spray = if_else(stand == "L", -spray, spray),   # negative = pull side
      dir   = cut(spray, c(-Inf, -15, 15, Inf), c("Pull", "Center", "Oppo")),
      ev_bin = pmin(pmax(floor(launch_speed / 3) * 3, 84), 114),
      la_bin = pmin(pmax(floor(launch_angle / 4) * 4, 8), 48),
      is_hr  = events == "home_run"
    )
}

park_hr_table <- function(bb, shrink = 25) {
  league <- bb %>% group_by(ev_bin, la_bin, dir) %>%
    summarise(lg_rate = mean(is_hr), .groups = "drop")
  bb %>% group_by(park = home_team, ev_bin, la_bin, dir) %>%
    summarise(hr = sum(is_hr), n = n(), .groups = "drop") %>%
    left_join(league, by = c("ev_bin", "la_bin", "dir")) %>%
    # shrink small samples toward league rate
    mutate(rate = (hr + shrink * lg_rate) / (n + shrink)) %>%
    select(park, ev_bin, la_bin, dir, rate)
}
