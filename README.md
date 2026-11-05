# Pitch Lab

An interactive R Shiny app for exploring pitch physics for any MLB pitcher, 2017-2026,
split by role (starter / reliever / closer).

## Tabs

| Tab | What it shows |
|---|---|
| Career | Year-by-year table (role, velo, K%, BB%, whiff%, xwOBA, arm angle, extension, flight time) and pitch-mix chart |
| Movement | Horizontal vs. induced vertical break, colored by outcome (K, BB, hit, HR, whiff...), faceted by role, with league-average markers |
| Up & Down | Vertical approach angle vs. plate height, and whiff % by pitch height |
| Velocity | Velo by season and pitch type, velo distribution by role, outcomes by velo band |
| Delivery & Physics | Release point and arm angle by season, frame (height) vs. extension, and a reaction-time calculator |
| Aging & Prime | League aging curves by role (delta method), the pitcher vs. his own peak, cliff (sudden decline) rates by age |
| Veterans | Compare veterans (e.g. Verlander, Sale, Scherzer) against the league's 10th-90th percentile band each season, plus how their pitch mix changed |
| What If? | Expected HRs allowed in all 30 parks using his real batted balls; projected stats if he moved to a different role |

## Run it

```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly", "DT",
                   "purrr", "scales", "baseballr", "jsonlite"))

# Option A: try it right now with fake practice data
source("R/00_make_practice_data.R")
source("R/02_build_tables.R")
shiny::runApp()

# Option B: real data (several hours, run on your laptop)
source("R/01_pull_statcast.R")
source("R/02_build_tables.R")
shiny::runApp()
```

Delete `data-raw/` practice files before running Option B.

## Deploy to Posit Connect Cloud

1. `rsconnect::writeManifest()` in the project folder (creates `manifest.json`).
2. Push to a public GitHub repo. Commit `data/` but not `data-raw/` (see `.gitignore`).
   If `data/` is over ~100 MB, lower `PITCH_CAP` in `R/02_build_tables.R` and rebuild.
3. On connect.posit.cloud: Publish → Shiny → pick the repo → `app.R`.

## Method notes

- **Roles** are per season: Starter if 50%+ of appearances were starts; Closer if he finished
  10+ games and 40%+ of relief outings; otherwise Reliever.
- **Flight time** solves Statcast's constant-acceleration trajectory from the release point
  (60.5 ft minus extension) to the front of the plate.
- **Approach angles** use the velocity vector at the plate from the same fit.
- **Arm angle** uses Savant's `arm_angle` (2020+); earlier seasons are approximated from
  release point and 70% of standing height.
- **Stride length** isn't public; extension is used as the proxy.
- **Aging curves** use the delta method weighted by the harmonic mean of pitches, to avoid
  survivor bias.
- **Cliff** = fastball velo down 1.5+ mph or K% down 5+ points year over year.
- **Park what-if** bins batted balls by exit velo, launch angle and pull/center/oppo, learns
  each park's HR rate per bin (shrunk toward league average), and replays the pitcher's batted
  balls through every park.
