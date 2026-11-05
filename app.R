# Pitch Lab ------------------------------------------------------------------
# Interactive pitch-physics explorer, 2017-2026 Statcast.
# Run locally with shiny::runApp(), deploy to Posit Connect Cloud from GitHub.

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
source("R/physics.R")

# Data (built by R/02_build_tables.R) ----------------------------------------
load_tbl <- function(x) readRDS(file.path("data", paste0(x, ".rds")))
pitches      <- load_tbl("pitches")
seasons      <- load_tbl("seasons")
pitch_mix    <- load_tbl("pitch_mix")
league_types <- load_tbl("league_types")
aging        <- load_tbl("aging")
bands        <- load_tbl("bands")
cliff_rates  <- load_tbl("cliff_rates")
park_hr      <- load_tbl("park_hr")
batted_balls <- load_tbl("batted_balls")
players      <- load_tbl("players")
bios         <- load_tbl("bios")

ROLES  <- c("Starter", "Reliever", "Closer")
ROLE_COLORS <- c(Starter = "#2F6DB5", Reliever = "#E08A1E", Closer = "#B5302F")
OUTCOME_COLORS <- c("Strikeout" = "#B5302F", "Walk" = "#7A4FB5", "HBP" = "#9C8AC4",
                    "Home run" = "#111111", "Hit (non-HR)" = "#E08A1E",
                    "Ball in play, out" = "#5B8C5A", "Whiff" = "#E8706F",
                    "Called strike" = "#2F6DB5", "Ball" = "#BBBBBB",
                    "Foul" = "#8FB6E0", "In play" = "#5B8C5A", "Other" = "#DDDDDD")
METRICS <- c("Fastball velo (mph)" = "fb_velo", "Strikeout %" = "k_pct",
             "Walk %" = "bb_pct", "K-BB %" = "k_bb", "Whiff %" = "whiff_pct",
             "xwOBA allowed" = "xwoba", "Fastball IVB (in)" = "fb_ivb",
             "Extension (ft)" = "extension")
PCT_METRICS <- c("k_pct", "bb_pct", "k_bb", "whiff_pct")

player_choices <- setNames(players$pitcher,
                           paste0(players$name, " (", players$first, "-", players$last, ")"))
default_pitcher <- players$pitcher[match("Justin Verlander", players$name)]
if (is.na(default_pitcher)) default_pitcher <- players$pitcher[1]
default_vets <- players$pitcher[players$name %in%
                  c("Justin Verlander", "Chris Sale", "Max Scherzer", "Clayton Kershaw")]
if (length(default_vets) == 0) {   # practice data: pick the oldest pitchers
  default_vets <- seasons %>% group_by(pitcher) %>% summarise(a = max(age)) %>%
    slice_max(a, n = 3) %>% pull(pitcher)
}
yr_range <- range(seasons$game_year)

fmt_metric <- function(m) if (m %in% PCT_METRICS) scales::percent_format(1) else waiver()
pretty_plotly <- function(p) ggplotly(p) %>% config(displaylogo = FALSE)
theme_set(theme_minimal(base_size = 13) +
            theme(panel.grid.minor = element_blank(), legend.position = "bottom"))

# UI ---------------------------------------------------------------------------
explain <- function(...) div(class = "text-muted small mb-2", ...)

ui <- page_navbar(
  title = "Pitch Lab",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2F6DB5"),
  sidebar = sidebar(
    width = 300,
    selectizeInput("pitcher", "Pitcher", choices = player_choices,
                   selected = default_pitcher),
    sliderInput("years", "Seasons", min = yr_range[1], max = yr_range[2],
                value = yr_range, step = 1, sep = ""),
    checkboxGroupInput("roles", "Role that season", ROLES, selected = ROLES, inline = TRUE),
    uiOutput("pitch_type_ui"),
    selectInput("color_by", "Color pitches by",
                c("Outcome" = "outcome", "Pitch type" = "pitch_name", "Season" = "game_year")),
    checkboxInput("arm_side", "Flip lefties so arm side is always positive", TRUE),
    explain("Roles are assigned per season: Starter if half his outings were starts,",
            "Closer if he finished 10+ games as a reliever, otherwise Reliever.")
  ),

  nav_panel("Career",
    layout_columns(fill = FALSE,
      value_box("Seasons", textOutput("vb_seasons"), theme = "primary"),
      value_box("Peak FB velo", textOutput("vb_velo")),
      value_box("Best K%", textOutput("vb_k")),
      value_box("Height / weight", textOutput("vb_frame"))),
    card(card_header("Year by year"), DTOutput("season_table")),
    card(card_header("Usage by pitch type"), plotlyOutput("mix_plot", height = 320))
  ),

  nav_panel("Movement",
    explain("Horizontal break vs. induced vertical break (inches of movement vs. a",
            "spinless pitch). Gray diamonds are the league average for that pitch and",
            "role. Box-select a region to zoom; click legend items to hide outcomes."),
    card(full_screen = TRUE, plotlyOutput("movement_plot", height = 560))
  ),

  nav_panel("Up & Down",
    explain("Vertical approach angle (VAA) is the angle the ball enters the zone.",
            "A flatter VAA (closer to 0) on a high fastball is a big driver of whiffs."),
    layout_columns(
      card(full_screen = TRUE, card_header("Where it crosses vs. how steeply it arrives"),
           plotlyOutput("vaa_plot", height = 460)),
      card(full_screen = TRUE, card_header("Whiff % by vertical location"),
           plotlyOutput("height_whiff_plot", height = 460)))
  ),

  nav_panel("Velocity",
    layout_columns(
      card(full_screen = TRUE, card_header("Average velocity by season"),
           plotlyOutput("velo_trend", height = 400)),
      card(full_screen = TRUE, card_header("Velocity distribution by role"),
           plotlyOutput("velo_dist", height = 400))),
    card(card_header("Outcomes by velocity band (fastballs)"),
         plotlyOutput("velo_outcomes", height = 320))
  ),

  nav_panel("Delivery & Physics",
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE, card_header("Release point and arm angle by season"),
           plotlyOutput("release_plot", height = 420)),
      card(card_header("Reaction-time calculator"),
           explain("Starts from this pitcher's real average fastball. Move the sliders",
                   "to see how velocity and a longer stride (more extension) shrink the",
                   "hitter's window."),
           sliderInput("calc_velo", "Release velocity (mph)", 80, 103, 95, 0.5),
           sliderInput("calc_ext", "Extension (ft)", 5, 8, 6.3, 0.1),
           tableOutput("calc_table"))),
    layout_columns(
      card(card_header("Arm angle and extension over time"),
           plotlyOutput("delivery_trend", height = 320)),
      card(card_header("Frame vs. extension (all pitchers, this season range)"),
           explain("Public Statcast has no stride-length field. Extension is the best",
                   "proxy: a longer stride releases the ball closer to the plate."),
           plotlyOutput("frame_plot", height = 320)))
  ),

  nav_panel("Aging & Prime",
    layout_columns(col_widths = c(4, 8), fill = FALSE,
      selectInput("age_metric", "Metric", METRICS),
      explain("League curves use the delta method: average year-over-year change for",
              "the same pitchers, so survivors don't bias the result. 0 = peak.")),
    layout_columns(
      card(full_screen = TRUE, card_header("League aging curve by role"),
           plotlyOutput("aging_plot", height = 400)),
      card(full_screen = TRUE, card_header("This pitcher vs. his own peak"),
           plotlyOutput("own_curve", height = 400))),
    layout_columns(
      card(card_header("How often pitchers fall off a cliff, by age"),
           explain("Cliff = fastball down 1.5+ mph or K% down 5+ points in one year."),
           plotlyOutput("cliff_plot", height = 300)),
      card(card_header("This pitcher's sudden drops"), DTOutput("cliff_table")))
  ),

  nav_panel("Veterans",
    layout_columns(col_widths = c(6, 6), fill = FALSE,
      selectizeInput("vets", "Veterans to compare", player_choices,
                     selected = default_vets, multiple = TRUE),
      selectInput("vet_metric", "Metric", METRICS)),
    card(full_screen = TRUE,
         card_header("Veterans vs. the league (shaded = 10th-90th percentile, line = median)"),
         plotlyOutput("vet_plot", height = 440)),
    card(card_header("How they adapted: pitch mix over time"),
         plotlyOutput("vet_mix", height = 360))
  ),

  nav_panel("What If?",
    layout_columns(
      card(card_header("What if he pitched his home games somewhere else?"),
           explain("Replays every ball hit off him through each park's home-run rate",
                   "for that exit velocity, launch angle and direction."),
           plotlyOutput("park_plot", height = 520)),
      card(card_header("What if he changed roles?"),
           explain("Average change seen when real pitchers moved between roles from",
                   "one season to the next, applied to his most recent season."),
           selectInput("new_role", "Move him to", ROLES, selected = "Reliever"),
           tableOutput("role_swap"),
           uiOutput("role_swap_note")))
  ),

  nav_spacer(),
  nav_item(tags$a("Data: Baseball Savant / MLB Stats API", href = "https://baseballsavant.mlb.com",
                  target = "_blank"))
)

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {

  # Pitcher's pitches after sidebar filters
  pitches_sel <- reactive({
    req(input$pitcher)
    d <- pitches %>%
      filter(pitcher == as.numeric(input$pitcher),
             between(game_year, input$years[1], input$years[2]),
             role %in% input$roles)
    if (!is.null(input$ptypes)) d <- d %>% filter(pitch_name %in% input$ptypes)
    if (isTRUE(input$arm_side)) d <- d %>% mutate(hb = if_else(p_throws == "L", -hb, hb))
    d %>% mutate(game_year = factor(game_year))
  })
  seasons_sel <- reactive({
    seasons %>% filter(pitcher == as.numeric(input$pitcher),
                       between(game_year, input$years[1], input$years[2]),
                       role %in% input$roles)
  })
  pitcher_name <- reactive(players$name[players$pitcher == as.numeric(input$pitcher)])

  output$pitch_type_ui <- renderUI({
    types <- pitches %>% filter(pitcher == as.numeric(input$pitcher)) %>%
      count(pitch_name, sort = TRUE) %>% filter(n >= 30) %>% pull(pitch_name)
    checkboxGroupInput("ptypes", "Pitch types", types, selected = types)
  })

  # Keep the calculator sliders on this pitcher's real fastball
  observeEvent(input$pitcher, {
    s <- seasons %>% filter(pitcher == as.numeric(input$pitcher)) %>% slice_max(game_year)
    if (nrow(s)) {
      updateSliderInput(session, "calc_velo", value = round(s$fb_velo, 1))
      updateSliderInput(session, "calc_ext", value = round(s$extension, 1))
    }
  })

  # ---- Career ----
  output$vb_seasons <- renderText(n_distinct(seasons_sel()$game_year))
  output$vb_velo <- renderText(sprintf("%.1f mph", max(seasons_sel()$fb_velo, na.rm = TRUE)))
  output$vb_k <- renderText(scales::percent(max(seasons_sel()$k_pct, na.rm = TRUE), .1))
  output$vb_frame <- renderText({
    b <- bios %>% filter(pitcher == as.numeric(input$pitcher))
    paste0(b$height, " / ", b$weight, " lb")
  })
  output$season_table <- renderDT({
    seasons_sel() %>%
      transmute(Year = game_year, Age = age, Role = role, G, GS, Pitches = pitches,
                `FB velo` = round(fb_velo, 1), `K%` = k_pct, `BB%` = bb_pct,
                `Whiff%` = whiff_pct, xwOBA = round(xwoba, 3),
                `Arm angle` = round(arm_angle, 1), `Ext (ft)` = round(extension, 2),
                `FB flight (s)` = round(fb_flight, 3)) %>%
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 15)) %>%
      formatPercentage(c("K%", "BB%", "Whiff%"), 1)
  })
  output$mix_plot <- renderPlotly({
    d <- pitch_mix %>% filter(pitcher == as.numeric(input$pitcher),
                              between(game_year, input$years[1], input$years[2]))
    pretty_plotly(ggplot(d, aes(game_year, usage, fill = pitch_name)) +
      geom_area(position = "stack", alpha = .9) +
      scale_y_continuous(labels = scales::percent) + labs(x = NULL, y = NULL, fill = NULL))
  })

  # ---- Movement ----
  output$movement_plot <- renderPlotly({
    d <- pitches_sel(); req(nrow(d) > 0)
    lg <- league_types %>%
      filter(role %in% unique(d$role), pitch_name %in% unique(d$pitch_name),
             p_throws == d$p_throws[1],
             between(game_year, input$years[1], input$years[2])) %>%
      group_by(role, pitch_name) %>%
      summarise(hb = weighted.mean(hb, n), ivb = weighted.mean(ivb, n), .groups = "drop")
    if (isTRUE(input$arm_side) && d$p_throws[1] == "L") lg$hb <- -lg$hb
    p <- ggplot(d, aes(hb, ivb, color = .data[[input$color_by]],
                       text = paste0(pitch_name, " ", round(release_speed, 1), " mph<br>",
                                     outcome, "<br>", game_date))) +
      geom_hline(yintercept = 0, color = "gray80") + geom_vline(xintercept = 0, color = "gray80") +
      geom_point(alpha = .45, size = 1.3) +
      geom_point(data = lg, aes(hb, ivb, text = paste("League avg", role, pitch_name)),
                 inherit.aes = FALSE, shape = 23, size = 3, fill = "gray60", color = "black") +
      facet_wrap(~ role) + coord_equal() +
      labs(x = "Horizontal break (in)", y = "Induced vertical break (in)", color = NULL)
    if (input$color_by == "outcome") p <- p + scale_color_manual(values = OUTCOME_COLORS)
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  })

  # ---- Up & Down ----
  output$vaa_plot <- renderPlotly({
    d <- pitches_sel(); req(nrow(d) > 0)
    p <- ggplot(d, aes(vaa, plate_z, color = .data[[input$color_by]],
                       text = paste0(pitch_name, "<br>VAA ", round(vaa, 1), "°<br>", outcome))) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1.6, ymax = 3.4, alpha = .06) +
      geom_point(alpha = .45, size = 1.2) + facet_wrap(~ role) +
      labs(x = "Vertical approach angle (°)", y = "Height at plate (ft)", color = NULL)
    if (input$color_by == "outcome") p <- p + scale_color_manual(values = OUTCOME_COLORS)
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  })
  output$height_whiff_plot <- renderPlotly({
    d <- pitches_sel() %>%
      filter(pitch_result %in% c("Whiff", "Foul", "In play")) %>%
      mutate(height = cut(plate_z, c(-Inf, 1.5, 2.0, 2.5, 3.0, 3.5, Inf),
                          c("<1.5", "1.5-2", "2-2.5", "2.5-3", "3-3.5", "3.5+"))) %>%
      group_by(role, pitch_name, height) %>%
      summarise(whiff = mean(pitch_result == "Whiff"), swings = n(), .groups = "drop") %>%
      filter(swings >= 15)
    req(nrow(d) > 0)
    pretty_plotly(ggplot(d, aes(whiff, height, fill = pitch_name)) +
      geom_col(position = "dodge") + facet_wrap(~ role) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = "Whiff per swing", y = "Height at plate (ft)", fill = NULL))
  })

  # ---- Velocity ----
  output$velo_trend <- renderPlotly({
    d <- pitches_sel() %>% group_by(game_year, pitch_name, role) %>%
      summarise(velo = mean(release_speed), n = n(), .groups = "drop") %>% filter(n >= 30)
    pretty_plotly(ggplot(d, aes(as.numeric(as.character(game_year)), velo,
                                color = pitch_name, shape = role)) +
      geom_line() + geom_point(size = 2.5) +
      scale_x_continuous(breaks = scales::breaks_pretty()) +
      labs(x = NULL, y = "mph", color = NULL, shape = NULL))
  })
  output$velo_dist <- renderPlotly({
    d <- pitches_sel(); req(nrow(d) > 0)
    pretty_plotly(ggplot(d, aes(release_speed, fill = role)) +
      geom_density(alpha = .5, color = NA) + facet_wrap(~ pitch_name, scales = "free_y") +
      scale_fill_manual(values = ROLE_COLORS) + labs(x = "mph", y = NULL, fill = NULL))
  })
  output$velo_outcomes <- renderPlotly({
    d <- pitches_sel() %>% filter(pitch_name %in% c("4-Seam Fastball", "Sinker")) %>%
      mutate(band = cut(release_speed, seq(80, 106, 2))) %>%
      group_by(role, band) %>%
      summarise(`Whiff/swing` = sum(pitch_result == "Whiff") /
                  pmax(sum(pitch_result %in% c("Whiff", "Foul", "In play")), 1),
                `K per PA` = mean(pa_result[!is.na(pa_result)] == "Strikeout"),
                `Hit per PA` = mean(pa_result[!is.na(pa_result)] %in% c("Hit (non-HR)", "Home run")),
                n = n(), .groups = "drop") %>%
      filter(n >= 40) %>% pivot_longer(c(`Whiff/swing`, `K per PA`, `Hit per PA`))
    req(nrow(d) > 0)
    pretty_plotly(ggplot(d, aes(band, value, color = role, group = role)) +
      geom_line() + geom_point() + facet_wrap(~ name) +
      scale_color_manual(values = ROLE_COLORS) + scale_y_continuous(labels = scales::percent) +
      labs(x = "Velocity band (mph)", y = NULL, color = NULL))
  })

  # ---- Delivery & Physics ----
  output$release_plot <- renderPlotly({
    d <- pitches_sel() %>% mutate(release_pos_x = abs(release_pos_x))
    s <- seasons_sel()
    pretty_plotly(ggplot(d, aes(release_pos_x, release_pos_z, color = game_year)) +
      geom_point(alpha = .25, size = 1) +
      geom_point(data = d %>% group_by(game_year) %>%
                   summarise(release_pos_x = mean(release_pos_x),
                             release_pos_z = mean(release_pos_z)), size = 4) +
      coord_equal() +
      labs(x = "Release distance from center of rubber (ft)",
           y = "Release height (ft)", color = "Season"))
  })
  output$delivery_trend <- renderPlotly({
    d <- seasons_sel() %>% select(game_year, role, `Arm angle (°)` = arm_angle,
                                  `Extension (ft)` = extension,
                                  `FB flight time (s)` = fb_flight) %>%
      pivot_longer(-c(game_year, role))
    pretty_plotly(ggplot(d, aes(game_year, value, color = role)) + geom_line(aes(group = 1)) +
      geom_point(size = 2.5) + facet_wrap(~ name, scales = "free_y") +
      scale_color_manual(values = ROLE_COLORS) + labs(x = NULL, y = NULL, color = NULL))
  })
  output$frame_plot <- renderPlotly({
    d <- seasons %>% filter(between(game_year, input$years[1], input$years[2]),
                            pitches >= 300) %>%
      group_by(pitcher, name) %>%
      summarise(height_ft = first(height_ft), extension = mean(extension),
                role = names(which.max(table(role))), .groups = "drop") %>%
      mutate(me = pitcher == as.numeric(input$pitcher))
    pretty_plotly(ggplot(d, aes(height_ft * 12, extension, text = name)) +
      geom_jitter(aes(color = role), alpha = .4, width = .3) +
      geom_point(data = filter(d, me), size = 5, shape = 21, fill = "gold", color = "black") +
      geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "gray40") +
      scale_color_manual(values = ROLE_COLORS) +
      labs(x = "Height (in)", y = "Extension (ft)", color = NULL))
  })
  output$calc_table <- renderTable({
    v   <- input$calc_velo; ext <- input$calc_ext
    # Statcast measures vy0 ~4 ft after release, after a little drag; ay is the
    # drag deceleration (~28 ft/s^2 for a 95 mph fastball, scales with v^2)
    vy0 <- -v * 1.4667 * 0.985
    ay  <- 0.00158 * vy0^2
    t   <- flight_time(vy0, ay, ext)
    decide <- 0.175   # roughly how long a swing takes once started
    tibble(
      ` ` = c("Flight time, release to plate", "Time left to decide (minus ~0.175 s swing)",
              "Perceived velocity", "Distance ball travels"),
      Value = c(sprintf("%.3f s", t), sprintf("%.3f s", t - decide),
                sprintf("%.1f mph", perceived_velo(v, ext)),
                sprintf("%.1f ft", RUBBER - ext - Y_PLATE)))
  }, striped = TRUE, width = "100%")

  # ---- Aging & Prime ----
  output$aging_plot <- renderPlotly({
    d <- aging %>% filter(metric == input$age_metric, role %in% input$roles)
    req(nrow(d) > 0)
    peaks <- d %>% distinct(role, peak_age)
    pretty_plotly(ggplot(d, aes(age, curve, color = role)) +
      geom_hline(yintercept = 0, color = "gray70") + geom_line(linewidth = 1.1) + geom_point() +
      geom_vline(data = peaks, aes(xintercept = peak_age, color = role), linetype = 2) +
      scale_color_manual(values = ROLE_COLORS) +
      scale_y_continuous(labels = fmt_metric(input$age_metric)) +
      labs(x = "Age", y = "Change vs. peak", color = NULL))
  })
  output$own_curve <- renderPlotly({
    m <- input$age_metric
    d <- seasons_sel() %>% filter(!is.na(.data[[m]]))
    req(nrow(d) > 0)
    best <- if (m %in% LOWER_BETTER) min(d[[m]]) else max(d[[m]])
    d <- d %>% mutate(vs_peak = .data[[m]] - best)
    pretty_plotly(ggplot(d, aes(age, vs_peak)) + geom_hline(yintercept = 0, color = "gray70") +
      geom_line(color = "gray50") +
      geom_point(aes(color = role, shape = cliff), size = 3) +
      scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4), labels = c("", "Cliff year")) +
      scale_color_manual(values = ROLE_COLORS) +
      scale_y_continuous(labels = fmt_metric(m)) +
      labs(x = "Age", y = paste("vs. his best", names(METRICS)[METRICS == m]),
           color = NULL, shape = NULL, title = pitcher_name()))
  })
  output$cliff_plot <- renderPlotly({
    d <- cliff_rates %>% filter(role %in% input$roles)
    pretty_plotly(ggplot(d, aes(age, cliff_rate, fill = role)) +
      geom_col(position = "dodge") + scale_fill_manual(values = ROLE_COLORS) +
      scale_y_continuous(labels = scales::percent) + labs(x = "Age", y = NULL, fill = NULL))
  })
  output$cliff_table <- renderDT({
    seasons_sel() %>% filter(cliff) %>%
      transmute(Year = game_year, Age = age, Role = role,
                `Velo change` = round(d_velo, 1), `K% change` = round(100 * d_k, 1)) %>%
      datatable(rownames = FALSE, options = list(dom = "t"))
  })

  # ---- Veterans ----
  output$vet_plot <- renderPlotly({
    req(input$vets)
    m <- input$vet_metric
    v <- seasons %>% filter(pitcher %in% as.numeric(input$vets), pitches >= 200)
    b <- bands %>% filter(metric == m, role == "Starter")
    pretty_plotly(ggplot() +
      geom_ribbon(data = b, aes(game_year, ymin = p10, ymax = p90), fill = "gray85") +
      geom_line(data = b, aes(game_year, p50), color = "gray45", linetype = 2) +
      geom_line(data = v, aes(game_year, .data[[m]], color = name)) +
      geom_point(data = v, aes(game_year, .data[[m]], color = name,
                               text = paste0(name, ", age ", age, ", ", role)), size = 2.5) +
      scale_y_continuous(labels = fmt_metric(m)) +
      labs(x = NULL, y = names(METRICS)[METRICS == m], color = NULL))
  })
  output$vet_mix <- renderPlotly({
    req(input$vets)
    d <- pitch_mix %>% filter(pitcher %in% as.numeric(input$vets)) %>%
      left_join(players %>% select(pitcher, name), by = "pitcher")
    pretty_plotly(ggplot(d, aes(game_year, usage, fill = pitch_name)) +
      geom_area(position = "stack") + facet_wrap(~ name) +
      scale_y_continuous(labels = scales::percent) + labs(x = NULL, y = NULL, fill = NULL))
  })

  # ---- What If ----
  output$park_plot <- renderPlotly({
    bb <- batted_balls %>% filter(pitcher == as.numeric(input$pitcher),
                                  between(game_year, input$years[1], input$years[2]))
    req(nrow(bb) > 0)
    actual <- sum(bb$is_hr)
    d <- park_hr %>%
      inner_join(bb %>% count(ev_bin, la_bin, dir), by = c("ev_bin", "la_bin", "dir"),
                 relationship = "many-to-many") %>%
      group_by(park) %>% summarise(exp_hr = sum(rate * n), .groups = "drop") %>%
      mutate(park = reorder(park, exp_hr))
    pretty_plotly(ggplot(d, aes(exp_hr, park, text = paste0(park, ": ", round(exp_hr, 1), " HR"))) +
      geom_col(fill = "#2F6DB5") +
      geom_vline(xintercept = actual, linetype = 2, color = "#B5302F") +
      labs(x = paste0("Expected HR allowed (red line = actual: ", actual, ")"), y = NULL))
  })
  role_effects <- reactive({
    s <- seasons %>% filter(pitches >= 200) %>% arrange(pitcher, game_year)
    s %>% inner_join(s, by = "pitcher", suffix = c("_a", "_b"),
                     relationship = "many-to-many") %>%
      filter(game_year_b == game_year_a + 1, role_a != role_b) %>%
      group_by(from = role_a, to = role_b) %>%
      summarise(n = n(), d_velo = mean(fb_velo_b - fb_velo_a, na.rm = TRUE),
                d_k = mean(k_pct_b - k_pct_a, na.rm = TRUE),
                d_bb = mean(bb_pct_b - bb_pct_a, na.rm = TRUE), .groups = "drop")
  })
  output$role_swap <- renderTable({
    cur <- seasons %>% filter(pitcher == as.numeric(input$pitcher)) %>% slice_max(game_year)
    eff <- role_effects() %>% filter(from == cur$role, to == input$new_role)
    if (cur$role == input$new_role || nrow(eff) == 0) return(NULL)
    tibble(Stat = c("FB velo", "K%", "BB%"),
           `Now` = c(sprintf("%.1f", cur$fb_velo), scales::percent(cur$k_pct, .1),
                     scales::percent(cur$bb_pct, .1)),
           `As a new role` = c(sprintf("%.1f", cur$fb_velo + eff$d_velo),
                               scales::percent(cur$k_pct + eff$d_k, .1),
                               scales::percent(cur$bb_pct + eff$d_bb, .1)))
  }, striped = TRUE, width = "100%")
  output$role_swap_note <- renderUI({
    cur <- seasons %>% filter(pitcher == as.numeric(input$pitcher)) %>% slice_max(game_year)
    eff <- role_effects() %>% filter(from == cur$role, to == input$new_role)
    if (cur$role == input$new_role) return(explain("He was already a ", cur$role, " in ",
                                                   cur$game_year, "."))
    if (nrow(eff) == 0) return(explain("Not enough pitchers made this switch to estimate it."))
    explain(sprintf("Based on %d real %s-to-%s switches. %s season, age %d.",
                    eff$n, cur$role, input$new_role, cur$game_year, cur$age))
  })
}

shinyApp(ui, server)
