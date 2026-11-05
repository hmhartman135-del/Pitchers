# 01b_pull_bios.R ---------------------------------------------------------------
# Looks up height, weight, birthdate and throwing hand for every pitcher in
# data-raw/, ONE player at a time, so a single bad ID can't sink a whole batch.
# Takes ~10 minutes. Does not re-download any pitch data.

library(dplyr)
library(purrr)
library(jsonlite)

RAW_DIR <- "data-raw"

ids <- map(list.files(RAW_DIR, "^statcast_\\d{4}\\.rds$", full.names = TRUE),
           ~ unique(readRDS(.x)$pitcher)) %>% unlist() %>% unique()
ids <- sort(ids[ids < 900000])
message(length(ids), " pitchers to look up")

get_bio <- function(id, i) {
  if (i %% 100 == 0) message(i, " / ", length(ids))
  p <- tryCatch(fromJSON(paste0("https://statsapi.mlb.com/api/v1/people/", id))$people,
                error = function(e) NULL)
  Sys.sleep(0.15)
  if (is.null(p)) return(tibble(pitcher = id, name = NA_character_, height = NA_character_,
                                weight = NA_integer_, birth_date = as.Date(NA),
                                throws = NA_character_))
  tibble(pitcher = id, name = p$fullName,
         height = p$height %||% NA_character_, weight = p$weight %||% NA_integer_,
         birth_date = as.Date(p$birthDate %||% NA_character_),
         throws = p$pitchHand$code %||% NA_character_)
}

bios <- imap_dfr(ids, ~ get_bio(.x, .y))

# Backup name source: Statcast's own "Last, First" names, for anyone the API missed
sc_names <- map_dfr(list.files(RAW_DIR, "^statcast_\\d{4}\\.rds$", full.names = TRUE),
                    ~ readRDS(.x) %>% distinct(pitcher, player_name)) %>%
  distinct(pitcher, .keep_all = TRUE) %>%
  mutate(sc_name = sub("^(.*), (.*)$", "\\2 \\1", player_name))
bios <- bios %>% left_join(sc_names %>% select(pitcher, sc_name), by = "pitcher") %>%
  mutate(name = coalesce(name, sc_name)) %>% select(-sc_name)

saveRDS(bios, file.path(RAW_DIR, "pitcher_bios.rds"))
missing <- sum(is.na(bios$birth_date))
message("Saved ", nrow(bios), " pitchers (", missing, " without a birthdate).")
if (missing > nrow(bios) / 2) {
  message("Most lookups failed. Paste this test into the Console and send me the output:\n",
          "  jsonlite::fromJSON('https://statsapi.mlb.com/api/v1/people/434378')$people$fullName")
} else message("Now run R/02_build_tables.R")
