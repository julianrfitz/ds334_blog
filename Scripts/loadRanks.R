# author: Julian Fitz
# description: load historical ranking data from USCHO

library(tidyverse)
library(rvest)
library(jsonlite)

npi_base_url <- "https://www.uscho.com/rankings/historical/npi/d-i-men"

# single-season scraper

.scrape_npi_season <- function(season_text) {
  url <- sprintf("%s/%s", npi_base_url, season_text)
  
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) {
    message("Skipping ", season_text, " (cannot read page).")
    return(tibble())
  }
  
  data_page_json <- page |>
    html_element("div#app") |>
    html_attr("data-page")
  
  if (is.na(data_page_json) || is.null(data_page_json)) {
    message("No data-page JSON for ", season_text, ", skipping.")
    return(tibble())
  }
  
  data_page <- fromJSON(data_page_json, simplifyVector = FALSE)
  
  # Found from str(): NPI snapshots live in props$content$snap_data
  snap_data <- data_page$props$content$snap_data
  if (is.null(snap_data) || length(snap_data) == 0) {
    message("No snap_data for ", season_text, ", skipping.")
    return(tibble())
  }
  
  team_list <- snap_data$data
  teams     <- names(team_list)
  
  out <- map_dfr(teams, function(team_name) {
    team_snap <- team_list[[team_name]]
    
    map_dfr(names(team_snap), function(dlab) {
      v <- team_snap[[dlab]]            # length-3 vector: rank, npi, record [page:1]
      tibble(
        season     = season_text,
        date_label = dlab,
        team       = team_name,
        rank       = suppressWarnings(as.integer(v[[1]])),
        npi        = suppressWarnings(as.numeric(gsub("[^0-9.]+$", "", v[[2]]))),
        record     = as.character(v[[3]])
      )
    })
  })
  
  out
}

# season list from JSON

get_seasons <- function() {
  # Use current-season page once to read all available seasons.
  url <- sprintf("%s/%s", npi_base_url, "2025-2026")
  page <- read_html(url)
  
  data_page_json <- page |>
    html_element("div#app") |>
    html_attr("data-page")
  
  data_page <- fromJSON(data_page_json, simplifyVector = FALSE)
  
  season_list <- data_page$props$content$season
  sapply(season_list, function(x) x$text)
}

# EXPANDED - all snapshots

load_npi_expanded <- function(start_season = "2020-2021") {
  all_seasons <- get_seasons()
  seasons <- all_seasons[all_seasons >= start_season]
  
  out <- map_dfr(seasons, .scrape_npi_season)
  
  if (!nrow(out)) {
    message("No NPI data found for any generated season.")
    return(out)
  }
  
  out |>
    arrange(season, date_label, rank)
}

# COLLAPSED - final per team season

# If a "Current" snapshot exists, use that
# Otherwise use the latest dated snapshot for that team-season.
load_npi <- function(start_season = "2020-2021") {
  raw <- load_npi_expanded(start_season)
  if (!nrow(raw)) return(raw)
  
  raw |>
    mutate(is_current = date_label == "Current") |>
    group_by(season, team) |>
    arrange(is_current, date_label, .by_group = TRUE) |>
    slice_tail(n = 1) |>
    ungroup() |>
    select(season, team, rank, npi, record, date_label, rank)
}

get_npi_teams <- function() {
  url <- sprintf("%s/%s", npi_base_url, "2025-2026")
  page <- read_html(url)
  
  data_page_json <- page |>
    html_element("div#app") |>
    html_attr("data-page")
  
  data_page <- fromJSON(data_page_json, simplifyVector = FALSE)
  
  snap_data <- data_page$props$content$snap_data
  unique(unlist(snap_data$teams, use.names = FALSE))
}