

# Load libraries ----------------------------------------------------------


library(tidyverse)
library(rvest)
library(mlbstatsR)
library(ggtext)
library(ggimage)
library(here)
library(gt)
library(espnscrapeR) # gt_theme_538()
library(scales)

# Data Wrangler -----------------------------------------------------------

teams <- get_reference_team_mlb()
teams_standings <- get_reference_team_mlb(stats = "pitching") %>%
  select(tm, w, l, ra = r)
logos <- get_png_logos() %>%
  select(tm = full_name, team_name, logo = logoscoreboard)


df <- teams %>%
  select(tm, g, r) %>%
  left_join(teams_standings) %>%
  filter(!(tm %in% c("Tm", "", "League Average"))) %>%
  mutate_at(c("l", "g", "w", "r", "ra"), as.numeric) %>%
  select(tm, g, w, l, r, ra)
df <- df %>%
  mutate(
    sc_ratio = round(r / ra, 3),
    win_pct = round(sc_ratio^2 / (sc_ratio^2 + 1), 3),
    act_win_pct = round(w / g, 3),
    abs_error = abs(act_win_pct - win_pct)
  )

df <- left_join(df, logos)

# gt table ----------------------------------------------------------------

df %>%
  arrange(
    desc(w),
    l
  ) %>%
  select(logo, team_name, g, w, l, r, ra, sc_ratio, win_pct, act_win_pct, abs_error) %>%
  gt() %>%
  text_transform(
    locations = cells_body(c(logo)),
    fn = function(x) {
      web_image(
        url = x,
        height = px(20)
      )
    }
  ) %>%
  tab_options(
    data_row.padding = px(1),
  ) %>%
  cols_label(
    logo = gt::html("<span style='font-weight:bold;font-size:12px'>TEAM</span>"),
    team_name = "",
    g = gt::html("<span style='font-weight:bold;font-size:12px'>GAMES</span>"),
    w = gt::html("<span style='font-weight:bold;font-size:12px'>WINS</span>"),
    l = gt::html("<span style='font-weight:bold;font-size:12px'>LOSSES</span>"),
    r = gt::html("<span style='font-weight:bold;font-size:12px'>RUNS<br>SCORED</span>"),
    ra = gt::html("<span style='font-weight:bold;font-size:12px'>RUNS<br>ALLOWED</span>"),
    sc_ratio = gt::html("<span style='font-weight:bold;font-size:12.7px'>SCORING<br>RATIO</span>"),
    win_pct = gt::html("<span style='font-weight:bold;font-size:12.7px'>PREDICTED<br>WINNING %</span>"),
    act_win_pct = gt::html("<span style='font-weight:bold;font-size:12.7px'>ACTUAL<br>WINNING %</span>"),
    abs_error = gt::html("<span style='font-weight:bold;font-size:12.7px'>ABSOLUTE<br>ERROR</span>"),
  ) %>%
  gt_theme_538() %>%
  tab_header(
    title = md("**Pronóstico de los porcentajes de victorias de la MLB**"),
    subtitle = md(paste0("Temporada 2021 por equipos con MAD (Desviación media absoluta) = ", scales::percent(DescTools::MeanAD(df$abs_error), accuracy = 0.01), format(Sys.Date(), format = "<br>A %d %B, %Y"))) # round(DescTools::MeanAD(df$abs_error),3)
  ) %>%
  cols_align(
    align = "right",
    columns = c(logo)
  ) %>%
  cols_align(
    align = "center",
    columns = c(g:abs_error)
  ) %>%
  fmt_percent(
    columns = c(win_pct:abs_error),
    decimals = 1
  ) %>%
  data_color(
    columns = c(win_pct),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>%
  tab_options(
    heading.title.font.size = 22.652,
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = 12,
    footnotes.padding = px(1)
  ) %>%
  tab_source_note(
    source_note = md("Inspired by BASEBALL'S PYTHAGOREAN THEOREM from <i>Winston, Wayne L.. Mathletics Book<i>
                       <div><b>Grafica por</b> : <i>\n Ivo Villanueva @elcheff<i>
                       <div><b>Datos por</b> : \n<i>mlbstatsR y @baseball_ref<i>")
  ) %>%
  gtsave("pitagorean.html")

# 31 de Agosto 2021 -----------------------------------------------------
# Ivo Villanueva ----------------------------------------------------------


