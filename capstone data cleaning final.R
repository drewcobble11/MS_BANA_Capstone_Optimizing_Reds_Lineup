library(tidyverse)
library(janitor)
library(stringr)

folder_path <- "pas folder"

files <- list.files(folder_path, pattern = "\\.(txt|csv)$", full.names = TRUE)

read_hitter_file <- function(path) {
  batter_name <- tools::file_path_sans_ext(basename(path)) |>
    str_replace_all("_", " ") |>
    str_to_title()
  
  df <- read_csv(path, show_col_types = FALSE) |>
    clean_names() |>
    mutate(batter = batter_name)
  
  return(df)
}

reds_pa <- map_dfr(files, read_hitter_file)

glimpse(reds_pa)

reds_pa %>%
  select(pitcher, event, play_description, batter)



lhp_names <- read_csv("leftys.txt")
rhp_names <- read_csv("right handed pitchers.txt")



rhp_names <- rhp_names %>%
  mutate(Player = str_to_lower(str_trim(Player)))

lhp_names <- lhp_names %>%
  mutate(Player = str_to_lower(str_trim(Player)))

reds_pa <- reds_pa %>%
  mutate(pitcher = str_to_lower(str_trim(pitcher)))

reds_pa <- reds_pa %>%
  mutate(
    pitcher_hand = case_when(
      pitcher %in% rhp_names$Player ~ "R",
      pitcher %in% lhp_names$Player ~ "L",
      TRUE ~ NA_character_
    )
  )

summary(reds_pa)

reds_pa <- reds_pa %>%
  select(event, play_description, batter, pitcher_hand)
lapply(reds_pa, table)

library(dplyr)
library(stringr)



reds_pa <- reds_pa %>%
  mutate(batter = str_remove(batter, " Pas$"))

name_map <- c(
  "Steer" = "Spencer Steer", 
  "Friedl" = "TJ Friedl",
  "Hayes" = "Ke'Bryan Hayes",
  "Lowe" = "Nathaniel Lowe",
  "Marte" = "Noelvi Marte", 
  "Myers"= "Will Myers",
  "Saurez"= "Eugenio Saurez", 
  "Stewart" = "Sal Stewart"
  )

reds_pa$batter <- recode(reds_pa$batter, !!!name_map)

reds_pa %>%
  group_by(event) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))



reds_outs <- reds_pa %>%
  filter(event == "Out")

reds_outs <- reds_outs %>%
  mutate(sim_event_type = case_when(
    
    str_detect(play_description, "Groundout|Bunt Groundout") ~ "groundout",
    
    str_detect(play_description, "Lineout") &
      str_detect(play_description, "1B|2B|3B|SS|P|C") ~ "infield_out",
    
    str_detect(play_description, "Lineout") &
      str_detect(play_description, "LF|CF|RF") ~ "flyout",
    
    str_detect(play_description, "Popup|Popfly|Foul Pop") ~ "infield_out",
    
    str_detect(play_description, "Flyball") ~ "flyout",
    
    
    TRUE ~ "other"
  ))



event_probs <- reds_pa %>%
  group_by(batter, pitcher_hand, event) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(batter, pitcher_hand) %>%
  mutate(prob = count / sum(count)) %>%
  arrange(batter, pitcher_hand, desc(prob))

reds_pa_sim <- reds_pa %>%
  mutate(sim_event_type = case_when(
    
    str_detect(play_description, "Groundout|Bunt Groundout|Ground Ball Double Play|Ground Ball to SS") ~ "groundout",
    
    str_detect(play_description, "Lineout") &
      str_detect(play_description, "1B|2B|3B|SS|P|C") ~ "infield_out",
    
    str_detect(play_description, "Lineout") &
      str_detect(play_description, "LF|CF|RF") ~ "flyout",
    
    str_detect(play_description, "Popup|Popfly|Foul Pop") ~ "infield_out",
    
    str_detect(play_description, "Flyball") ~ "flyout",
    
    str_detect(event, "SO") ~ "K", 
    
    str_detect(event, "1B|RoE") ~ "1B",
    
    str_detect(event, "2B") ~ "2B",
    
    str_detect(event, "3B") ~ "3B",
    
    str_detect(event, "HR") ~ "HR",
    
    str_detect(event, "BB|HBP|IBB") ~ "BB/HBP",
    
    str_detect(event, "FC") ~ "groundout",
    
    TRUE ~ "other"
  ))


sim_event_probs <- reds_pa_sim %>%
  filter(!is.na(pitcher_hand)) %>%   
  group_by(batter, pitcher_hand, sim_event_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(batter, pitcher_hand) %>%
  mutate(prob = count / sum(count)) %>%
  ungroup() %>%
  arrange(batter, pitcher_hand, desc(prob))

