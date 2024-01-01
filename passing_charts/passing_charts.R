"
Plot the QB pass maps of QBs given a year and team
Inspired by the Passing Charts on rbsdm.com
https://rbsdm.com/stats/box_scores/

I want to plot the season charts. The website only has the game-by-game charts

@author: TDK
@date: 1/1/2024
"

### Load packages
library(glue)
library(tidyverse)
library(nflreadr)
library(nflfastR)

### SET CONSTANTS
PASSES = c("INTERCEPTION", "PASS")

### Write the caption to use for the plots
DATA = "Data from @nflfastR"
VERTICAL_PLACEMENT = "Vertical placement is air yards"
HORIZONTAL_PLACEMENT = "Horizontal placement is random"
INSPIRATION = "Inspired by rbsdm.com"
CAPTION = glue("{DATA} || {VERTICAL_PLACEMENT} || {HORIZONTAL_PLACEMENT} || {INSPIRATION}")

load_qb_plays <- function(years, qbs, team){
  "
  Load the QB for a subset of seasons and keep information on the play such as:
    passer_player_name, season, posteam, air_yards, pass_location, complete_pass,
    incomplete_pass, interception, pass_touchdown
  
  @param years vector: a vector of integers containing the years of interest
  @param qbs vector: a vector of strings containing the QB name
    (First Initial.Last Name)
  @param team str: the team abbreviation of interest. This is the possession team
  @return plays df: r dataframe containing information about the pass attempt
  "
  
  ### Grab the plays
  plays <- load_pbp(years) |>
    filter(season_type == "REG" &
             passer_player_name %in% qbs &
             posteam == team &
             play_type_nfl %in% PASSES &
             !is.na(pass_location)) |>
    select(passer_player_id, passer_player_name, season, posteam, air_yards,
           pass_location, epa, complete_pass, incomplete_pass, interception,
           pass_touchdown) |>
    ### Define the pass result
    mutate(pass_result = ifelse(complete_pass == 1 & pass_touchdown == 0, 'Completion',
                             ifelse(incomplete_pass == 1, 'Incomplete',
                                    ifelse(interception == 1, 'Interception',
                                           ifelse(pass_touchdown == 1, 'Touchdown',
                                                  'MISSING')))))
  ### Make sure to remove the MISSING plays
  plays <- plays |> filter(pass_result != 'MISSING')
  
  ### Add team information
  teams = load_teams() |>
    select(team_abbr, team_name, team_color, team_color2, team_logo_wikipedia)
  
  plays <- plays |>
    left_join(teams, by = join_by("posteam" == "team_abbr"))

  return(plays)
}

plot_qb_passes <- function(years, qbs, team){
  "
  Plot the QB pass map. We will make it look like RBSDM.com but also more of a
  football field
  
  @param years vector: a vector of integers containing the years of interest
  @param qbs vector: a vector of strings containing the QB name
    (First Initial.Last Name)
  @param team str: the team abbreviation of interest. This is the possession team
  @save pass_chart_{team}.jpeg jpeg: the passing chart for the subset of qbs
  "
  
  ### Load data
  qb_plays = load_qb_plays(years = years, qbs = qbs, team = team)
  
  ### Plot the data
  qb_plot <- qb_plays |>
    ggplot(aes(x = pass_location,
               y = air_yards,
               shape = pass_result,
               color = as.factor(complete_pass)
               )
           ) +
    geom_jitter(size = length(qbs)) +
    geom_hline(yintercept = 0, linewidth = length(qbs)*1.5, color = "blue", alpha = 0.5) +
    scale_shape_manual(values = c(1, 2, 17, 16)) +
    scale_color_manual(values=c(unique(qb_plays$team_color),
                                unique(qb_plays$team_color2),
                                unique(qb_plays$team_color2),
                                unique(qb_plays$team_color))
                       ) +
    scale_y_continuous(breaks=seq(-10, 60, 10)) +
    facet_wrap(~passer_player_name, ncol = length(qbs)) +
    labs(title = glue("{years} {team} Passer Charts"),
         caption = CAPTION,
         shape = "Pass Result") +
    guides(color = "none")
  
  ### Add the football field
  qb_plot <- qb_plot +
    ### Create the football field
    annotate("segment", 1.5, xend = 1.75, -10:60, yend = -10:60, color = "white", alpha = .2) +
    annotate("segment", 2.5, xend = 2.75, -10:60, yend = -10:60, color = "white", alpha = .2) +
    theme(panel.background = element_rect(fill = "darkgreen", color = "black"),
          plot.margin = margin(length(qbs) * 0.17,
                               length(qbs) * 0.17,
                               length(qbs) * 0.17,
                               length(qbs) * 0.17,
                               "cm"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = length(qbs) * 3),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.key = element_rect(colour = NA, fill = NA),
          legend.box.background = element_blank(),
          legend.title = element_text(size = length(qbs) * 4),
          legend.text = element_text(size = length(qbs) * 4),
          strip.background = element_rect(fill = "white"),
          plot.title = element_text(size = length(qbs) * 5),
          plot.caption = element_text(size = length(qbs) * 4),
          strip.text = element_text(size = length(qbs) * 3.3))
  
  ### Save plot
  ggsave(glue("pass_chart_{team}.jpeg"),
         width = length(qbs) * 11.67, height = length(qbs) * 6.67, units = "cm")
  
}
