## If NBA ratings have dipped over time, have games become less interesting? 
## Overall game differential (end of game)
## Game differential by quarter over time 
## Number of 10/15/20pt comebacks
library(tidyverse)
library(nbastatR)
library(data.table)
library(sqldf)
library(future)

## get NBA schedules for 1990 - 2020 seasons 
seasons <- 1990:2020
nba_schedule <- data.frame()
for (s in seasons){
  print(s) 
  ## get individual season schedule (reg season & playoffs)
  single_season <- suppressWarnings(suppressMessages(seasons_schedule(s, season_types = c('Regular Season', 'Playoffs')) %>% 
    data.frame() %>% 
    mutate(season = s)))
  ## combine individual season with all seasons data 
  nba_schedule <- suppressWarnings(bind_rows(nba_schedule, single_season))
}

## get box scores (by month) for 1990 - 2020 seasons 
bad_box_scores <- data.frame(game_id = 1)
seasons <- 1990:2020
# s <- 2013 ##(april)
for(s in seasons){
  print(s)
  season_games <- nba_schedule %>% filter(season == s)
  print(dim(season_games))
  game_ids <- season_games$idGame
  game_months <- months(season_games$dateGame) %>% unique()
  
  for (gm in game_months){
    plan(multiprocess) 
    print(gm)
    sg <- season_games %>% filter(months(dateGame) == gm)
    print(dim(sg))
    
    game_ids <- sg$idGame
    monthly_box_scores <- data.frame()
    
    for(g_id in 1:length(game_ids)){
      g <- game_ids[g_id]
      # print(g)
    # for(g in game_ids){
      print(g)
      print(paste('Game ', g_id, ' of ', length(game_ids), sep = ''))
      
      ## get single game box scores 
      box_score_game <- suppressMessages(suppressWarnings(box_scores(g 
                                                                     ,result_types = 'team'
                                                                     ,box_score_types = 'traditional'))) %>% as.list()
      if (length(box_score_game) == 0) {
        print(g)
        bad_box_scores <- bind_rows(bad_box_scores, data.frame(game_id = g))
      } else {
      team_box_score <- box_score_game[[2]][1] %>% data.frame()
      }
      ## combine single game box score with all box scores 
      monthly_box_scores <- suppressWarnings(bind_rows(monthly_box_scores, team_box_score))
    }
    
    output_df_name <- paste('box_scores_', s, '_', gm, '.csv', sep = '')
    # fwrite(monthly_box_scores, paste('Desktop/SPORTS/NBA_History_outputs/', output_df_name, sep = ''))
    fwrite(monthly_box_scores, paste('Desktop/SPORTS/NBA_History_outputs2/', output_df_name, sep = ''))
    
  }
  
}

## 21201214 - canceled game (box score function fails)


## combine all box scores data (2000 - 2020 seasons)
files_all <- list.files('Desktop/SPORTS/NBA_History_outputs/')
box_scores <- data.frame()
for (f in files_all){
  # print(f)
  file_loc <- paste('Desktop/SPORTS/NBA_History_outputs/', f, sep = '')
  # print(file_loc)
  tmp_file <- fread(file_loc)
  box_scores <- bind_rows(box_scores, tmp_file)
}

## combine all box scores data (1990 - 1999 seasons)
files_all2 <- list.files('Desktop/SPORTS/NBA_History_outputs2/')
box_scores2 <- data.frame()
for (f2 in files_all2){
  # print(f)
  file_loc2 <- paste('Desktop/SPORTS/NBA_History_outputs2/', f2, sep = '')
  # print(file_loc)
  tmp_file2 <- fread(file_loc2)
  box_scores2 <- bind_rows(box_scores2, tmp_file2)
}

## fix columns in box_scores2
box_scores2 <- box_scores2 %>% 
  mutate(minExact = coalesce(as.numeric(minExact), as.numeric(minutes)))
## drop minutes column 
box_scores2$minutes <- NULL

## combine all 30 seasons of box scores 
box_scores_all <- bind_rows(box_scores, box_scores2) %>% distinct()

## write files for github upload
fwrite(nba_schedule, 'Desktop/SPORTS/NBA_History/nba_schedule.csv')
fwrite(box_scores_all, 'Desktop/SPORTS/NBA_History/box_scores_all.csv')