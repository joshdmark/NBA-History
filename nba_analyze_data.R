library(tidyverse)
library(data.table)
library(sqldf)
library(lubridate)
library(RCurl)

## get NBA schedule
schedule_url <- getURL('https://raw.githubusercontent.com/joshdmark/NBA-History/master/NBA_History/nba_schedule.csv')
schedule <- read.csv(text = schedule_url, stringsAsFactors = FALSE) %>% data.frame()
rm(schedule_url) ## remove url 

## get box scores 
box_scores_url <- getURL('https://raw.githubusercontent.com/joshdmark/NBA-History/master/NBA_History/box_scores_all.csv')
box_scores <- read.csv(text = box_scores_url, stringsAsFactors = FALSE) %>% data.frame()
rm(box_scores_url) ## remove url

## check_NA function
check_NA <- function(df){
  apply(apply(df, 2, FUN = is.na), 2, sum)
}

## unique datasets
schedule <- schedule %>% 
  mutate(team1 = slugTeamWinner, 
         team2 = slugTeamLoser) %>% distinct()
box_scores <- distinct(box_scores)

## add box_score_data to schedule
# winning team
team1 <- schedule %>% 
  select(idGame, slugTeamWinner) %>% 
  mutate(slugTeam = slugTeamWinner) %>% 
  left_join(box_scores, by = c('idGame', 'slugTeam'))
check_NA(df = team1)
# fix names (add team1)
team1_names <- paste(names(team1), 'team1', sep = '_')
names(team1) <- team1_names

# losing team
team2 <- schedule %>% 
  select(idGame, slugTeamLoser) %>% 
  mutate(slugTeam = slugTeamLoser) %>% 
  left_join(box_scores, by = c('idGame', 'slugTeam'))
check_NA(df = team2)
# fix names (add team2)
team2_names <- paste(names(team2), 'team2', sep = '_')
names(team2) <- team2_names

## combine box score and schedule
schedule <- sqldf("select s.*, t1.*, t2.*
             from schedule s 
             left join team1 t1 on s.idGame = t1.idGame_team1 and s.team1 = t1.slugTeam_team1
             left join team2 t2 on s.idGame = t2.idGame_team2 and s.team2 = t2.slugTeam_team2")

teams_list <- schedule %>% select(team1) %>% arrange(team1) %>% unique() %>% as.vector()
teams_list <- teams_list$team1

## remove everything except schedule, teams_list
rm(list=setdiff(ls(), c("schedule", "teams_list")))

nba_output <- data.frame()
## loop through teams 
for(t in 1:length(teams_list)){
  team <- teams_list[t]
  print(paste('team ', t, ' of ', length(teams_list), ': ', team, sep = ''))
  
  team_stuff <- schedule %>% filter(team1 == team) %>% mutate(team = team)
  team_names <- names(team_stuff)
  team_names <- str_replace_all(team_names, 'team1', 'team')
  team_names <- str_replace_all(team_names, 'team2', 'opp')
  names(team_stuff) <- team_names
  
  opp_stuff <- schedule %>% filter(team2 == team) %>% mutate(team = team)
  opp_names <- names(opp_stuff)
  opp_names <- str_replace_all(opp_names, 'team1', 'opp')
  opp_names <- str_replace_all(opp_names, 'team2', 'team')
  names(opp_stuff) <- opp_names
  
  team_df <- bind_rows(team_stuff, opp_stuff) %>% arrange(dateGame) %>% distinct()
  nba_output <- bind_rows(nba_output, team_df) %>% 
    select(team, everything()) %>% 
    arrange(team)
}

## add margin of victory (mov column)
xx <- nba_output %>% select(idGame, dateGame, pts_team, pts_opp) %>% mutate(mov = pts_team - pts_opp)
table(xx$mov > 0)

## fix tied score games
tied_scores <- xx %>% filter(pts_team == pts_opp) %>% select(idGame)
tied_games <- nba_output %>% 
  filter(idGame %in% tied_scores$idGame) %>% 
  arrange(dateGame, idGame) %>% 
  select(team, idGame, dateGame, slugMatchup, slugTeamWinner, slugTeamLoser, pts_team, pts_opp)
fwrite(tied_games, 'Desktop/SPORTS/NBA_History/tied_games_output.csv')

## read fixed tied games 
fixed_scores_url <- getURL('https://raw.githubusercontent.com/joshdmark/NBA-History/master/NBA_History/tied_games_fixed.csv')
fixed_scores <- read.csv(text = fixed_scores_url, stringsAsFactors = FALSE) %>% data.frame()
rm(fixed_scores_url) ## remove url

## add fixed scores to schedule 
schedule <- sqldf("select s.*, 
              fs1.pts_team pts_team_fs1, fs1.pts_opp pts_opp_fs1, 
              fs2.pts_team pts_team_fs2, fs2.pts_opp pts_opp_fs2
             from schedule s 
             left join fixed_scores fs1 on s.idGame = fs1.idGame and s.team1 = fs1.team
             left join fixed_scores fs2 on s.idGame = fs2.idGame and s.team2 = fs2.team") %>% 
  data.frame() %>% 
  mutate(pts_team1 = coalesce(pts_team_fs1, pts_team1), 
         pts_team2 = coalesce(pts_team_fs2, pts_team2)) %>% 
  mutate(mov = pts_team1 - pts_team2) %>% 
  select(-pts_team_fs1, -pts_opp_fs1, -pts_team_fs2, -pts_opp_fs2)

## add fixed scores to nba_output
nba_output <- sqldf("select nba.*, fs1.pts_team pt, fs1.pts_opp po
                  from nba_output nba
                  left join fixed_scores fs1 on nba.idGame = fs1.idGame and nba.team = fs1.team") %>% 
  data.frame() %>% 
  mutate(pts_team = coalesce(pt, pts_team), 
         pts_opp = coalesce(po, pts_opp)) %>% 
  mutate(mov = pts_team - pts_opp) %>% 
  distinct()

#### fix: make winning team team1 always 
ok_data <- schedule %>% filter(pts_team1 > pts_team2)
bad_data <- schedule %>% filter(pts_team1 < pts_team2)

bad_data <- bad_data %>% 
  arrange(dateGame, idGame) %>% 
  select(idGame, dateGame, slugMatchup, team1, team2, pts_team1, pts_team2)
dim(bad_data)
fwrite(bad_data, 'Desktop/SPORTS/NBA_History/bad_scores_schedule.csv')

bad_games <- schedule %>% filter(pts_team1 < pts_team2) %>% select(idGame)
bad_score_games <- nba_output %>% 
  filter(idGame %in% bad_games$idGame) %>% 
  arrange(dateGame, idGame) %>% 
  select(team, idGame, dateGame, slugMatchup, slugTeamWinner, slugTeamLoser, pts_team, pts_opp)
dim(bad_score_games)
fwrite(bad_score_games, 'Desktop/SPORTS/NBA_History/bad_score_games.csv')

## read fixed bad score games 
bs_url <- getURL('https://raw.githubusercontent.com/joshdmark/NBA-History/master/fixed_scores/bad_score_games_fixed.csv')
bad_scores_fixed <- read.csv(text = bs_url, stringsAsFactors = FALSE) %>% data.frame()
rm(bs_url) ## remove url

## fix scores for 37 games (74 rows)
nba_output <- sqldf("select n.*, b.pts_team as pts_team_fixed, b.pts_opp as pts_opp_fixed
             from nba_output n 
             left join bad_scores_fixed b 
              on n.idGame = b.idGame and n.team = b.team")
nba_output <- nba_output %>% 
  mutate(pts_team = coalesce(pts_team_fixed, pts_team), 
         pts_opp = coalesce(pts_opp_fixed, pts_opp)) %>% 
  mutate(mov = pts_team - pts_opp) %>% 
  select(-pts_team_fixed, -pts_opp_fixed, -pt, -po)

## read fixed bad schedule games 
bs_url <- getURL('https://raw.githubusercontent.com/joshdmark/NBA-History/master/fixed_scores/bad_scores_schedule_fixed.csv')
bad_schedule_fixed <- read.csv(text = bs_url, stringsAsFactors = FALSE) %>% data.frame()
rm(bs_url) ## remove url

## fix scores for 37 games 
schedule <- sqldf("select s.*, bs.pts_team1 as pts_team1_fixed, bs.pts_team2 as pts_team2_fixed 
             from schedule s 
             left join bad_schedule_fixed bs 
              on s.idGame = bs.idGame") %>% 
  mutate(pts_team1 = coalesce(pts_team1_fixed, pts_team1), 
         pts_team2 = coalesce(pts_team2_fixed, pts_team2), 
         mov = pts_team1 - pts_team2) %>% 
  select(-pts_team1_fixed, -pts_team2_fixed)

## remove everything except schedule, nba_output 
rm(list=setdiff(ls(), c("schedule", "nba_output")))

## NOP: NOP, NOH, NOK
## MEM: MEM, VAN
## BKN: BKN, NJN
## OKC: OKC, SEA 
## GSW: GSW, GOS 
## SAS: SAN, SAS
## CHA: CHH, CHA 

## add team franchises to nba_output
schedule <- schedule %>% 
  mutate(franchise_team1 = case_when(
    team1 %in% c('NOP', 'NOH', 'NOK') ~ 'NOP', 
    team1 %in% c('MEM', 'VAN') ~ 'MEM', 
    team1 %in% c('BKN', 'NJN') ~ 'BKN', 
    team1 %in% c('OKC', 'SEA') ~ 'OKC', 
    team1 %in% c('GSW', 'GOS') ~ 'GSW', 
    team1 %in% c('SAN', 'SAS') ~ 'SAS', 
    team1 %in% c('CHH', 'CHA') ~ 'CHA', 
    team1 %in% c('PHI', 'PHL') ~ 'PHI',
    team1 %in% c('UTA', 'UTH') ~ 'UTA',
    TRUE ~ team1
  )) %>% 
  mutate(franchise_team2 = case_when(
    team2 %in% c('NOP', 'NOH', 'NOK') ~ 'NOP', 
    team2 %in% c('MEM', 'VAN') ~ 'MEM', 
    team2 %in% c('BKN', 'NJN') ~ 'BKN', 
    team2 %in% c('OKC', 'SEA') ~ 'OKC', 
    team2 %in% c('GSW', 'GOS') ~ 'GSW', 
    team2 %in% c('SAN', 'SAS') ~ 'SAS', 
    team2 %in% c('CHH', 'CHA') ~ 'CHA', 
    team2 %in% c('PHI', 'PHL') ~ 'PHI',
    team2 %in% c('UTA', 'UTH') ~ 'UTA',
    TRUE ~ team2
  )) 

## add team franchises to nba_output
nba_output <- nba_output %>% 
  mutate(franchise_team = case_when(
    slugTeam_team %in% c('NOP', 'NOH', 'NOK') ~ 'NOP', 
    slugTeam_team %in% c('MEM', 'VAN') ~ 'MEM', 
    slugTeam_team %in% c('BKN', 'NJN') ~ 'BKN', 
    slugTeam_team %in% c('OKC', 'SEA') ~ 'OKC', 
    slugTeam_team %in% c('GSW', 'GOS') ~ 'GSW', 
    slugTeam_team %in% c('SAN', 'SAS') ~ 'SAS', 
    slugTeam_team %in% c('CHH', 'CHA') ~ 'CHA', 
    slugTeam_team %in% c('PHI', 'PHL') ~ 'PHI',
    slugTeam_team %in% c('UTA', 'UTH') ~ 'UTA',
    TRUE ~ slugTeam_team
  )) %>% 
  mutate(franchise_opp = case_when(
    slugTeam_opp %in% c('NOP', 'NOH', 'NOK') ~ 'NOP', 
    slugTeam_opp %in% c('MEM', 'VAN') ~ 'MEM', 
    slugTeam_opp %in% c('BKN', 'NJN') ~ 'BKN', 
    slugTeam_opp %in% c('OKC', 'SEA') ~ 'OKC', 
    slugTeam_opp %in% c('GSW', 'GOS') ~ 'GSW', 
    slugTeam_opp %in% c('SAN', 'SAS') ~ 'SAS', 
    slugTeam_opp %in% c('CHH', 'CHA') ~ 'CHA', 
    slugTeam_opp %in% c('PHI', 'PHL') ~ 'PHI',
    slugTeam_opp %in% c('UTA', 'UTH') ~ 'UTA',
    TRUE ~ slugTeam_opp
  )) 
## remove extra columns 
nba_output$idGame_opp <- NULL 
nba_output$idGame_team <- NULL 
nba_output$slugTeamWinner_opp <- NULL 
nba_output$slugTeamWinner_team <- NULL 
nba_output$slugTeamLoser_opp <- NULL 
nba_output$slugTeamLoser_team <- NULL 
#
nba_output$numberGameDay <- NULL 
nba_output$idTeam_opp <- NULL 
nba_output$idTeam_team <- NULL 

## write output files 
fwrite(schedule, 'Desktop/SPORTS/NBA_History/final_outputs/schedule.csv')
fwrite(nba_output, 'Desktop/SPORTS/NBA_History/final_outputs/nba_output.csv')
