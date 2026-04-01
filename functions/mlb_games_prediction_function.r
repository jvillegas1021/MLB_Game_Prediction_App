
################### MLB GAMES ############################
                       
mlb_games <- function(game_date = Sys.Date()) {
# grab games
url <- paste0("https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=", game_date, "&hydrate=probablePitcher(note)")
games_df <- GET(url)
parsed <- jsonlite::fromJSON(content(games_df, "text"), flatten = TRUE)
games_table <- parsed$dates$games[[1]]


# check for allstar game

games_table <- games_table %>%
    filter(!gameType %in% c('A', 'E'))
    
if (is.null(games_table) || nrow(games_table) == 0) {
    return(list(
        no_games = TRUE,
        matchup_display_df = NULL,
        starting_pitcher_season_df = NULL,
        starting_pitcher_recent_df = NULL,
        team_batting_df = NULL,
        team_pitching_df = NULL,
        batter_df = NULL,
        batter_splits_df = NULL,
        ball_park_factor = NULL))
  }

###### PULL ALL DATA ################
# pitcher_data
starting_pitcher_season_df <- pitcher_data()
# starting pithcers recent form
starting_pitcher_recent_df <- starting_pitcher_recent_form_data()
#batter data
batter_df <- batter_data()
# batter splits
batter_splits_df <- batting_splits_data()
# team batting_data
team_batting_df <- team_batting_data()
# team pitching
team_pitching_df <- team_pitching_data()
# travel data
team_travel_df <- team_travel_data()
# ball_park_factor
ball_park_factor <- read.csv('data/ball_park_factor.csv')
# start
    
matchup_df <- games_table %>%
  dplyr::select(
    gamePk,
    officialDate,
    status.detailedState,
    venue.name,
    gameDate,
    dayNight,
    teams.home.team.name,
    teams.home.probablePitcher.fullName,
    teams.home.probablePitcher.id,
    teams.away.team.name,
    teams.away.probablePitcher.fullName,
    teams.away.probablePitcher.id
  ) %>%
  dplyr::rename(
    Game_ID = gamePk,
    Game_Date = officialDate,
    Game_Status = status.detailedState,
    Game_Venue = venue.name,
    Game_Time = gameDate,
    Day_Night = dayNight,
    Home_Team = teams.home.team.name,
    Home_Pitcher = teams.home.probablePitcher.fullName,
    Home_Pitcher_ID = teams.home.probablePitcher.id,
    Away_Team = teams.away.team.name,
    Away_Pitcher = teams.away.probablePitcher.fullName,
    Away_Pitcher_ID = teams.away.probablePitcher.id
  )

time <- matchup_df$Game_Time
dt_utc <- ymd_hms(time, tz = 'UTC')
dt_est <- with_tz(dt_utc, tzone = 'America/New_York')
times_est <- format(dt_est, "%I:%M:%p")

matchup_df$Game_Time <- times_est
matchup_df$Game_Time_Stamp <- ymd_hms(games_table$gameDate, tz = 'UTC')

starting_pitcher_season_df <- starting_pitcher_season_df %>%
    filter(xMLBAMID %in% matchup_df$Home_Pitcher_ID | xMLBAMID %in% matchup_df$Away_Pitcher_ID)

starting_pitcher_recent_df <- starting_pitcher_recent_df %>%
    filter(xMLBAMID %in% matchup_df$Home_Pitcher_ID | xMLBAMID %in% matchup_df$Away_Pitcher_ID)

########################## ADD PITCHER THROWING HANDS / WINS / LOSES / ERA###################################
matchup_df <- assign_starting_pitcher_throwing_hands_wins_loses_era(matchup_df, starting_pitcher_season_df)

################### ADD BATTING LINEUPS LIST PLUS HYDRATION STATUS ###################################

matchup_df <- assign_batting_lineups_with_hydration_status(matchup_df, team_batting_df)

################## JOIN BALL PARK FACTOR #########################
matchup_df <- matchup_df %>%
left_join(ball_park_factor, by = c('Game_Venue' = 'Venue', 'Day_Night'))

matchup_df <- matchup_df %>%
    rename(Park_Factor = Park.Factor)
######### PROABABLE PITCHER & PITCHER STATS & LINE UP HYDRATION FLAGS##################################
matchup_df <- matchup_df %>%
mutate(Probable_Pitchers = if_else(
    Home_Pitcher != 'TBD' & Away_Pitcher != 'TBD',
       'Yes',
       'No'
       )
    )

matchup_df <- matchup_df %>%
    mutate(
        Home_Pitcher_Stats_Available = Home_Pitcher_ID %in% starting_pitcher_season_df$xMLBAMID,
        Away_Pitcher_Stats_Available = Away_Pitcher_ID %in% starting_pitcher_season_df$xMLBAMID,
        Pitcher_Stats_Available = Home_Pitcher_Stats_Available & Away_Pitcher_Stats_Available
    )


matchup_df <- matchup_df %>%
  mutate(
    Lineup_Hydration = if_else(
      Home_Lineup_Hydrated == "Yes" & Away_Lineup_Hydrated == "Yes",
      "Yes",
      "No"
    )
  )


############################################## calculate pitcher score #######################################################
home_season_pitcher_df <- starting_pitcher_season_df %>%
    filter(xMLBAMID %in% matchup_df$Home_Pitcher_ID)
    
home_recent_pitcher_df <- starting_pitcher_recent_df %>%
    filter(xMLBAMID %in% matchup_df$Home_Pitcher_ID)
    
away_season_pitcher_df <- starting_pitcher_season_df %>%
    filter(xMLBAMID %in% matchup_df$Away_Pitcher_ID)
    
away_recent_pitcher_df <- starting_pitcher_recent_df %>%
    filter(xMLBAMID %in% matchup_df$Away_Pitcher_ID)

pitcher_season_benchmark <- read.csv("data/pitcher_benchmark.csv")
pitcher_recent_benchmark <- read.csv("data/pitcher_recent_form_benchmark.csv")
    
home_starting_pitcher_season_score_df <- starting_pitcher_season_scores(home_season_pitcher_df,
                                                          pitcher_season_benchmark)  
    
home_starting_pitcher_recent_score_df <- starting_pitcher_recent_scores(home_recent_pitcher_df,
                                                          pitcher_recent_benchmark)    
    
home_starting_pitcher_total_score <- starting_pitcher_total_score(home_starting_pitcher_season_score_df,
                                                                  home_starting_pitcher_recent_score_df,
                                                                  label = "Home_Pitcher_Score")

away_starting_pitcher_season_score_df <- starting_pitcher_season_scores(away_season_pitcher_df,
                                                          pitcher_season_benchmark)

away_starting_pitcher_recent_score_df <- starting_pitcher_recent_scores(away_recent_pitcher_df,
                                                          pitcher_recent_benchmark)     

   
away_starting_pitcher_total_score <- starting_pitcher_total_score(away_starting_pitcher_season_score_df,
                                                             away_starting_pitcher_recent_score_df,
                                                             label = "Away_Pitcher_Score")        

matchup_df <- matchup_df %>%
left_join(home_starting_pitcher_total_score,
          by = c("Home_Pitcher_ID" = "pitcher")
          )

matchup_df <- matchup_df %>%
left_join(away_starting_pitcher_total_score,
          by = c("Away_Pitcher_ID" = "pitcher")
          )

matchup_df <- matchup_df %>%
  mutate(
    Home_Pitcher_Score = if_else(is.na(Home_Pitcher_Score), 0, Home_Pitcher_Score),
    Away_Pitcher_Score = if_else(is.na(Away_Pitcher_Score), 0, Away_Pitcher_Score)
  )

###############################calculate team batting score#######################################################


home_team_df <- team_batting_df %>%
filter(team_batting_df[["team_name"]] %in% matchup_df$Home_Team)

away_team_df <- team_batting_df %>%
filter(team_batting_df[["team_name"]] %in% matchup_df$Away_Team)

benchmark <- read.csv("data/team_batting_benchmark.csv")

home_team_batting_score_df <- team_batting_scores(home_team_df,
                                                  benchmark,
                                                  label = "Home_Batting_Score")        # BATTING SCORE DF
away_team_batting_score_df <- team_batting_scores(away_team_df,
                                                  benchmark,
                                                  label = "Away_Batting_Score")        # BATTING SCORE DF

matchup_df <- matchup_df %>%
left_join(home_team_batting_score_df,
          by = c("Home_Team" = "team_name")
          )

matchup_df <- matchup_df %>%
left_join(away_team_batting_score_df,
          by = c("Away_Team" = "team_name")
          )

###############################################calculate team pitching score########################################

home_team_df <- team_pitching_df %>%
filter(team_pitching_df[["team_name"]] %in% matchup_df$Home_Team)

away_team_df <- team_pitching_df %>%
filter(team_pitching_df[["team_name"]] %in% matchup_df$Away_Team)

benchmark <- read.csv("data/team_pitching_benchmark.csv")

home_team_pitching_score_df <- team_pitching_scores(home_team_df,
                                                  benchmark,
                                                  label = "Home_Pitching_Score")        # PITCHING SCORE DF
away_team_pitching_score_df <- team_pitching_scores(away_team_df,
                                                  benchmark,
                                                  label = "Away_Pitching_Score")        # PITCHING SCORE DF

matchup_df <- matchup_df %>%
left_join(home_team_pitching_score_df,
          by = c("Home_Team" = "team_name")
          )

matchup_df <- matchup_df %>%
left_join(away_team_pitching_score_df,
          by = c("Away_Team" = "team_name")
          )
###############################calculate context Score#####################################################
   
matchup_df <- team_context_scores(matchup_df, starting_pitcher_season_df, batter_df, batter_splits_df, team_batting_df, team_travel_df)

###############################################calculate total score##########################################
# Handle Missing Pitchers
matchup_df <- matchup_df %>%
    mutate(
        Home_Pitcher_Score  = replace_na(Home_Pitcher_Score, 0),
        Home_Batting_Score  = replace_na(Home_Batting_Score, 0),
        Home_Pitching_Score = replace_na(Home_Pitching_Score, 0),
        Home_Context_Score  = replace_na(Home_Context_Score, 0),
        
        Away_Pitcher_Score  = replace_na(Away_Pitcher_Score, 0),
        Away_Batting_Score  = replace_na(Away_Batting_Score, 0),
        Away_Pitching_Score = replace_na(Away_Pitching_Score, 0),
        Away_Context_Score  = replace_na(Away_Context_Score, 0)
    )


matchup_df$Home_Pitcher_Score <- 1.0 * matchup_df$Home_Pitcher_Score
matchup_df$Home_Batting_Score <- 1.0 * matchup_df$Home_Batting_Score
matchup_df$Home_Pitching_Score <- 0.3 * matchup_df$Home_Pitching_Score
matchup_df$Home_Context_Score <- 0.3 * matchup_df$Home_Context_Score
matchup_df$Away_Pitcher_Score <- 1.0 * matchup_df$Away_Pitcher_Score
matchup_df$Away_Batting_Score <- 1.0 * matchup_df$Away_Batting_Score
matchup_df$Away_Pitching_Score <- 0.3 * matchup_df$Away_Pitching_Score
matchup_df$Away_Context_Score <- 0.3 * matchup_df$Away_Context_Score

    
home_total <- matchup_df$Home_Pitcher_Score + matchup_df$Home_Batting_Score + matchup_df$Home_Pitching_Score + matchup_df$Home_Context_Score
away_total <- matchup_df$Away_Pitcher_Score + matchup_df$Away_Batting_Score + matchup_df$Away_Pitching_Score + matchup_df$Away_Context_Score

matchup_df <- matchup_df %>%
mutate(Home_Total_Score = home_total,
       Away_Total_Score = away_total)

matchup_df <- matchup_df %>%
mutate(
    Predicted_Winner = case_when(
      Home_Total_Score > Away_Total_Score ~ Home_Team,
      Home_Total_Score < Away_Total_Score ~ Away_Team,
      TRUE ~ "Tie"
    ),
    Predicted_Loser = case_when(
      Home_Total_Score > Away_Total_Score ~ Away_Team,
      Home_Total_Score < Away_Total_Score ~ Home_Team,
      TRUE ~ "Tie"
    )
  )

matchup_df <- matchup_df %>%
mutate(Score_Difference = round(abs(matchup_df$Home_Total_Score - matchup_df$Away_Total_Score), 4) * 0.5
       )

################################### Calculate win prob ####################################
model <- readRDS("data/win_prob_model.rds")

matchup_df <- matchup_df %>%
mutate(
    Win_Probability = round((predict(model, newdata = matchup_df, type = "response") * 100), 2)
    )

matchup_df <- matchup_df %>%
  mutate(
    Win_Probability = if_else(
    Probable_Pitchers == "No" | !Pitcher_Stats_Available,
    NA_real_,
    Win_Probability
    ),
    Predicted_Winner = if_else(
        Probable_Pitchers == "No" | !Pitcher_Stats_Available,
        "No Prediction",
        Predicted_Winner
    ),
    Predicted_Loser = if_else(
        Probable_Pitchers == "No" | !Pitcher_Stats_Available,
        "No Prediction",
        Predicted_Loser
    )
  )
    
matchup_df <- matchup_df %>%
    mutate(
        Prediction_Status = case_when(
            Probable_Pitchers == "No" ~ "No Prediction",
            !Pitcher_Stats_Available ~ "No Prediction",
            Lineup_Hydration == "No" ~ "Not Hydrated Prediction",
            TRUE ~ "Full Prediction"
        )
    )

# round dispaly columns
matchup_df$Home_Total_Score <- round(matchup_df$Home_Total_Score, 2)
matchup_df$Away_Total_Score <- round(matchup_df$Away_Total_Score, 2)
# pitcher_data
starting_pitcher_season_df$WHIP <- round(starting_pitcher_season_df$WHIP, 2)
starting_pitcher_season_df$`K%` <- round(starting_pitcher_season_df$`K%` * 100, 2)
starting_pitcher_season_df$`BB%` <- round(starting_pitcher_season_df$`BB%` * 100, 2)
# team batting_data
team_batting_df$AVG <- round(team_batting_df$AVG, 3)
team_batting_df$SLG <- round(team_batting_df$SLG, 3)
team_batting_df$BABIP <- round(team_batting_df$BABIP, 3)
team_batting_df$OBP <- round(team_batting_df$OBP, 3)
team_batting_df$ISO <- round(team_batting_df$ISO, 3)

    
matchup_display_df <- matchup_df %>%
select(
    Game_ID,
    Game_Date,
    Game_Status,
    Game_Venue,
    Park_Factor,
    Game_Time,
    Day_Night,
    Home_Team,
    Home_Pitcher,
    Home_Pitcher_ID,
    Home_Pitcher_Hand,
    Home_Batting_Lineup,
    Away_Team,
    Away_Pitcher,
    Away_Pitcher_ID,
    Away_Pitcher_Hand,
    Away_Batting_Lineup,
    Home_Pitcher_Score,
    Away_Pitcher_Score,
    Home_Batting_Score,
    Away_Batting_Score,
    Home_Pitching_Score,
    Away_Pitching_Score,
    Home_Context_Score,
    Away_Context_Score,
    Home_Total_Score,
    Away_Total_Score,
    Predicted_Winner,
    Predicted_Loser,
    Score_Difference,
    Win_Probability,
    Home_Lineup_Hydrated,
    Away_Lineup_Hydrated,
    Prediction_Status
)
    
return(list(
    no_games = FALSE,
    matchup_display_df = matchup_display_df,
    starting_pitcher_season_df = starting_pitcher_season_df, 
    starting_pitcher_recent_df = starting_pitcher_recent_df,
    team_batting_df = team_batting_df,
    team_pitching_df = team_pitching_df,
    batter_df = batter_df,
    batter_splits_df = batter_splits_df,
    ball_park_factor = ball_park_factor,
    team_travel_df = team_travel_df
))
}
