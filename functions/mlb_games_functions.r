########################### ASSIGN STARTING PITCHER THROWING HANDS ###########################
assign_starting_pitcher_throwing_hands_wins_loses_era <- function(matchup_df, pitcher_df) {
    
    matchup_df <- matchup_df %>%
    mutate(
        Home_Pitcher_ID = as.character(Home_Pitcher_ID),
        Away_Pitcher_ID = as.character(Away_Pitcher_ID)
        )
        
    pitcher_df <- pitcher_df %>%
    mutate(
        xMLBAMID = as.character(xMLBAMID)
          ) %>%
    select(
        xMLBAMID,
        Throws
        )
    
    home_pitcher_df <- matchup_df %>%
    select(
        Home_Pitcher_ID
        )
    
    home_pitcher_df <- home_pitcher_df %>%
    left_join(
        pitcher_df,
        by=c('Home_Pitcher_ID' = 'xMLBAMID')
        ) %>%
    mutate(
        Throws = if_else(is.na(Throws), 'NA', Throws)
    ) %>%
    rename(
        Home_Pitcher_Hand = Throws
        ) %>%
    select(
        Home_Pitcher_ID,
        Home_Pitcher_Hand
        )
    
    away_pitcher_df <- matchup_df %>%
    select(
        Away_Pitcher_ID
        )
    
    away_pitcher_df <- away_pitcher_df %>%
    left_join(
        pitcher_df,
        by=c('Away_Pitcher_ID' = 'xMLBAMID')
        ) %>%
    mutate(
        Throws = if_else(is.na(Throws), 'NA', Throws)
    ) %>%
    rename(
        Away_Pitcher_Hand = Throws
        ) %>%
    select(
        Away_Pitcher_ID,
        Away_Pitcher_Hand
        )
    
    matchup_df <- matchup_df %>%
    left_join(home_pitcher_df, by = "Home_Pitcher_ID") %>%
    relocate(Home_Pitcher_Hand, .after = Home_Pitcher_ID) %>%
    left_join(away_pitcher_df, by = "Away_Pitcher_ID") %>%
    relocate(Away_Pitcher_Hand, .after = Away_Pitcher_ID)
    
    return(matchup_df)
}
######################## PARSE LINE UP ##############################
parse_lineup <- function(x) {
    x |>
      gsub("[{}]", "", x = _) |>
      strsplit(",") |>
      unlist() |>
      as.integer()
}

####################### ASSIGN HITTER LINEUPS LISTS TO TEAMS ############################
assign_batting_lineups_with_hydration_status <- function(matchup_df, team_batting_df) {
    
    matchup_df <- matchup_df %>%
      mutate(
        Game_Time_Stamp = with_tz(Game_Time_Stamp, "America/New_York")
      )
    
    home_table <- matchup_df %>%
    select(
      Game_ID,
      Home_Team,
      Game_Time_Stamp,
    ) %>%
    left_join(
      team_batting_df,
      by = c('Home_Team' = 'team_name')
    ) %>%
    mutate(
      Home_Lineup_Hydrated = if_else(
        (Game_Time_Stamp - minutes(65)) <= `update date`,
        'Yes',
        'No'
      )
    ) %>%
    rename(
      Home_Batting_Lineup = hitter_player_ids
    ) %>%
    select(
        Game_ID,
        Home_Batting_Lineup,
        Home_Lineup_Hydrated
    )
    
    
    away_table <- matchup_df %>%
    select(
      Game_ID,
      Away_Team,
      Game_Time_Stamp,
    ) %>%
    left_join(
      team_batting_df,
      by = c('Away_Team' = 'team_name')
    ) %>%
    mutate(
      Away_Lineup_Hydrated = if_else(
        (Game_Time_Stamp - minutes(65)) <= `update date`,
        'Yes',
        'No'
      )
    ) %>%
    rename(
      Away_Batting_Lineup = hitter_player_ids
    ) %>%
    select(
        Game_ID,
        Away_Batting_Lineup,
        Away_Lineup_Hydrated
    )
    
    matchup_df <- matchup_df %>%
    left_join(
      home_table,
      by = c('Game_ID')
    ) %>%
    left_join(
      away_table,
      by = c('Game_ID')
    )
    
    
    return(matchup_df)
    
    }
####################### ASSIGN HITTER LINEUPS LISTS TO TEAMS HISTORICAL ############################

assign_batting_lineups_with_hydration_status_historical <- function(matchup_df, team_batting_df) {

    matchup_df <- matchup_df %>%
      mutate(
        Game_Time_Stamp = with_tz(Game_Time_Stamp, "America/New_York")
      )
    home_table <- matchup_df %>%
        select(
            home_gamepk_team_name_label,
            Game_Time_Stamp,
        ) %>%
        left_join(
          team_batting_df,
          by = c('home_gamepk_team_name_label' = 'gamepk_team_name_label')
        ) %>%
        mutate(
          Home_Lineup_Hydrated = if_else(
            (Game_Time_Stamp - minutes(60)) <= `update date`,
            'Yes',
            'No'
          )
        ) %>%
        rename(
          Home_Batting_Lineup = hitter_player_ids
        ) %>%
        select(
            home_gamepk_team_name_label,
            Home_Batting_Lineup,
            Home_Lineup_Hydrated
        )
    
    
    away_table <- matchup_df %>% 
        select(
            away_gamepk_team_name_label,
            Game_Time_Stamp,
        ) %>%
        left_join(
            team_batting_df,
            by = c('away_gamepk_team_name_label' = 'gamepk_team_name_label')
        ) %>%
        mutate(
            Away_Lineup_Hydrated = if_else(
                (Game_Time_Stamp - minutes(60)) <= `update date`,
                'Yes',
                'No'
              )
        ) %>%
        rename(
            Away_Batting_Lineup = hitter_player_ids
        ) %>%
        select(
            away_gamepk_team_name_label,
            Away_Batting_Lineup,
            Away_Lineup_Hydrated
        )
    
    matchup_df <- matchup_df %>%
    left_join(
      home_table,
      by = c('home_gamepk_team_name_label')
    ) %>%
    left_join(
      away_table,
      by = c('away_gamepk_team_name_label')
    )


return(matchup_df)

}
################################# CALCULATE PITCHER SEASON SCORE##################################
starting_pitcher_season_scores <- function(starting_pitcher_season_df,
                                    season_benchmark_df) {
    # create pitcher score df
    score_table <- data.frame(pitcher = starting_pitcher_season_df$xMLBAMID)

    columns_list <- unique(season_benchmark_df$stat)

    for (stat in columns_list) {
        stat_benchmark <- season_benchmark_df[season_benchmark_df$stat == stat, ]
        min_stat <- stat_benchmark$min
        first_q_stat <- stat_benchmark$first_q
        second_q_stat <- stat_benchmark$second_q
        third_q_stat <- stat_benchmark$third_q
        max_stat <- stat_benchmark$max
        scale_points <- stat_benchmark$weight
        high_low <- stat_benchmark$high_low


        pitcher_stat_value <- starting_pitcher_season_df[[stat]]
        points <- numeric(length(pitcher_stat_value))
        
        if (high_low == "low") {
            points[pitcher_stat_value > max_stat]      <- 0.0
            points[pitcher_stat_value < max_stat]      <- 0.25
            points[pitcher_stat_value < third_q_stat]  <- 0.50
            points[pitcher_stat_value < second_q_stat] <- 0.75
            points[pitcher_stat_value < first_q_stat]  <- 1.00
        } else {
            points[pitcher_stat_value < min_stat]      <- 0.0
            points[pitcher_stat_value > min_stat]      <- 0.25
            points[pitcher_stat_value > first_q_stat]  <- 0.50
            points[pitcher_stat_value > second_q_stat] <- 0.75
            points[pitcher_stat_value > third_q_stat]  <- 1.00
        }


        score_table[[stat]] <- points * scale_points
    }
    score_table$Season_Score <- rowSums(score_table[, -1])
    final_season_score_table <- score_table %>%
        select(pitcher,
              Season_Score) %>%
      mutate(
        pitcher = as.character(pitcher)
      )
    return(final_season_score_table)
}

################################# CALCULATE PITCHER RECENT SCORE##################################
starting_pitcher_recent_scores <- function(starting_pitcher_recent_form_df,
                                    recent_form_benchmark) {
    
    recent_score_table <- data.frame(pitcher = starting_pitcher_recent_form_df$xMLBAMID)

    columns_list <- unique(recent_form_benchmark$stat)

    for (stat in columns_list) {
        stat_benchmark <- recent_form_benchmark[recent_form_benchmark$stat == stat, ]
        min_stat <- stat_benchmark$min
        first_q_stat <- stat_benchmark$first_q
        second_q_stat <- stat_benchmark$second_q
        third_q_stat <- stat_benchmark$third_q
        max_stat <- stat_benchmark$max
        scale_points <- stat_benchmark$weight
        high_low <- stat_benchmark$high_low


        pitcher_stat_value <- starting_pitcher_recent_form_df[[stat]]
        points <- numeric(length(pitcher_stat_value))
        if (high_low == "low") {
            points[pitcher_stat_value > max_stat]      <- 0.0
            points[pitcher_stat_value < max_stat]      <- 0.25
            points[pitcher_stat_value < third_q_stat]  <- 0.50
            points[pitcher_stat_value < second_q_stat] <- 0.75
            points[pitcher_stat_value < first_q_stat]  <- 1.00
        } else {
            points[pitcher_stat_value < min_stat]      <- 0.0
            points[pitcher_stat_value > min_stat]      <- 0.25
            points[pitcher_stat_value > first_q_stat]  <- 0.50
            points[pitcher_stat_value > second_q_stat] <- 0.75
            points[pitcher_stat_value > third_q_stat]  <- 1.00
        }

        recent_score_table[[stat]] <- points * scale_points
    }
    recent_score_table$Recent_Score <- rowSums(recent_score_table[ , -1])
    final_recent_score_table <- recent_score_table %>%
        select(pitcher, Recent_Score) %>%
      mutate(
        pitcher = as.character(pitcher)
      )

    starting_pitcher_recent_form_df <- starting_pitcher_recent_form_df %>%
    select(
        xMLBAMID,
        'Number of Starts'
        ) %>%
    mutate(
        xMLBAMID = as.character(xMLBAMID)
        )
    
    final_recent_score_table <- final_recent_score_table %>%
    left_join(starting_pitcher_recent_form_df, by=c('pitcher' = 'xMLBAMID'))

    return(final_recent_score_table)
}

############################### CALCULATE TOTAL PITCHER SCORE ##########################
starting_pitcher_total_score <- function(starting_pitcher_season_df,
                                         starting_pitcher_recent_form_df,
                                         label) {
 
    # combine both season and recent
    final_score_table <- starting_pitcher_season_df %>%
    left_join(starting_pitcher_recent_form_df, by='pitcher')

    # calculate pitcher complete score

    final_score_table <- final_score_table %>%
      mutate(
        !!label := case_when(
          `Number of Starts` == 3 ~ Season_Score * .7 + Recent_Score * .3,
          `Number of Starts` == 2 ~ Season_Score * .8 + Recent_Score * .2,
          `Number of Starts` == 1 ~ Season_Score * .9 + Recent_Score * .1,
          TRUE ~ Season_Score
        )
      )

    # Select pitcher + dynamically named score column
    final_score_table <- final_score_table %>%
      select(pitcher, all_of(label))

    return(final_score_table)
    }
##########################################################################################

################################# CALCULATE TEAM BATTING SCORE##################################
team_batting_scores <- function(team_batting_df, benchmark_df, label) {
    # create final score df
    score_table <- data.frame(team_name = team_batting_df[["team_name"]])
    columns_list <- unique(benchmark_df$stat)

    for (stat in columns_list) {
        stat_benchmark <- benchmark_df[benchmark_df$stat == stat, ]
        min_stat <- stat_benchmark$min
        first_q_stat <- stat_benchmark$first_q
        second_q_stat <- stat_benchmark$second_q
        third_q_stat <- stat_benchmark$third_q
        max_stat <- stat_benchmark$max
        scale_points <- stat_benchmark$weight
        high_low <- stat_benchmark$high_low


        team_batting_stat_value <- team_batting_df[[stat]]
        points <- numeric(length(team_batting_stat_value))
        
        if (high_low == "low") {
            points[team_batting_stat_value > max_stat]      <- 0.0
            points[team_batting_stat_value < max_stat]      <- 0.25
            points[team_batting_stat_value < third_q_stat]  <- 0.50
            points[team_batting_stat_value < second_q_stat] <- 0.75
            points[team_batting_stat_value < first_q_stat]  <- 1.00
        } else {
            points[team_batting_stat_value < min_stat]      <- 0.0
            points[team_batting_stat_value > min_stat]      <- 0.25
            points[team_batting_stat_value > first_q_stat]  <- 0.50
            points[team_batting_stat_value > second_q_stat] <- 0.75
            points[team_batting_stat_value > third_q_stat]  <- 1.00
        }
        
        score_table[[stat]] <- points * scale_points
    }
    score_table[[label]] <- rowSums(score_table[ , -1])
    final_score_table <- score_table %>%
        select(team_name, all_of(label))
    return(final_score_table)
    }

################################# CALCULATE TEAM BATTING SCORE HISTORICAL##################################
team_batting_scores_historical <- function(team_batting_df, benchmark_df, label) {
    # create final score df
    score_table <- data.frame(gamepk_team_name_label = team_batting_df[["gamepk_team_name_label"]])
    columns_list <- unique(benchmark_df$stat)

    for (stat in columns_list) {
        stat_benchmark <- benchmark_df[benchmark_df$stat == stat, ]
        min_stat <- stat_benchmark$min
        first_q_stat <- stat_benchmark$first_q
        second_q_stat <- stat_benchmark$second_q
        third_q_stat <- stat_benchmark$third_q
        max_stat <- stat_benchmark$max
        scale_points <- stat_benchmark$weight
        high_low <- stat_benchmark$high_low


        team_batting_stat_value <- team_batting_df[[stat]]
        points <- numeric(length(team_batting_stat_value))
        if (high_low == "low") {
            points[team_batting_stat_value > max_stat]      <- 0.0
            points[team_batting_stat_value < max_stat]      <- 0.25
            points[team_batting_stat_value < third_q_stat]  <- 0.50
            points[team_batting_stat_value < second_q_stat] <- 0.75
            points[team_batting_stat_value < first_q_stat]  <- 1.00
        } else {
            points[team_batting_stat_value < min_stat]      <- 0.0
            points[team_batting_stat_value > min_stat]      <- 0.25
            points[team_batting_stat_value > first_q_stat]  <- 0.50
            points[team_batting_stat_value > second_q_stat] <- 0.75
            points[team_batting_stat_value > third_q_stat]  <- 1.00
        }

        score_table[[stat]] <- points * scale_points
    }
    score_table[[label]] <- rowSums(score_table[ , -1])
    final_score_table <- score_table %>%
        select(gamepk_team_name_label, all_of(label))
    return(final_score_table)
    }
################################# CALCULATE TEAM PITCHING SCORE##################################
team_pitching_scores <- function(team_pitching_df, benchmark_df, label) {
    # create final score df
    score_table <- data.frame(team_name = team_pitching_df[["team_name"]])
    columns_list <- unique(benchmark_df$stat)

    for (stat in columns_list) {
        stat_benchmark <- benchmark_df[benchmark_df$stat == stat, ]
        min_stat <- stat_benchmark$min
        first_q_stat <- stat_benchmark$first_q
        second_q_stat <- stat_benchmark$second_q
        third_q_stat <- stat_benchmark$third_q
        max_stat <- stat_benchmark$max
        scale_points <- stat_benchmark$weight
        high_low <- stat_benchmark$high_low


        team_pitching_stat_value <- team_pitching_df[[stat]]
        points <- numeric(length(team_pitching_stat_value))
        if (high_low == "low") {
            points[team_pitching_stat_value > max_stat]      <- 0.0
            points[team_pitching_stat_value < max_stat]      <- 0.25
            points[team_pitching_stat_value < third_q_stat]  <- 0.50
            points[team_pitching_stat_value < second_q_stat] <- 0.75
            points[team_pitching_stat_value < first_q_stat]  <- 1.00
        } else {
            points[team_pitching_stat_value < min_stat]      <- 0.0
            points[team_pitching_stat_value > min_stat]      <- 0.25
            points[team_pitching_stat_value > first_q_stat]  <- 0.50
            points[team_pitching_stat_value > second_q_stat] <- 0.75
            points[team_pitching_stat_value > third_q_stat]  <- 1.00
        }

        score_table[[stat]] <- points * scale_points
    }
    score_table[[label]] <- rowSums(score_table[ , -1])
    final_score_table <- score_table %>%
        select(team_name, all_of(label))
    return(final_score_table)
    }

################################# CALCULATE TEAM PITCHING SCORE HISTORICAL##################################
team_pitching_scores_historical <- function(team_pitching_df, benchmark_df, label) {
    # create final score df
    score_table <- data.frame(gamepk_team_name_label = team_pitching_df[["gamepk_team_name_label"]])
    columns_list <- unique(benchmark_df$stat)

    for (stat in columns_list) {
        stat_benchmark <- benchmark_df[benchmark_df$stat == stat, ]
        min_stat <- stat_benchmark$min
        first_q_stat <- stat_benchmark$first_q
        second_q_stat <- stat_benchmark$second_q
        third_q_stat <- stat_benchmark$third_q
        max_stat <- stat_benchmark$max
        scale_points <- stat_benchmark$weight
        high_low <- stat_benchmark$high_low


        team_pitching_stat_value <- team_pitching_df[[stat]]
        points <- numeric(length(team_pitching_stat_value))
        if (high_low == "low") {
            points[team_pitching_stat_value > max_stat]      <- 0.0
            points[team_pitching_stat_value < max_stat]      <- 0.25
            points[team_pitching_stat_value < third_q_stat]  <- 0.50
            points[team_pitching_stat_value < second_q_stat] <- 0.75
            points[team_pitching_stat_value < first_q_stat]  <- 1.00
        } else {
            points[team_pitching_stat_value < min_stat]      <- 0.0
            points[team_pitching_stat_value > min_stat]      <- 0.25
            points[team_pitching_stat_value > first_q_stat]  <- 0.50
            points[team_pitching_stat_value > second_q_stat] <- 0.75
            points[team_pitching_stat_value > third_q_stat]  <- 1.00
        }

        score_table[[stat]] <- points * scale_points
    }
    score_table[[label]] <- rowSums(score_table[ , -1])
    final_score_table <- score_table %>%
        select(gamepk_team_name_label, all_of(label))
    return(final_score_table)
    }
######################### CALCULATE PLATTON ADVANTAGE SCORE########################
    
calculate_platoon_splits_advantage <-function(matchup_df, batter_df, batter_splits_df) {
    matchup_df <- matchup_df %>%
      mutate(
        Home_Team_Lineup = lapply(Home_Batting_Lineup, parse_lineup),
        Away_Team_Lineup = lapply(Away_Batting_Lineup, parse_lineup)
      )
      
    matchup_df$Home_Team_Platoon_Splits <- numeric(nrow(matchup_df))
    matchup_df$Away_Team_Platoon_Splits <- numeric(nrow(matchup_df))

    

    
    for (row in 1:nrow(matchup_df)) {
         
        home_player_ids <- batter_df %>%
        filter(batter_id %in% matchup_df$Home_Team_Lineup[[row]]) %>%
        select(batter_id, playerid) %>%
        left_join(batter_splits_df, by =c('playerid' = 'playerId'))

        away_player_ids <- batter_df %>%
        filter(xMLBAMID %in% matchup_df$Away_Team_Lineup[[row]]) %>%
        select(xMLBAMID, playerid) %>%
        left_join(batter_splits_df, by =c('playerid' = 'playerId'))
        
        team_wOBA_splits <- sum(home_player_ids$wOBA_splits, na.rm=TRUE) * 1.0
        team_ISO_splits <- sum(home_player_ids$ISO_splits, na.rm=TRUE) * 0.5
        team_BB_perc_splits <- sum(home_player_ids$`BB%_splits`, na.rm=TRUE) * 0.2
        team_K_perc_splits <- sum(home_player_ids$`K%_splits`, na.rm=TRUE) * -0.3
        
        team_platoon_advantage = (
            team_wOBA_splits +
            team_ISO_splits + 
            team_BB_perc_splits + 
            team_K_perc_splits
            )
        
        away_throwing_hand <- matchup_df$Away_Pitcher_Hand[row]
        
        if (away_throwing_hand == 'NA') {
          matchup_df$Home_Context_Score[row] = matchup_df$Home_Context_Score[row] + 0
        } else if (away_throwing_hand == 'L' && team_platoon_advantage > 0) {
            matchup_df$Home_Context_Score[row] = matchup_df$Home_Context_Score[row] + abs(team_platoon_advantage) * 0.5
        } else if (away_throwing_hand == 'R' && team_platoon_advantage < 0) {
            matchup_df$Home_Context_Score[row] = matchup_df$Home_Context_Score[row] + abs(team_platoon_advantage) * 0.5
        } else {
            matchup_df$Home_Context_Score[row] = matchup_df$Home_Context_Score[row] + 0
        }

        matchup_df$Home_Team_Platoon_Splits[row] <- round(team_platoon_advantage, 4)
        
        team_wOBA_splits <- sum(away_player_ids$wOBA_splits, na.rm=TRUE) * 1.0
        team_ISO_splits <- sum(away_player_ids$ISO_splits, na.rm=TRUE) * 0.5
        team_BB_perc_splits <- sum(away_player_ids$`BB%_splits`, na.rm=TRUE) * 0.2
        team_K_perc_splits <- sum(away_player_ids$`K%_splits`, na.rm=TRUE) * -0.3
        
        team_platoon_advantage = (
            team_wOBA_splits +
            team_ISO_splits + 
            team_BB_perc_splits + 
            team_K_perc_splits
            )
        
        home_throwing_hand <- matchup_df$Home_Pitcher_Hand[row]
        
        if (home_throwing_hand == 'NA') {
          matchup_df$Away_Context_Score[row] = matchup_df$Away_Context_Score[row] + 0
        } else if (home_throwing_hand == 'L' && team_platoon_advantage > 0) {
            matchup_df$Away_Context_Score[row] = matchup_df$Away_Context_Score[row] + abs(team_platoon_advantage) * 0.5
        } else if (home_throwing_hand == 'R' && team_platoon_advantage < 0) {
            matchup_df$Away_Context_Score[row] = matchup_df$Away_Context_Score[row] + abs(team_platoon_advantage) * 0.5
        } else {
            matchup_df$Away_Context_Score[row] = matchup_df$Away_Context_Score[row] + 0
        }

        matchup_df$Away_Team_Platoon_Splits[row] <- round(team_platoon_advantage, 4)
    }
    
    return(matchup_df)

}
############################# CALCULATE FATIGUE SCORE ####################################
calculate_team_travel_fatigue_score <- function(matchup_df, team_travel_df) {

    team_travel_df <- team_travel_df %>%
        mutate(
            fatigue_score = replace_na(fatigue_score, 0)
            )
    
    matchup_df <- matchup_df %>%
        left_join(
            team_travel_df %>% select(team_name, fatigue_score),
            by = c("Home_Team" = "team_name")
        ) %>%
        rename(Home_Fatigue_Score = fatigue_score) %>%
        
        left_join(
            team_travel_df %>% select(team_name, fatigue_score),
            by = c("Away_Team" = "team_name")
        ) %>%
        rename(Away_Fatigue_Score = fatigue_score) %>%
        
        # Add fatigue into context scores
        mutate(
            Home_Fatigue_Adjust = pmax(pmin(Home_Fatigue_Score, 1), -1),
            Away_Fatigue_Adjust = pmax(pmin(Away_Fatigue_Score, 1), -1),
        
            Home_Context_Score = Home_Context_Score - Home_Fatigue_Adjust,
            Away_Context_Score = Away_Context_Score - Away_Fatigue_Adjust
        ) %>%
 
        # Drop the temporary fatigue columns
        select(-Home_Fatigue_Score, -Away_Fatigue_Score)
    
    return(matchup_df)
}

############################# CALCULATE FATIGUE SCORE HISTORICAL####################################
calculate_team_travel_fatigue_score_historical <- function(matchup_df, team_travel_df) {

    team_travel_df <- team_travel_df %>%
        mutate(
            fatigue_score = replace_na(fatigue_score, 0)
            )
    
    matchup_df <- matchup_df %>%
        left_join(
            team_travel_df %>% select(team_name, fatigue_score),
            by = c("Home_Team" = "team_name")
        ) %>%
        rename(Home_Fatigue_Score = fatigue_score) %>%
        
        left_join(
            team_travel_df %>% select(team_name, fatigue_score),
            by = c("Away_Team" = "team_name")
        ) %>%
        rename(Away_Fatigue_Score = fatigue_score) %>%
        
        # Add fatigue into context scores
        mutate(
            Home_Fatigue_Adjust = 0,
            Away_Fatigue_Adjust = 0,
        
            Home_Context_Score = Home_Context_Score - Home_Fatigue_Adjust,
            Away_Context_Score = Away_Context_Score - Away_Fatigue_Adjust
        ) %>%
 
        # Drop the temporary fatigue columns
        select(-Home_Fatigue_Score, -Away_Fatigue_Score)
    
    return(matchup_df)
}
############################ CALCULATE POWER SYNERGY BOOST ############################
calculate_power_synergy_boost <- function(matchup_df, pitcher_df, team_batting_df) {

    pitcher_df <- pitcher_df%>%
    mutate(
        xMLBAMID = as.character(xMLBAMID)
           )

    
    #### HOME TEAM ####
    home_team_df <- matchup_df %>%
    select(
      Game_ID,
      Home_Team,
      Away_Pitcher_ID,
      Park_Factor
    )
    
    home_opposing_pitcher_df <- pitcher_df %>%
    filter(xMLBAMID %in% home_team_df$Away_Pitcher_ID) %>%
    select(
      xMLBAMID,
      `GB%`,
      `HR/9`,
      `FB%`
    )
    
    home_team_batting_df <- team_batting_df %>%
    filter(team_name %in% home_team_df$Home_Team) %>%
    select(
      team_name,
      ISO,
      SLG
    )
    
    home_team_df <- home_team_df %>%
    left_join(
      home_opposing_pitcher_df,
      by = c("Away_Pitcher_ID" = "xMLBAMID")
    ) %>%
    left_join(
      home_team_batting_df,
      by = c("Home_Team" = "team_name")
    ) %>%
    mutate(
      pitcher_flag = if_else(
          is.na(`GB%`) | is.na(`FB%`) | is.na(`HR/9`),
            FALSE,
            (`GB%` < .40) | (`FB%` >= 0.38) | (`HR/9` >= 1.1)
          ),
      batting_flag = (ISO >= 0.165) & (SLG >= 0.420),
      park_flag = if_else(is.na(Park_Factor), FALSE, Park_Factor >= 103),
      home_power_synergy_score = if_else(
        pitcher_flag & batting_flag & park_flag,
        0.10,
        0
      )
    )
    
    #### AWAY TEAM ####
    away_team_df <- matchup_df %>%
    select(
      Game_ID,
      Away_Team,
      Home_Pitcher_ID,
      Park_Factor
    )
    
    away_opposing_pitcher_df <- pitcher_df %>%
    filter(xMLBAMID %in% away_team_df$Home_Pitcher_ID) %>%
    select(
      xMLBAMID,
      `GB%`,
      `HR/9`,
      `FB%`
    )
    
    away_team_batting_df <- team_batting_df %>%
    filter(team_name %in% away_team_df$Away_Team) %>%
    select(
      team_name,
      ISO,
      SLG
    )
    
    away_team_df <- away_team_df %>%
    left_join(
      away_opposing_pitcher_df,
      by = c("Home_Pitcher_ID" = "xMLBAMID")
    ) %>%
    left_join(
      away_team_batting_df,
      by = c("Away_Team" = "team_name")
    ) %>%
    mutate(
      pitcher_flag = if_else(
          is.na(`GB/FB`) | is.na(`FB%`) | is.na(`HR/9`),
            FALSE,
            (`GB/FB` < 1.0) | (`FB%` >= 0.38) | (`HR/9` >= 1.1)
          ),
      batting_flag = (ISO >= 0.165) & (SLG >= 0.420),
      park_flag = if_else(is.na(Park_Factor), FALSE, Park_Factor >= 103),
      away_power_synergy_score = if_else(
        pitcher_flag & batting_flag & park_flag,
        0.10,
        0
      )
    )
    
    #### COMBINE ####
    complete_df <- home_team_df %>%
    select(Game_ID, home_power_synergy_score) %>%
    left_join(
      away_team_df %>% select(Game_ID, away_power_synergy_score),
      by = "Game_ID"
    )
    
    matchup_df <- matchup_df %>%
    left_join(complete_df, by = "Game_ID") %>%
    mutate(
      Home_Context_Score = Home_Context_Score + home_power_synergy_score,
      Away_Context_Score = Away_Context_Score + away_power_synergy_score
    ) %>%
    select(
      -home_power_synergy_score,
      -away_power_synergy_score
    )
    
    return(matchup_df)
}

############################ CALCULATE POWER SYNERGY BOOST HISTORICAL ############################
calculate_power_synergy_boost_historical <- function(matchup_df, pitcher_df, team_batting_df) {

    pitcher_df <- pitcher_df%>%
    mutate(
        xMLBAMID = as.character(xMLBAMID)
           )

    
    #### HOME TEAM ####
    home_team_df <- matchup_df %>%
    select(
        home_gamepk_team_name_label,
        Game_ID,
        Away_Pitcher_ID,
        Park_Factor
    )
    
    home_opposing_pitcher_df <- pitcher_df %>%
    filter(xMLBAMID %in% home_team_df$Away_Pitcher_ID) %>%
    select(
      xMLBAMID,
      `GB/FB`,
      `HR/9`,
      `FB%`
    )
    
    home_team_batting_df <- team_batting_df %>%
      filter(
          gamepk_team_name_label %in% matchup_df$home_gamepk_team_name_label
      ) %>%
      select(gamepk_team_name_label, ISO, SLG)

    
    home_team_df <- home_team_df %>%
    left_join(
      home_opposing_pitcher_df,
      by = c("Away_Pitcher_ID" = "xMLBAMID")
    ) %>%
    left_join(
      home_team_batting_df,
      by = c("home_gamepk_team_name_label" = "gamepk_team_name_label")
    ) %>%
    mutate(
      pitcher_flag = if_else(
          is.na(`GB/FB`) | is.na(`FB%`) | is.na(`HR/9`),
            FALSE,
            (`GB/FB` < 1.0) | (`FB%` >= 0.38) | (`HR/9` >= 1.1)
          ),
      batting_flag = (ISO >= 0.165) & (SLG >= 0.420),
      park_flag = if_else(is.na(Park_Factor), FALSE, Park_Factor >= 103),
      home_power_synergy_score = if_else(
        pitcher_flag & batting_flag & park_flag,
        0.10,
        0
      )
    )
    
    #### AWAY TEAM ####
    away_team_df <- matchup_df %>%
    select(
        away_gamepk_team_name_label,
        Game_ID,
        Home_Pitcher_ID,
        Park_Factor
        )  
    
    
    away_opposing_pitcher_df <- pitcher_df %>%
    filter(xMLBAMID %in% away_team_df$Home_Pitcher_ID) %>%
    select(
      xMLBAMID,
      `GB/FB`,
      `HR/9`,
      `FB%`
    )
    
    away_team_batting_df <- team_batting_df %>%
      filter(
          gamepk_team_name_label %in% matchup_df$away_gamepk_team_name_label
      ) %>%
      select(gamepk_team_name_label, ISO, SLG)
    
    away_team_df <- away_team_df %>%
    left_join(
      away_opposing_pitcher_df,
      by = c("Home_Pitcher_ID" = "xMLBAMID")
    ) %>%
    left_join(
      away_team_batting_df,
      by = c("away_gamepk_team_name_label" = "gamepk_team_name_label")
    ) %>%
    mutate(
      pitcher_flag = if_else(
          is.na(`GB/FB`) | is.na(`FB%`) | is.na(`HR/9`),
            FALSE,
            (`GB/FB` < 1.0) | (`FB%` >= 0.38) | (`HR/9` >= 1.1)
          ),
      batting_flag = (ISO >= 0.165) & (SLG >= 0.420),
      park_flag = if_else(is.na(Park_Factor), FALSE, Park_Factor >= 103),
      away_power_synergy_score = if_else(
        pitcher_flag & batting_flag & park_flag,
        0.10,
        0
      )
    )
    
    #### COMBINE ####
    complete_df <- home_team_df %>%
    select(Game_ID, home_gamepk_team_name_label, home_power_synergy_score) %>%
    left_join(
      away_team_df %>% select(Game_ID, away_gamepk_team_name_label, away_power_synergy_score),
      by = "Game_ID"
    )
    
    matchup_df <- matchup_df %>%
    left_join(complete_df, by = "Game_ID") %>%
    mutate(
      Home_Context_Score = Home_Context_Score + home_power_synergy_score,
      Away_Context_Score = Away_Context_Score + away_power_synergy_score
    ) %>%
    select(
      -home_power_synergy_score,
      -away_power_synergy_score
    )
    
    return(matchup_df)
}


    
############################ CALCULATE CONTEXT SCORE#######################################
team_context_scores <- function(matchup_df, starting_pitcher_season_df, batter_df, batter_splits_df, team_batting_df, team_travel_df)  {
    matchup_df <- matchup_df %>%
    mutate(Home_Context_Score = 0.1,
          Away_Context_Score = 0)


    # compute platoon splits advantage and add to score
    #matchup_df <- calculate_platoon_splits_advantage(matchup_df, batter_df, batter_splits_df)
    # add in fatigue scores to context score
    #matchup_df <- calculate_team_travel_fatigue_score(matchup_df, team_travel_df)
    # add in power synergy boost
    #matchup_df <- calculate_power_synergy_boost(matchup_df, starting_pitcher_season_df, team_batting_df) 


    return(matchup_df)

}

############################ CALCULATE CONTEXT SCORE HISTORICAL #######################################
team_context_scores_historical <- function(matchup_df, starting_pitcher_season_df, batter_df, batter_splits_df, team_batting_df, team_travel_df)  {
    matchup_df <- matchup_df %>%
    mutate(Home_Context_Score = 0.1,
          Away_Context_Score = 0)


    # compute platoon splits advantage and add to score
    #matchup_df <- calculate_platoon_splits_advantage(matchup_df, batter_df, batter_splits_df)
    # add in fatigue scores to context score
    #matchup_df <- calculate_team_travel_fatigue_score(matchup_df, team_travel_df)
    # add in power synergy boost
    #matchup_df <- calculate_power_synergy_boost(matchup_df, starting_pitcher_season_df, team_batting_df) 


    return(matchup_df)

}

#################### CHECK COLOR FOR SPLITS ###################
check_color_for_platoon_splits <- function(platoon_splits, opposing_pitcher_hand) {

    platoon_splits <- as.numeric(platoon_splits)[1]
    opposing_pitcher_hand <- as.character(opposing_pitcher_hand)[1]

    if (is.na(platoon_splits) || is.na(opposing_pitcher_hand)) {
        return("black")
    }

    if ((platoon_splits >= 0 && opposing_pitcher_hand == "L") ||
        (platoon_splits < 0  && opposing_pitcher_hand == "R")) {
        return("green")
    } else {
        return("red")
    }
}


################# CHECK COLOR FOR STAT HIGH###################
check_color_for_stat_high <- function(stat_x, stat_y) {
    if (stat_x > stat_y) {
        return('green')
    } else if (stat_x < stat_y) {
        return('red')
    } else return('black')
}
################ CHECK COLOR FOR STAT LOW ##################

check_color_for_stat_low <- function(stat_x, stat_y) {
    if (stat_x < stat_y) {
        return('green')
    } else if (stat_x > stat_y) {
        return('red')
    } else return('black')
}

############### CREATE CORRECT PICK PLOT########################

make_correct_pick_plot <- function() {
    con <- dbConnect(RPostgres::Postgres(),
                dbname = 'neondb',
                host = 'ep-crimson-forest-ahw3nquq-pooler.c-3.us-east-1.aws.neon.tech',
                port = 5432,
                user = 'neondb_owner',
                password = 'npg_gW6jAT5xvnbc')

    query <- 'SELECT * from mlb_matchups'
    
    results <- dbSendQuery(con, query)
    
    data <- dbFetch(results)
    
    filtered_data <- data %>%
    filter(
        Predicted_Winner != 'No Prediction'
        )
    
    filtered_data <- filtered_data %>%
    mutate(
        Winner = if_else(
            Home_Team_Winner == 'TRUE',
            Home_Team,
            Away_Team),
        Correct_Pick = if_else(
            Predicted_Winner == Winner,
            1,
            0)
        )
    
    winner_data <- filtered_data %>%
    filter(Correct_Pick == 1)
    
    win_perc <- round((nrow(winner_data) / nrow(filtered_data)), 2) * 100
    
    
    results_table <- winner_data %>%
      group_by(Winner) %>%
      summarise(total_count = sum(Correct_Pick), .groups = "drop") %>%
      mutate(
        team_color = map_chr(Winner, ~ team_colors[[.x]]),
        team_logos = map_chr(Winner, ~ team_logos[[.x]]),
        correct_win_perc = win_perc
      )
    
    # set variables
    
    legend_title <- paste0("Correct Pick Percentage: ", win_perc, " %")
    
    gg <- ggplot(results_table, aes(x = Winner, y = total_count)) +
      geom_col(aes(fill = Winner), color = "gray20", width = 0.75) +
      scale_fill_manual(name = legend_title,
                        values = team_colors,
                       ) +
      
      geom_image(
        aes(image = team_logos, 
            y = total_count + max(total_count) * 0.05),
        size = 0.10
      ) +
      scale_y_continuous(
          breaks = function(x) floor(min(x)):ceiling(max(x))
          ) + 
      labs(x = "Teams", y = "Correct Picks") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    return(gg)
}