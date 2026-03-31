

source("functions/data_extract_functions.r")
source("functions/mlb_games_functions.r")
source("functions/mlb_games_prediction_function.r")
source("data/mlb_logos.r")


ui <- fluidPage(
    h1("Baseball Games"),

    tabsetPanel(
        tabPanel("Baseball Games",
            uiOutput("main_panel") 
        ),
        
        tabPanel("Matchup Details",
            selectInput("selected_game", "Choose matchup:", choices = NULL),
            uiOutput("details_panel"),
            downloadButton("download_full_matchup", "Download Matchup Workbook")
        ),

        tabPanel("Download All Data",
            h3("Download Complete Dataset"),
            downloadButton("download_all_data", "Download All Tables")
        )
    )
)


server <- function(input, output, session) {
    # if no games
    games <- mlb_games(Sys.Date())

    if (games$no_games) {
        output$main_panel <- renderUI({
            div(
                style = "padding: 20px;",
                h2("NO MLB GAMES TODAY"),
                p("Check back tomorrow.")
            )
        })
        return()
    }

    
    
    matchup_df      <- games$matchup_display_df
    starting_pitcher_season_df      <- games$starting_pitcher_season_df
    starting_pitcher_recent_df      <- games$starting_pitcher_recent_df
    team_batting_df <- games$team_batting_df
    team_pitching_df <- games$team_pitching_df
    batter_df       <- games$batter_df
    batter_splits_df <- games$batter_splits_df
    ballpark_df     <- games$ball_park_factor
    team_travel_df <- games$team_travel_df



    
    day_icon <- "☀️"
    night_icon <- "🌑"
    check_mark <- "✅"
    x_mark <- "❌"
    
    games <- paste(matchup_df$Home_Team, "vs", matchup_df$Away_Team)
    updateSelectInput(session, "selected_game", choices = games)


    filtered_matchup <- reactive({
        req(input$selected_game)
    
        df <- matchup_df
        df$label <- paste(df$Home_Team, "vs", df$Away_Team)
        game <- df[df$label == input$selected_game, ]
    
        home_pitcher_season <- subset(starting_pitcher_season_df, xMLBAMID == game$Home_Pitcher_ID)
        home_pitcher_recent <- subset(starting_pitcher_recent_df, xMLBAMID == game$Home_Pitcher_ID)
        away_pitcher_season <- subset(starting_pitcher_season_df, xMLBAMID == game$Away_Pitcher_ID)
        away_pitcher_recent <- subset(starting_pitcher_recent_df, xMLBAMID == game$Away_Pitcher_ID)
        
        if (!nrow(home_pitcher_season)) home_pitcher <- data.frame()
        if (!nrow(away_pitcher_season)) away_pitcher <- data.frame()
        if (!nrow(home_pitcher_recent)) home_pitcher <- data.frame()
        if (!nrow(away_pitcher_recent)) away_pitcher <- data.frame()
    
        pitchers_season <- rbind(home_pitcher_season, away_pitcher_season)
        pitchers_recent <- rbind(home_pitcher_recent, away_pitcher_recent)
    
        home_team_batting_stats  <- subset(team_batting_df,  team_name == game$Home_Team)
        away_team_batting_stats  <- subset(team_batting_df,  team_name == game$Away_Team)
        home_team_pitching_stats <- subset(team_pitching_df, team_name == game$Home_Team)
        away_team_pitching_stats <- subset(team_pitching_df, team_name == game$Away_Team)
    
        current_ballpark_df <- ballpark_df[
            ballpark_df$Venue == game$Game_Venue &
            ballpark_df$Day_Night == game$Day_Night,
        ]

        home_travel <- subset(team_travel_df, team_name == game$Home_Team)
        away_travel <- subset(team_travel_df, team_name == game$Away_Team)
        matchup_travel_df <- rbind(home_travel, away_travel)
        
        list(
            game = game,
            pitchers_season = pitchers_season,
            pitchers_recent = pitchers_recent,
            home_batting = home_team_batting_stats,
            away_batting = away_team_batting_stats,
            home_pitching = home_team_pitching_stats,
            away_pitching = away_team_pitching_stats,
            ballpark = current_ballpark_df,
            team_travel = matchup_travel_df
        )
    })

    output$download_all_data <- downloadHandler(
        filename = function() {
            paste0("all_mlb_data_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write_xlsx(
                list(
                    Matchups       = matchup_df,
                    Pitchers_Season_Form       = starting_pitcher_season_df,
                    Pitchers_Recent_Form       = starting_pitcher_recent_df,
                    Team_Batting   = team_batting_df,
                    Team_Pitching  = team_pitching_df,
                    Batters        = batter_df,
                    Batter_Splits  = batter_splits_df,
                    Ballpark       = ballpark_df,
                    Team_Travel    = team_travel_df
                ),
                file
            )
        }
    )


    output$download_full_matchup <- downloadHandler(
        filename = function() {
            data <- filtered_matchup()
            game <- data$game
            paste0(game$Home_Team, "_vs_", game$Away_Team, "_full_matchup.xlsx")
        },
        content = function(file) {
            data <- filtered_matchup()
    
            write_xlsx(
                list(
                    Matchup  = data$game,
                    Pitchers_Season_Form = (data$pitchers_season),
                    Pitchers_Recent_Form = (data$pitchers_recent),
                    Batting  = rbind(data$home_batting, data$away_batting),
                    Pitching = rbind(data$home_pitching, data$away_pitching),
                    Ballpark = data$ballpark,
                    Travel = data$team_travel
                ),
                file
            )
        }
    )


    
    output$main_panel <- renderUI({
    df <- matchup_df

    panels <- lapply(1:nrow(df), function(i) {
        matchup <- df[i, ]

        home_logo <- tags$img(src = team_logos[[matchup$Home_Team]], width = "80px")
        away_logo <- tags$img(src = team_logos[[matchup$Away_Team]], width = "80px")
        winner_logo <- tags$img(src = team_logos[[matchup$Predicted_Winner]], width = "60px")

        game_day_or_night_logo <- if (matchup$Day_Night == "day") day_icon else night_icon
        game_status_mark <- if (matchup$Game_Status == "Pre-Game") check_mark else x_mark
            
        local({
            ii <- i

            prob <- df$Win_Probability[ii] / 100
            home_color <- team_colors[[df$Home_Team[ii]]]
            away_color <- team_colors[[df$Away_Team[ii]]]
            winning_team <- df$Predicted_Winner[ii]

            # Only compute fill_xmin if prediction exists
            if (winning_team != "No Prediction") {
                if (winning_team == df$Home_Team[ii]) {
                    fill_xmin <- prob
                } else {
                    fill_xmin <- 1 - prob
                }
            }

            output[[paste0("win_prob_bar_", ii)]] <- renderPlot({

                # CASE 1: No Prediction → full gray bar
                if (winning_team == "No Prediction") {
                    ggplot() +
                        geom_rect(
                            aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
                            fill = "gray80",
                            color = "gray40",
                            linewidth = 0.5
                        ) +
                        scale_x_continuous(expand = c(0, 0)) +
                        scale_y_continuous(expand = c(0, 0)) +
                        theme_void() +
                        theme(
                            plot.margin = margin(0, 0, 0, 0),
                            panel.spacing = unit(0, "pt")
                        )

                # CASE 2: Normal prediction
                } else {
                    ggplot() +
                        geom_rect(
                            aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
                            fill = home_color,
                            color = "gray40",
                            linewidth = 0.5
                        ) +
                        geom_rect(
                            aes(xmin = fill_xmin, xmax = 1, ymin = 0, ymax = 1),
                            fill = away_color,
                            color = NA
                        ) +
                        geom_segment(
                            aes(x = fill_xmin, xend = fill_xmin, y = 0, yend = 1),
                            color = "white",
                            linewidth = 1
                        ) +
                        scale_x_continuous(expand = c(0, 0)) +
                        scale_y_continuous(expand = c(0, 0)) +
                        theme_void() +
                        theme(
                            plot.margin = margin(0, 0, 0, 0),
                            panel.spacing = unit(0, "pt")
                        )
                }
            })
        })

        # Prediction status color
        if (matchup$Prediction_Status == "Full Prediction") {
            prediction_status <- "green"
        } else if (matchup$Prediction_Status == "Not Hydrated Prediction") {
            prediction_status <- "orange"
        } else {
            prediction_status <- "red"
        }

        tagList(
            fluidRow(
                column(width = 3,
                    div(
                        style = "
                            display: flex;
                            flex-direction: column;
                            align-items: center;
                            justify-content: center;
                        ",
                        img(home_logo),
                        div(style = 'font-weight: 600; margin-top: 4px;', matchup$Home_Team),
                        div(paste0(matchup$Home_Pitcher, " (", matchup$Home_Pitcher_Hand, ")")),
                        div(paste0("W-L: ", matchup$Home_Pitcher_Wins, " - ", matchup$Home_Pitcher_Losses)),
                        div(paste0('ERA: ', matchup$Home_Pitcher_ERA))
                    )
                ),

                column(width = 6,
                    p("Game Date: ", matchup$Game_Date),
                    p("Game Time: ", matchup$Game_Time),
                    p("Game Status: ", matchup$Game_Status, " ", game_status_mark),
                    p("Ball Park: ", matchup$Game_Venue),
                    p("Day / Night: ", game_day_or_night_logo, " ", matchup$Day_Night),
                    p("Predicted Winner: ", strong(matchup$Predicted_Winner), " ", winner_logo),
                    p("Win Probability: ",
                      ifelse(is.na(matchup$Win_Probability), "NA", paste0(matchup$Win_Probability, "%"))
                    ),
                    plotOutput(paste0("win_prob_bar_", i), height = "20px"),
                    p(
                        span(matchup$Prediction_Status,
                            style = paste0("color:", prediction_status, "; font-weight:bold;")
                        )
                    )
                ),

                column(width = 3,
                    div(
                        style = "
                            display: flex;
                            flex-direction: column;
                            align-items: center;
                            justify-content: center;
                        ",
                        img(away_logo),
                        div(style = 'font-weight: 600; margin-top: 4px;', matchup$Away_Team),
                        div(paste0(matchup$Away_Pitcher, " (", matchup$Away_Pitcher_Hand, ")")),
                        div(paste0("W-L: ", matchup$Away_Pitcher_Wins, " - ", matchup$Away_Pitcher_Losses)),
                        div(paste0('ERA: ', matchup$Away_Pitcher_ERA))
                    )
                )
            ),
            hr()
        )
    })

    tagList(panels)
})

    output$details_panel <- renderUI({
        req(input$selected_game)
    
        # use filtered matchup function
        data <- filtered_matchup()

        game <- data$game
        
        pitchers_season <- data$pitchers_season
        pitchers_recent <- data$pitchers_recent
        
        home_pitcher_stats <- subset(pitchers_season, xMLBAMID == game$Home_Pitcher_ID)
        away_pitcher_stats <- subset(pitchers_season, xMLBAMID == game$Away_Pitcher_ID)
        
        home_team_batting_stats  <- data$home_batting
        away_team_batting_stats  <- data$away_batting
        
        home_team_pitching_stats <- data$home_pitching
        away_team_pitching_stats <- data$away_pitching
        
        ballpark <- data$ballpark

    
        # extract home/away rows from combined tables
       
    
        game_day_or_night_logo <- if (game$Day_Night == "day") day_icon else night_icon
        #status_mark <- if (game$Game_Status == "Pre-Game") check_mark else x_mark
            
        # For Left Column Home Team
        
        home_team_logo = tags$img(src = team_logos[[game$Home_Team]], width = "100px")
        home_team <- game$Home_Team
        home_team_total_score <- game$Home_Total_Score
        home_team_pitcher <- game$Home_Pitcher
        home_team_pitcher_hand <- game$Home_Pitcher_Hand
        home_pitcher_WHIP <- home_pitcher_stats$WHIP
        home_pitcher_K_perc <- home_pitcher_stats$`K%`
        home_pitcher_BB_perc <- home_pitcher_stats$`BB%`
        home_pitcher_score <- game$Home_Pitcher_Score

        home_team_batting_avg <- home_team_batting_stats$AVG
        home_team_batting_slg <- home_team_batting_stats$SLG
        home_team_batting_babip <- home_team_batting_stats$BABIP
        home_team_batting_obp <- home_team_batting_stats$OBP
        home_team_batting_iso <- home_team_batting_stats$ISO
        home_team_batting_score <- game$Home_Batting_Score


        home_team_pitching_score <- game$Home_Pitching_Score

        # For Middle Column Ball Park / Weather / Umpire Etc
        ball_park_name <- game$Game_Venue
        game_day_or_night <- game$Day_Night
        stadium_image <- if (game_day_or_night == "day") {
            tags$img(src = team_stadiums[[game$Home_Team]]$day, width = "600px")
        } else {
            tags$img(src = team_stadiums[[game$Home_Team]]$night, width = "600px")
        }
        game_date <- game$Game_Date
        game_time <- game$Game_Time
        game_ID <- game$Game_ID
        ball_park_factor <- ballpark$Park.Factor
        ball_park_wOBACon <- ballpark$wOBACon
        ball_park_BACON <- ballpark$BACON
        ball_park_hard_hit <- ballpark$HardHit
        
        
        # For Right Column Away Team
        away_team_logo = tags$img(src = team_logos[[game$Away_Team]], width = "100px")
        away_team <- game$Away_Team
        away_team_total_score <- game$Away_Total_Score
        away_team_pitcher <- game$Away_Pitcher
        away_team_pitcher_hand <- game$Away_Pitcher_Hand
        away_pitcher_WHIP <- away_pitcher_stats$WHIP
        away_pitcher_K_perc <- away_pitcher_stats$`K%`
        away_pitcher_BB_perc <- away_pitcher_stats$`BB%`
        away_pitcher_score <- game$Away_Pitcher_Score
        
        away_team_batting_avg <- away_team_batting_stats$AVG
        away_team_batting_slg <- away_team_batting_stats$SLG
        away_team_batting_babip <- away_team_batting_stats$BABIP
        away_team_batting_obp <- away_team_batting_stats$OBP
        away_team_batting_iso <- away_team_batting_stats$ISO
        away_team_batting_score <- game$Away_Batting_Score
        

        away_team_pitching_score <- game$Away_Pitching_Score
        


        tagList(
            fluidRow(
                      column(
                          width = 3,
                          div(
                            style = "
                              display: flex;
                              flex-direction: column;
                              align-items: center;
                              justify-content: center;
                            ",
                            
                            img(home_team_logo),
                        
                            # Team name
                            div(style = 'font-weight:600; margin-top:4px;', home_team),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                            div(
                              span(style = "font-weight:600; color:black;", "Total Home Team Score: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_team_total_score, away_team_total_score), ";"),
                                home_team_total_score
                              )
                            ),
                            
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # Total Home Pitcher Score
                            div(
                              span(style = "font-weight:600; color:black;", "Total Home Pitcher Score: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_pitcher_score, away_pitcher_score), ";"),
                                home_pitcher_score
                              )
                            ),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # Starting Pitcher title
                            div(span(style = "font-weight:600; color:black;", "Starting Pitcher")),
                        
                            # Pitcher name + hand
                            div(
                              span(style = "font-weight:600; color:black;", "Pitcher: "),
                              span(style = "font-weight:600;", paste0(home_team_pitcher, " (", home_team_pitcher_hand, ")"))
                            ),
                        
                            # WHIP
                            div(
                              span(style = "font-weight:600; color:black;", "WHIP: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_low(home_pitcher_WHIP, away_pitcher_WHIP), ";"),
                                home_pitcher_WHIP
                              )
                            ),
                        
                            # K%
                            div(
                              span(style = "font-weight:600; color:black;", "K%: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_pitcher_K_perc, away_pitcher_K_perc), ";"),
                                home_pitcher_K_perc
                              )
                            ),
                        
                            # BB%
                            div(
                              span(style = "font-weight:600; color:black;", "BB%: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_low(home_pitcher_BB_perc, away_pitcher_BB_perc), ";"),
                                home_pitcher_BB_perc
                              )
                            ),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # Total Home Batting Score
                            div(
                              span(style = "font-weight:600; color:black;", "Total Home Batting Score: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_team_batting_score, away_team_batting_score), ";"),
                                home_team_batting_score
                              )
                            ),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # AVG
                            div(
                              span(style = "font-weight:600; color:black;", "AVG: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_team_batting_avg, away_team_batting_avg), ";"),
                                home_team_batting_avg
                              )
                            ),
                        
                            # SLG
                            div(
                              span(style = "font-weight:600; color:black;", "SLG: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_team_batting_slg, away_team_batting_slg), ";"),
                                home_team_batting_slg
                              )
                            ),
                        
                            # BABIP
                            div(
                              span(style = "font-weight:600; color:black;", "BABIP: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_team_batting_babip, away_team_batting_babip), ";"),
                                home_team_batting_babip
                              )
                            ),
                        
                            # OBP
                            div(
                              span(style = "font-weight:600; color:black;", "OBP: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_team_batting_obp, away_team_batting_obp), ";"),
                                home_team_batting_obp
                              )
                            ),
                        
                            # ISO
                            div(
                              span(style = "font-weight:600; color:black;", "ISO: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_team_batting_iso, away_team_batting_iso), ";"),
                                home_team_batting_iso
                              )
                            ),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # Total Home Pitching Score
                            div(
                              span(style = "font-weight:600; color:black;", "Total Home Pitching Score: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(home_team_pitching_score, away_team_pitching_score), ";"),
                                home_team_pitching_score
                              )
                            )
                          )
                        ),
                      column(
                          width = 6,
                          div(
                            style = "text-align: center;",
                            stadium_image,
                            div(style = 'font-weight: 600; margin-top: 4px;', ball_park_name),
                            div(paste0("Game ID: ", game_ID)),
                            div(paste0("Game Date: ", game_date)),
                            div(paste0("Game Time: ", game_time)),
                            div(paste0("Day / Night: ", game_day_or_night_logo, " ", game$Day_Night)),
                            div(paste0("Ball Park Factor: ", ball_park_factor)),
                            div(paste0("wOBACon: ", ball_park_wOBACon)),
                            div(paste0("BACON: ", ball_park_BACON)),
                            div(paste0("Hard Hit: ", ball_park_hard_hit))
                          )
                        ),
                # AWAY TEAM SIDE
                      column(
                          width = 3,
                          div(
                            style = "
                              display: flex;
                              flex-direction: column;
                              align-items: center;
                              justify-content: center;
                            ",
                            
                            img(away_team_logo),
                        
                            # Team name
                            div(style = 'font-weight:600; margin-top:4px;', away_team),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                            div(
                              span(style = "font-weight:600; color:black;", "Total Away Team Score: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_team_total_score, home_team_total_score), ";"),
                                away_team_total_score
                              )
                            ),
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # Total Away Pitcher Score
                            div(
                              span(style = "font-weight:600; color:black;", "Total Away Pitcher Score: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_pitcher_score, home_pitcher_score), ";"),
                                away_pitcher_score
                              )
                            ),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # Starting Pitcher title
                            div(span(style = "font-weight:600; color:black;", "Starting Pitcher")),
                        
                            # Pitcher name + hand
                            div(
                              span(style = "font-weight:600; color:black;", "Pitcher: "),
                              span(style = "font-weight:600;", paste0(away_team_pitcher, " (", away_team_pitcher_hand, ")"))
                            ),
                        
                            # WHIP
                            div(
                              span(style = "font-weight:600; color:black;", "WHIP: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_low(away_pitcher_WHIP, home_pitcher_WHIP), ";"),
                                away_pitcher_WHIP
                              )
                            ),
                        
                            # K%
                            div(
                              span(style = "font-weight:600; color:black;", "K%: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_pitcher_K_perc, home_pitcher_K_perc), ";"),
                                away_pitcher_K_perc
                              )
                            ),
                        
                            # BB%
                            div(
                              span(style = "font-weight:600; color:black;", "BB%: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_low(away_pitcher_BB_perc, home_pitcher_BB_perc), ";"),
                                away_pitcher_BB_perc
                              )
                            ),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # Total Away Batting Score
                            div(
                              span(style = "font-weight:600; color:black;", "Total Away Batting Score: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_team_batting_score, home_team_batting_score), ";"),
                                away_team_batting_score
                              )
                            ),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # AVG
                            div(
                              span(style = "font-weight:600; color:black;", "AVG: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_team_batting_avg, home_team_batting_avg), ";"),
                                away_team_batting_avg
                              )
                            ),
                        
                            # SLG
                            div(
                              span(style = "font-weight:600; color:black;", "SLG: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_team_batting_slg, home_team_batting_slg), ";"),
                                away_team_batting_slg
                              )
                            ),
                        
                            # BABIP
                            div(
                              span(style = "font-weight:600; color:black;", "BABIP: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_team_batting_babip, home_team_batting_babip), ";"),
                                away_team_batting_babip
                              )
                            ),
                        
                            # OBP
                            div(
                              span(style = "font-weight:600; color:black;", "OBP: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_team_batting_obp, home_team_batting_obp), ";"),
                                away_team_batting_obp
                              )
                            ),
                        
                            # ISO
                            div(
                              span(style = "font-weight:600; color:black;", "ISO: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_team_batting_iso, home_team_batting_iso), ";"),
                                away_team_batting_iso
                              )
                            ),
                        
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                            div(style = "height:12px;"),
                        
                            # Total Away Pitching Score
                            div(
                              span(style = "font-weight:600; color:black;", "Total Away Pitching Score: "),
                              span(
                                style = paste0("font-weight:600; color:", check_color_for_stat_high(away_team_pitching_score, home_team_pitching_score), ";"),
                                away_team_pitching_score
                              )
                            )
                          )
                        )
                    )
            )
    })
}


