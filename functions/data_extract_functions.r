################ PITCHER DATA FOR PRACTICE ####################################
pitcher_data_practice <- function() {

  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
    *
  from ind_pitcher_statcast')
  
  pitcher_df <- dbGetQuery(con, query)                         # MAIN PITCHER DF
  
  dbDisconnect(con)

  return(pitcher_df)
}

#######################PITCHER DF FOR PITCHER BENCHMARK! ############################################################

pitcher_data_for_benchmark <- function() {

  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
    *
  from "pitcher_benchmark_table_2025"')
  
  pitcher_df <- dbGetQuery(con, query)                         # MAIN PITCHER DF
  
  dbDisconnect(con)
  
  return(pitcher_df)
}

#############################TEAM BATTING DF FOR BENCHMARK###################################################################
team_batting_data_for_benchmark <- function() {
    
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('SELECT
      *
  FROM "batting_benchmark_table_2025";
  ')
  
  
  team_batting_df <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  
  return(team_batting_df)
}

#######################TEAM PITCHING DF FOR BENCHMARK#########################################################################
team_pitching_data_for_benchmark <- function() {

  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )
  
  query <- ('SELECT
      *
  FROM "pitching_benchmark_table_2025";
  ')
  
  team_pitching_df <- dbGetQuery(con, query)                         # MAIN TEAM PITCHING DF
  
  dbDisconnect(con)
  
  return(team_pitching_df)
}

######################################PITCHER DF ############################################################

pitcher_data <- function() {

  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select *
  from active_pitcher_stats
            ')
  
  pitcher_df <- dbGetQuery(con, query)                         # MAIN PITCHER DF
  
  dbDisconnect(con)
  
  return(pitcher_df)
}

########################## PITCHER RECENT FORM #########################################################################
starting_pitcher_recent_form_data <- function() {

  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
    *
  from starting_pitchers_recent_form')
  
  starting_pitcher_recent_form_df <- dbGetQuery(con, query)                         # MAIN PITCHER DF
  
  dbDisconnect(con)
  
  return(starting_pitcher_recent_form_df)
}

###########################################TEAM BATTING DF###################################################################
team_batting_data <- function() {
    
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
      *
      FROM active_team_batting_stats;')
  
  
  team_batting_df <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  
  return(team_batting_df)
}

###########################################TEAM PITCHING DF###################################################################
team_pitching_data <- function() {
    
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
      *
      FROM active_team_pitching_stats;')
  
  
  team_pitching_df <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  
  return(team_pitching_df)
}

###################################### BATTER STATS DF ####################################
batter_data <- function() {
    
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
      *
      FROM batter_seasonal_data;')
  
  
  batter_stats_data <- dbGetQuery(con, query)
  
  dbDisconnect(con)

  return(batter_stats_data)
}


####################### BATTING SPLITS DATA #######################
batting_splits_data <- function() {
    
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
      "playerName",
      "playerId",
      "wOBA_splits",
      "ISO_splits",
      "BB%_splits",
      "K%_splits"
      FROM active_batter_splits;')
  
  
  batting_splits_data <- dbGetQuery(con, query)
  
  dbDisconnect(con)

  return(batting_splits_data)
}


######################################### TEAM TRAVEL DF ################################
team_travel_data <- function() {

  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
    *
  from team_travel_data')
  
  team_travel_df <- dbGetQuery(con, query)                         # MAIN PITCHER DF
  
  dbDisconnect(con)
  
  return(team_travel_df)
}

###########################################TEAM BATTING HISTORICAL DF###################################################################
historical_team_batting_data <- function() {
    
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
      *
      FROM historical_team_batting_stats;')
  
  
  team_batting_df <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  
  return(team_batting_df)
}

###########################################TEAM PITCHING  HISTORICAL DF###################################################################
historical_team_pitching_data <- function() {
    
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )

  query <- ('select
      *
      FROM historical_team_pitching_stats;')
  
  
  team_pitching_df <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  
  return(team_pitching_df)
}