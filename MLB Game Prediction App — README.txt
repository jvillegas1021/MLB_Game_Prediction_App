MLB Game Prediction App — README
Overview
This Shiny application generates daily MLB matchup predictions using a structured evaluation pipeline built in R. The app pulls the current day’s games, retrieves pitcher and team performance data from a cloud‑hosted PostgreSQL database, and produces a matchup‑style display with scores, win probabilities, and predicted winners.

Because the database requires private credentials, the app cannot be run without the appropriate environment variables. The code is provided for demonstration of architecture, modeling logic, and data engineering practices.

Link to the web app is here :  https://jvillegas1021.shinyapps.io/MLB_Games_Prediction/

How the App Works
1. Daily Game Retrieval
The app begins by pulling the full MLB schedule for the current date through an external API. Each game is displayed as a matchup card within the Shiny interface.

2. Starting Pitcher Evaluation
For each matchup, the app retrieves:

Career performance (last 3 seasons)

Recent form metrics

Pitch‑level Statcast summaries (pre‑processed in the database)

These metrics are combined into a pitcher score that reflects both long‑term skill and short‑term performance.

3. Team Batting & Bullpen Comparison
The app pulls team‑level batting and pitching data and compares each club to custom benchmark tables. These benchmarks represent league‑normalized performance for:

Offense

Starting pitching

Bullpen strength

Each component contributes to a total team score.

4. Scoring & Prediction Logic
For every game, the app:

Combines pitcher score + team batting score + bullpen score

Computes a score differential

Converts that differential into a win probability

Assigns a predicted winner

The result is a clean, interpretable matchup summary for each game on the schedule.

5. Shiny App Display
The final output is presented as a web‑based interface showing:

Team logos

Starting pitchers

Scores and probabilities

Predicted winner

Supporting metrics

This provides a quick, readable view of the day’s matchups.

Data Sources
All data is stored in a private PostgreSQL database and accessed through secure environment variables. Key tables include:

Active pitcher stats

Pitcher recent form

Team batting and pitching benchmarks

Historical team performance

Batter splits

Travel and fatigue data

These tables are updated by an external ETL pipeline (not included in this repo).

Running the App
To run the app locally, you must create a .Renviron file containing your database credentials

Link to the web app is here :  https://jvillegas1021.shinyapps.io/MLB_Games_Prediction/
