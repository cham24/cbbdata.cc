#' This function calculates the statistics for a specific team based on the games played in 2023.
#' It calculates the total number of games played, total wins, total losses, and winning percentage.
#'
#' @param data A tibble containing game data, including columns for 'Home_Team', 'Away_Team', 'Home_Score', and 'Away_Score'.
#' @param teamName The name of the team for which statistics are to be calculated.
#' @return A tibble summarizing the team statistics for 2023, including total games played, total wins, total losses, and winning percentage.
#' @examples
#' calculateTeamStats2023(games_2023, "SUU")
#'
#' @import
#'   ggplot2
#'   magrittr
#'
#' @export
calculateTeamStats2023 <- function(data, teamName) {
  team_games <- data %>%
    filter(Home_Team == teamName | Away_Team == teamName)

  team_games <- team_games %>%
    mutate(outcome = case_when(
      (Home_Team == teamName & Home_Score > Away_Score) | (Away_Team == teamName & Home_Score < Away_Score) ~ "Win",
      (Home_Team == teamName & Home_Score < Away_Score) | (Away_Team == teamName & Home_Score > Away_Score) ~ "Loss",
      TRUE ~ "Tie"
    ))

  team_summary <- team_games %>%
    summarize(
      totalGames = n(),
      totalWins = sum(outcome == "Win"),
      totalLosses = sum(outcome == "Loss"),
      winningPercentage = totalWins / totalGames
    )

  return(team_summary)
}
