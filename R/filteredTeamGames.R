#' This function filters the input tibble to only include games played by the
#' specified team, whether as a home team or away team.
#'
#' @param data The input tibble containing game data.
#' @param team The name of the team to filter for.
#' @return A tibble containing only the games played by the specified team.
#' @examples
#' filtered_games <- filteredTeamGames(games_2023, "SUU")
#'
#' @import
#'   ggplot2
#'   magrittr
#'
#' @export
filteredTeamGames <- function(data, team) {
  filteredTeamData <- data %>%
    filter(Home_Team == team | Away_Team == team)
  return(filteredTeamData)
}

