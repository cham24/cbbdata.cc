#' This function creates a bar plot showing the winning percentage of each team.
#' It takes a tibble containing team statistics as input and plots the winning percentage for each team.
#'
#' @param data A tibble containing team statistics, including columns for 'teamName' and 'winningPercentage'.
#' @return A ggplot object representing a bar plot showing the winning percentage of each team.
#' @examples
#' plotGamesByTeam(team_data)
#'
#' @import
#'   ggplot2
#'   magrittr
#'
#' @export
plotGamesByTeam <- function(data) {
  ggplot(data, aes(x = teamName, y = winningPercentage)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Winning Percentage by Team",
         x = "Team",
         y = "Winning Percentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
