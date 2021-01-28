tournament <- function(teams) {
  if(is.numeric(teams) && length(teams) == 1) {
    teams <- paste0("T", seq_len(teams))
  }
  teams <- as.character(teams)
  if(length(teams) %% 2 == 1) { ## odd number of teams -> byes needed
    teams <- c("<Bye>", teams)
  }
  n <- length(teams)
  tournament <- vector(length = n-1, mode = "list")
  dnam <- list(paste0("G", 1:(n/2)), paste0("Team_",1:2))
  for(j in 1:(n-1)) {
    tournament[[j]] <- array(teams, dim = c(n/2, 2), dimnames = dnam)
    ##
    ## initial order: first column: 1      :(n/2),
    ##               second column: (n/2+1):n
    ##
    teams <- teams[c(1, n/2+1, if(n/2 > 2) 2:(n/2-1), ## else NULL
                     (n/2+2):n, n/2)]
  }
  structure(tournament, teams = teams, names = paste0("Round_", 1:(n-1)),
            class = "tournament")
}

print.tournament <- function(x, ...) {
  print(structure(unclass(x), teams = NULL), quote = FALSE)
  invisible(x)
}

team_schedule <- function(tournament) {
  teams <- attr(tournament, "teams")
  schedule <- lapply(teams, function(team) {
    cbind(team = team,
          opponent = sapply(tournament,
                            function(round) {
                              p <- which(round == team, arr.ind = TRUE)
                              round[p[1, "row"], 3-p[1, "col"]]
                            }))
  })
  structure(schedule, names = teams, class = "team_schedule")
}

print.team_schedule <- function(x, ...) {
  print(unclass(x), quote = FALSE)
  invisible(x)
}
