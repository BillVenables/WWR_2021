#' AFLW
#'
#' Australian Women's Football League teams for the national competition
#'
#' @format A character string vector with 14 team names
#'
"AFLW"

#' NRL
#'
#' Rugby League teams for the national ANZ competition
#'
#' @format A character string vector with 16 team names
#'
"NRL"

#### function to schedule a round robin tournament

#' Generate a round robin tournament schedule
#'
#' Given a list of n team names, generate a program of n-1 Rounds
#' where each team plays each other team precisely once.  If n is
#' odd the teams are augmented by a dummy team <Bye>.
#'
#' @param teams Either an interger specifing the number of teams,
#'        or a character string vector giving their names
#' @param alphabetical logical: should the team names be put in
#'        alphabetical order?
#' @param reorder logical: within each round should the games be
#'        listed in alphabetical order of the "Home" team?
#'
#' @return A 3-dimensional (n/2 x 2 x (n-1)) array giving the
#'        entire tournament
#' @export
#'
#' @examples
#' (Season21 <- round_robin(NRL))
#' summary(Season21)
#' summary(Season21, "travel")
round_robin <- function(teams, alphabetical = FALSE, reorder = FALSE) {
  if(is.numeric(teams) && length(teams) == 1 && teams > 0) {
    teams <- paste0("Team", format(1:ceiling(teams)))
  }
  stopifnot(is.character(teams) && length(teams) > 0)
  if(any(duplicated(teams)))
    stop("duplicated team names are not allowed")
  if(alphabetical) teams <- sort(teams)
  odd <- length(teams) %% 2 == 1
  if(odd)
    teams <- c("<Bye>", teams)
  teams <- format(teams)
  n <- length(teams)
  round <- cbind(teams[1:(n/2)], teams[n:(n/2+1)])
  colnames(round) <- c("Home", "Away")
  rownames(round) <- paste("Match", format(1:nrow(round)))
  Res <- structure(vector("list", n-1),
                   names = paste("Round", format(seq_len(n-1))))
  Res[[1]] <- round
  if(n > 2) {
    ij <- rbind(cbind(2:nrow(round), 1), cbind(nrow(round):1, 2))
    ji <- rbind(ij[-1,,drop = FALSE], ij[1,,drop = FALSE])
    for(i in 2:(n-1)) {
      round[ji] <- round[ij]
      r <- round
      if(!odd && (i %% 2 == 0))
        r[1, 1:2] <- r[1, 2:1]
      Res[[i]] <- r
    }
  }
  if(reorder) {
    for(j in seq_along(Res))
      Res[[j]][] <- as.vector(Res[[j]][order(Res[[j]][, 1]), ])
  }
  class(Res) <- "round_robin"
  Res
}

#' @rdname round_robin
#' @export
print.round_robin <- function(x, ...) {
  y <- lapply(x, data.frame, stringsAsFactors = FALSE)
  for(i in seq_along(x)) {
    cat(paste0("\n", names(y)[i], ":\n"))
    print(y[[i]])
  }
  invisible(x)
}

#' @rdname round_robin
#' @export
summary.round_robin <- function(object, type = c("venue", "travel"), ...) {
  type <- match.arg(type)
  if(type == "travel") {
    rounds <- format(seq_along(object))
    for(i in seq_along(object)) {
      object[[i]] <- cbind(Round = rounds[i], object[[i]])
    }
  }
  g <- do.call(rbind, object)
  if(any(byes <- grepl("^<Bye> *$", g))) {
    dim(byes) <- dim(g)
    i <- which(byes, arr.ind = TRUE)[, "row"]
    g <- g[-i,]
  }
  if(type == "venue") {
    f <- factor(col(g), levels = 1:2, labels = colnames(g))
    table(Team = g, Venue = f)
  } else {
    # g <- data.frame(g, stringsAsFactors = FALSE) %>%
    #   gather(key = Venue, value = Team, Home, Away) %>%
    #   within(Venue <- format(substring(Venue, 0, 1),
    #                          justify = "right",
    #                          width = nchar(rounds[1]))) %>%
    #   spread(key = Round, value = Venue)
    Home <- cbind(g[, c("Round", "Home")], Venue = "Home")
    Away <- cbind(g[, c("Round", "Away")], Venue = "Away")
    g <- rbind(Home, Away)
    colnames(g)[2] <- "Team"
    g <- within(data.frame(g, stringsAsFactors = FALSE), {
      Venue <- format(substring(Venue, 0, 1),
                      justify = "right",
                      width = nchar(rounds[1]))
    })
    teams <- with(g, sort(unique(Team)))
    rounds <- with(g, sort(unique(Round)))
    out <- matrix(NA_character_, length(teams), length(rounds))
    dimnames(out) <- list(Team = teams, Round = rounds)
    ij <- with(g, cbind(match(Team, teams), match(Round, rounds)))
    out[ij] <- g$Venue
    # out <- as.matrix(g[, -1])
    out[is.na(out)] <- format("*", width = nchar(rounds[1]),
                              justify = "right")
    # rownames(out) <- g[["Team"]]
    noquote(out)
  }
}
