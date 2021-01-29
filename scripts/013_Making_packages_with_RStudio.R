## ----"prelim",child="00-Prelim.Rnw"--------------------------------------------------------------------------------------

## ----prelim,include=FALSE,tidy=FALSE-------------------------------------------------------------------------------------



################
### knitr settings
################
.out0 <- knit_hooks$get("output") 
knit_hooks$set(output = function(x, options) { #$
  if(!is.null(n <- options$out.lines) && 
     is.numeric(n) && 
     length(n) == 1 && 
     is.finite(n) && 
     n > 0) {
    x <- unlist(strsplit(x, "\n"))
    if(length(x) > n) {
      x <- c(head(x, ceiling(n/2)), "....", tail(x, floor(n/2)), "")
    }
    x <- paste(x, collapse = "\n")
  }
  .out0(x, options)
})

knit_hooks$set(lattice = function(before, options, envir) { #$
  if(before) {
    suppressPackageStartupMessages(require(lattice))
    lattice.options(default.args = list(as.table = TRUE))
  } else {
    NULL
  }
})

opts_chunk$set(size = 'footnotesize',  #$
               ##               out.lines = 6,
               background = "#F4FCFE",
               fig.width = 9, 
               fig.height = 7,
               # out.width = '1.0\\linewidth',
               # out.height = '1.0\\textheight',
               out.width = '100%',
               out.height = '100%',
               ## out.height='0.43\\textheight',
               ## cache = TRUE,
               # comment = "# ",
               comment = "",
               warning = FALSE,
               fig.align="center",
               tidy.opts = list(keep.comment = TRUE,  
                                keep.blank.line = FALSE,
                                keep.space = TRUE,
                                left.brace.newline = FALSE,
                                replace.assign = TRUE,
                                reindent.spaces = 2),
               tidy = FALSE)

inline_hook <- function(x) {
    if(is.numeric(x)) x <- round(x, 2)
    paste(as.character(x), collapse = ", ")
}
knit_hooks$set(inline = inline_hook)

library(xtable)
options(width = 75, xtable.booktabs = TRUE)

###############################################################################
### 8< cut here 8< ### 8< cut here 8< ### 8< cut here 8< ### 8< cut here 8< ###
###               REMOVE or COMMENT OUT everything above here               ###
###############################################################################

################
### R settings
################

###
set.seed(20210202)  ## for reproducibility
###

suppressPackageStartupMessages({
  # library(SOAR)
  library(english)
  library(ggthemes)
  library(scales)
  library(patchwork)
  library(gridExtra)
  library(lattice)
  library(WWRCourse)
})


newTheme <- theme_minimal(base_family = "sans")  + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(strip.background = element_rect(fill = "#FFE5CC"))
oldTheme <- theme_set(newTheme)


Attach(lib = .Robjects)
._objects <- intersect(ls(".Robjects"), ls("package:WWRCourse"))
if(length(._objects) > 0) 
  Remove(list = ._objects, lib = .Robjects)  ## remove duplicates
rm(._objects)
Attach()
Remove(list = Ls())  ## clean up working .R_Cache
rm(list = ls())      ## clean up workspace ... for now.

options(show.signif.stars = FALSE)   ## signif.stars are for wimps...

setHook("plot.new", 
        function() par(las = 1, pch = 20),
        "append")

###################
### additional software
###################






## ----setFigPath,include=FALSE--------------------------------------------------------------------------------------------
.infile <- sub("\\.Rnw$", "", knitr::current_input())
knitr::opts_chunk$set(fig.path = paste0('Fig/', .infile, "_")) #$
session <- sub("^0+", "", sub("[^[:digit:][:alpha:]].*$", "", .infile))


## ----line_096_-----------------------------------------------------------------------------------------------------------
.libPaths()


## ----line_109_,eval=FALSE------------------------------------------------------------------------------------------------
## if(!dir.exists("~/R/win-library/4.0")) {
##   if(dir.create("~/R/win-library/4.0", recursive = TRUE)) {
##     cat("The folder '"~/R/win-library/4.0"' successfully created\n")
##   } else {
##     cat("The folder '"~/R/win-library/4.0"' cannot be created!\n")
##   }
## } else {
##   cat("The folder '"~/R/win-library/4.0"' already exists\n")
## }


## ----gcd,eval=FALSE------------------------------------------------------------------------------------------------------
## gcd <- function(a, b) {
##   if(b == 0) a else Recall(b, a %% b)
## }


## ----gcd2,eval=FALSE-----------------------------------------------------------------------------------------------------
## #' Title
## #'
## #' @param a
## #' @param b
## #'
## #' @return
## #' @export
## #'
## #' @examples
## gcd <- function(a, b) {
##   if(b == 0) a else Recall(b, a %% b)
## }


## ----gcd3,eval=FALSE-----------------------------------------------------------------------------------------------------
## #' Euclidean algorithm
## #'
## #' Find the greatest common divisor of two integers
## #'
## #' @param a A single integer value
## #' @param b A single ingeter value
## #'
## #' @return An integer value the GCD of \code{a} and \code{b}
## #' @export
## #'
## #' @examples
## #' gcd(24, 36)  ## answer: 12
## gcd <- function(a, b) {
##   if(b == 0) a else Recall(b, a %% b)
## }


## ----echo=FALSE, out.width="100%", fig.width=10,fig.height=4.5-----------------------------------------------------------
searchpath::use_packages(WWRGraphics)

pts <- expand.grid(x = 1:2, y = 1:5) %>% within(y <- 6-y)

z <- with(pts, complex(nrow(pts),x,y))
rotate <- c(2,3,5,7,9, 10, 8, 6, 4)
z1 <- z[rotate]

layout(rbind(1:4))
plot.new()
par(mar = rep(0, 4), usr = c(0, 3, 0, 6))
# points(z, pch=20, cex = 0.7, col = "firebrick4")
text(z, labels = paste0("T", 0:9), cex = 2.5, col = "firebrick4", family = "sans")
segments(0.5, 0.5 + 0:5, 2.5, 0.5 + 0:5)
segments(0.5 + c(0, 2), 0.5, 0.5 + c(0, 2), 5.5)
text(1.5, 5.75, "Round 1", cex = 2.5, family = "sans")

plot.new()
par(mar = rep(0, 4), usr = c(0, 3, 0, 6))
points(z, pch=20, cex = 1, col = "firebrick4")
# text(z, labels = paste0("T", 0:9), pos = rep(c(2, 4), 5), cex=1.5)
segments(0.5, 0.5 + 0:5, 2.5, 0.5 + 0:5)
segments(0.5 + c(0, 2), 0.5, 0.5 + c(0, 2), 5.5)
text(1.5, 5.75, "(Circulate)", cex = 2.5, family = "sans")

arrows(z1, circular = TRUE, gap = 2, angle = 15, length = 2, col=4)

plot.new()
par(mar = rep(0, 4), usr = c(0, 3, 0, 6))
# points(z, pch=20, cex = 0.7, col = "firebrick4")
text(z, labels = paste0("T", c(0, 3, 1, 5, 2, 7, 4, 9, 6, 8)), cex = 2.5, col = "firebrick4", family = "sans")
text(1.5, 5.75, "Round 2", cex = 2.5, family = "sans")
segments(0.5, 0.5 + 0:5, 2.5, 0.5 + 0:5)
segments(0.5 + c(0, 2), 0.5, 0.5 + c(0, 2), 5.5)

plot.new()
par(mar = rep(0, 4), usr = c(0, 3, 0, 6))
points(z, pch=20, cex = 1, col = "firebrick4")
# text(z, labels = paste0("T", 0:9), pos = rep(c(2, 4), 5), cex=1.5)
segments(0.5, 0.5 + 0:5, 2.5, 0.5 + 0:5)
segments(0.5 + c(0, 2), 0.5, 0.5 + c(0, 2), 5.5)
text(1.5, 5.75, "(Circulate)", cex = 2.5, family = "sans")

arrows(z1, circular = TRUE, gap = 2, angle = 15, length = 2, col=4)



## ------------------------------------------------------------------------------------------------------------------------
tournament <- function(teams) {
  if(length(teams) %% 2 == 1) { ## odd number of teams -> byes needed
    teams <- c("<Bye>", teams)
  }
  n <- length(teams)
  tournament <- vector(length = n-1, mode = "list")
  for(j in 1:(n-1)) {
    tournament[[j]] <- matrix(teams, ncol = 2)
    teams <- teams[c(1, n/2+1, if(n/2 > 2) 2:(n/2-1), ## else NULL
                     (n/2+2):n, n/2)]
  }
  return(tournament)
}


## ----out.lines=20--------------------------------------------------------------------------------------------------------
tournament(LETTERS[1:5]) ## Five (anonymous) teams!


## ----eval=FALSE----------------------------------------------------------------------------------------------------------
## library(roundRob)


## ----eval=FALSE----------------------------------------------------------------------------------------------------------
## tournament(NRL)


## ----eval=FALSE----------------------------------------------------------------------------------------------------------
## print.tournament(tournament(AFLW))


## ----eval=FALSE----------------------------------------------------------------------------------------------------------
## dir("./Temp")  ## check that it's where you think it is
## unlink("./Temp/roundRob", recursive = TRUE)  # To delete it


## ----eval=FALSE----------------------------------------------------------------------------------------------------------
## (tourn <- tornament(AFLW))  ## gives a full torunament
## team_schedule(tourn)        ## unpicks the tournament for each team


## ----line_420_,eval=FALSE------------------------------------------------------------------------------------------------
## library(devtools)
## use_vignette("planning_a_tournament")


## ----line_428_,eval=FALSE------------------------------------------------------------------------------------------------
## build_vignettes()


## ----line_448_-----------------------------------------------------------------------------------------------------------
dir("./PackageMaterials/example_cpp/")


## ----eval=FALSE----------------------------------------------------------------------------------------------------------
## #' @useDynLib miscSmooth
## #' @import Rcpp
## #' @import stats
## NULL


## ----pkg1,eval=FALSE-----------------------------------------------------------------------------------------------------
## devtools::document()


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo(), locale = FALSE)

