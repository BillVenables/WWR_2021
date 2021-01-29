## ----"prelim",child="00-Prelim.Rnw"----------------------------------------------------------------------------------

## ----prelim,include=FALSE,tidy=FALSE---------------------------------------------------------------------------------

# library("knitr")

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
               out.width = '1.0\\linewidth',
               out.height = '1.0\\textheight',
               ##               out.height='0.43\\textheight',
               ##               cache = TRUE,
               comment = "# ",
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
set.seed(20190205)  ## for reproducibility
###

suppressPackageStartupMessages({
  library(SOAR)
  library(english)
  library(ggthemes)
  library(scales)
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
        function() par(las = 1, pch = 16),
        "append")

###################
### additional software
###################






## ----setFigPath,include=FALSE----------------------------------------------------------------------------------------
.infile <- sub("\\.Rnw$", "", knitr::current_input())
knitr::opts_chunk$set(fig.path = paste0('Fig/', .infile, "_")) #$
session <- sub("^0+", "", sub("[^[:digit:][:alpha:]].*$", "", .infile))


## #include <Rcpp.h>

## using namespace Rcpp;

## 
## // [[Rcpp::export]]

## NumericVector convolve(NumericVector a, NumericVector b) {

##   int na = a.size(), nb = b.size(), nab = na + nb -1;

##   NumericVector ab(nab);

## 
##   for(int i = 0; i < na; i++) {

##     for(int j = 0; j < nb; j++) {

##       ab[i+j] += a[i]*b[j];

##     }

##   }

##   return ab;

## }


## ----line_070_-------------------------------------------------------------------------------------------------------
#' Title
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
convolveR <- function(a, b) {
  na <- length(a)
  nb <- length(b)
  ab <- numeric(na + nb - 1)
  for(i in 1:na) {
    for(j in 1:nb) {
      ab[i + j -1] <- ab[i + j -1] + a[i]*b[j]
    }
  }
  ab
}


## ----line_093_-------------------------------------------------------------------------------------------------------
library(rbenchmark)
a <- rnorm(5000)
b <- c(1,4,6,4,1)/16
identical(convolve(a, b), convolveR(a, b))
benchmark(convolve(a, b), convolveR(a, b))[,c(1,3:4)]


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-------------------------------------------------------------
cat("{\\bf\nDate: ", format(Sys.Date()), "\n}")
toLatex(sessionInfo())

