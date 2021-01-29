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


## ----line_059_,include=FALSE---------------------------------------------------------------------------------------------
oldOpt <- options(digits = 4)
theme_set(theme_solarized())


## ----hmat,echo=FALSE-----------------------------------------------------------------------------------------------------
Hmat <- function(k) {
  M <- matrix(0, k, k)
  1/(2 * row(M) + col(M) - 1)
}

## ----hmat2---------------------------------------------------------------------------------------------------------------
Hmat(7)


## ----hmat3,message=FALSE-------------------------------------------------------------------------------------------------
library(fractional)
Hmat(7) %>% fractional
detach("package:fractional", unload = TRUE)


## ----rat1----------------------------------------------------------------------------------------------------------------
#' @describeIn ratAppr Workhorse function for a single value
#' @export
.ratAppr <- function(x, eps = 1.0e-6, maxConv = 20) {
  PQ1 <- c(1, 0)
  PQ2 <- c(floor(x), 1)
  r <- x - PQ2[1]
  i <- 0
  while((i <- i+1) < maxConv && abs(x - PQ2[1]/PQ2[2]) > eps) {
    b <- floor(1/r)
    r <- 1/r - b
    PQ0 <- PQ1
    PQ1 <- PQ2
    PQ2 <- b*PQ1 + PQ0
  }
  return(c(PQ2, i-1))
}


## ----rat2----------------------------------------------------------------------------------------------------------------
ratAppr <- function(x, eps = 1.0e-6, maxConv = 20) {
  vapply(x, FUN = .ratAppr,
         FUN.VALUE = c(Pn = 0, Qn = 0, n = 0),
         eps = eps, maxConv = maxConv)
}


## ----rat3----------------------------------------------------------------------------------------------------------------
.ratAppr(base::pi)
ratAppr(c(1:10/7, pi, sqrt(2)))


## ----rat4----------------------------------------------------------------------------------------------------------------
vulgar <- function(x, eps = 1.0e-6, maxConv = 20) {
  structure(x, eps = eps, maxConv = maxConv,
            class = c("vulgar", class(x)))
}


## ----rat5----------------------------------------------------------------------------------------------------------------
unvulgar <- function(x) {
  x <- unclass(x)
  attr(x, "eps") <- attr(x, "maxConv") <- NULL
  x
}


## ----rat6----------------------------------------------------------------------------------------------------------------
getAttr <- function(x)
  UseMethod("getAttr")
getAttr.default <- function(x)      ## not "vulgar" make it up
  list(eps = 1.0e-6, maxConv = 20)
getAttr.vulgar <- function(x)       ## is "vulgar", get it
  attributes(x)[c("eps", "maxConv")]


## ----rat7----------------------------------------------------------------------------------------------------------------
Ops.vulgar <- function (e1, e2) {
  ax <- getAttr(e1)
  e1 <- unclass(e1)
  if (!missing(e2)) {  ## not unary minus or plus
    ax2 <- getAttr(e2)
    ax <- list(eps = min(ax$eps, ax2$eps),
               maxConv = max(ax$maxConv, ax2$maxConv))
    e2 <- unclass(e2)
  }
  res <- NextMethod(.Generic)
  if(typeof(res) == "logical") { ## it was a logical operator
    res
  } else {
    with(ax, vulgar(res, eps = eps, maxConv = maxConv))
  }
}


## ----rat8----------------------------------------------------------------------------------------------------------------
Hmat <- function(k) {
  M <- matrix(0, k, k)
  1/(2 * row(M) + col(M) - 1)
}
vulgar(Hmat(3)) + 1


## ----aschar--------------------------------------------------------------------------------------------------------------
as.character.vulgar <- function (x, eps = attr(x, "eps"),
                                 maxConv = attr(x, "maxConv"), ...) {
  x <- unclass(x)
  ax <- attributes(x)
  rx <- ratAppr(as.vector(x), eps = eps, maxConv = maxConv)
  fractions <- sub("/1$", "", paste(rx["Pn", ], rx["Qn", ], sep = "/"))
  ax$maxConv <- ax$eps <- NULL
  attributes(fractions) <- ax
  class(fractions) <- "vulgarCharacter"
  fractions
}


## ----print1--------------------------------------------------------------------------------------------------------------
print.vulgarCharacter <- function(x, ...) {
  y <- x
  x <- gsub("^0$", ".", unclass(x))
  NextMethod("print", quote = FALSE, ...)
  invisible(y)
}
print.vulgar <- function (x, ...) {
  x0 <- x
  y <- gsub("^0$", ".", as.character.vulgar(x))
  y <- format(y, justify = "right")
  ax <- attributes(x)
  ax$class <- ax$eps <- ax$maxConv <- NULL
  x <- do.call("structure", c(list(y), ax))
  NextMethod("print", quote = FALSE, ...)
  invisible(x0)
}


## ----check---------------------------------------------------------------------------------------------------------------
(vulgar(Hmat(3)) + 1) %>% as.character
cbind(diag(1/1:3), 0) - vulgar(cbind(0, diag(1/3:1)))
contr.helmert(4) %>% cbind(Ave = 1, .) %>% solve %>% vulgar


## #include <Rcpp.h>

## using namespace Rcpp;

## 
## IntegerVector ratApp_one(double x, double eps, int maxConv) {

##   int p0, p1 = 1, p2 = (int) floor(x),

##       q0, q1 = 0, q2 = 1, b, i = 0;

##   double z = x - (double) p2;

##   while(++i < maxConv) {

##     if(fabs(x - (double) p2 / (double) q2) < eps) break;

##     z = 1/z; b = (int) floor(z); z = z - b;

##     p0 = p1; p1 = p2; p2 = b*p1 + p0;

##     q0 = q1; q1 = q2; q2 = b*q1 + q0;

##   }

##   return IntegerVector::create(p2, q2, i-1);

## }

## 
## //' @describeIn ratAppr C++ version of the same function

## //' @export

## //' @import Rcpp

## //' @useDynLib vulgar

## // [[Rcpp::export]]

## IntegerMatrix ratApp(NumericVector x, double eps = 1.0e-6, int maxConv = 20) {

##   int nx = x.length();

##   IntegerMatrix PQC(3, nx);

##   PQC.attr("dimnames") =

##     List::create(CharacterVector::create("Pn", "Qn", "n"),

##                  R_NilValue);

##   for(int i = 0; i < nx; i++) {

##     PQC(_, i) = ratApp_one(x[i], eps, maxConv);

##   }

##   return PQC;

## }


## ----check_ratApp--------------------------------------------------------------------------------------------------------
ratApp


## ----chech_2-------------------------------------------------------------------------------------------------------------
u <- runif(11); rbind(R = ratAppr(u), Cpp = ratApp(u))
all.equal(ratAppr(u), ratApp(u))


## ----chech_3,out.height="2in",fig.height=4,fig.width=6,cache=TRUE--------------------------------------------------------
die <- Sys.time() %>% as.POSIXlt %>% unclass %>%
  (function(x) x$min*x$sec) %>% round
set.seed(die)  ## The die is cast!

library(microbenchmark)
u <- runif(10000)
(bm <- microbenchmark(ratAppr(u), ratApp(u)))
suppressMessages(autoplot(bm)) + ggthemes::theme_economist()


## ----aschar1-------------------------------------------------------------------------------------------------------------
as.character.vulgar <- function (x, eps = attr(x, "eps"),
                                 maxConv = attr(x, "maxConv"), ...) {
  x <- unclass(x)
  ax <- attributes(x)
  ## the only chenged line is the one below
  rx <- ratApp(as.vector(x), eps = eps, maxConv = maxConv)
  fractions <- sub("/1$", "", paste(rx["Pn", ], rx["Qn", ], sep = "/"))
  ax$maxConv <- ax$eps <- NULL
  attributes(fractions) <- ax
  class(fractions) <- "vulgarCharacter"
  fractions
}
cbind(Ave = 1, contr.helmert(4)) %>% solve %>% vulgar


## ----line_504_,eval=FALSE------------------------------------------------------------------------------------------------
## devtools::build_vignettes()


## ----line_516_,eval=FALSE------------------------------------------------------------------------------------------------
## devtools::document()


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo())

