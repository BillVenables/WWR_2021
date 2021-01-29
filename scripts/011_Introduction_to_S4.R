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


## ----line_069_-----------------------------------------------------------------------------------------------------------
TTest <- function(x, ...) {
  UseMethod("TTest")
}
TTest.numeric <- function(x, y, ...) {  ## the work horse
  stopifnot(is.numeric(y))
  nx <- length(x)
  ny <- length(y)
  m <- mean(x) - mean(y)
  s2 <- (sum((x - mean(x))^2) + sum((y - mean(y))^2))/(nx+ny-2)
  sd <- sqrt(s2*(1/nx + 1/ny))
  res <- within(list(tstat = m/sd, df = nx+ny-2), {
    p.value <- 2*pt(-abs(tstat), df)
  })
  class(res) <- "TTest"
  res
}
TTest.integr <- TTest.numeric


## ----line_089_-----------------------------------------------------------------------------------------------------------
TTest.factor <- function(x, y, ...) {
  stopifnot(is.numeric(y), length(levels(x)) == 2)
  x1 <- y[x == levels(x)[1]]
  x2 <- y[x == levels(x)[2]]
  TTest.numeric(x1, x2, ...)
}

TTest.character <- function(x, y, ...) {
  TTest.factor(factor(x), y, ...)
}

TTest.formula <- function(x, data, ...) {
  lhs <- eval(x[[2]], envir = as.environment(data))
  rhs <- eval(x[[3]], envir = as.environment(data))
  TTest.factor(as.factor(rhs), lhs, ...)
}


## ----line_109_-----------------------------------------------------------------------------------------------------------
TTest(Days ~ Eth, quine)
t.test(Days ~ Eth, quine, var.equal = TRUE)[cs(statistic, parameter)]


## ----line_115_-----------------------------------------------------------------------------------------------------------
find("print")
print
print.TTest <- function(x, ...) {
  with(x, cat("t = ", tstat, 
              ", df = ", df,
              ", p = ", p.value, "\n", sep = ""))
  invisible(x)
}

TTest(Days ~ Eth, quine)


## ----line_133_-----------------------------------------------------------------------------------------------------------
setClass("t_test", representation(x = "numeric",
                                  y = "numeric",
                                  t = "numeric",
                                  df = "integer",
                                  p = "numeric"))


## ----line_141_-----------------------------------------------------------------------------------------------------------
setGeneric("t_test", function(x, y) {
  standardGeneric("t_test")
})


## ----line_149_-----------------------------------------------------------------------------------------------------------
t_test


## ----line_153_-----------------------------------------------------------------------------------------------------------
setMethod("t_test", signature(x = "numeric", y = "numeric"),
          function(x, y) {
            tt <- stats::t.test(x, y, var.equal = TRUE) ## cheating!
            with(tt, new("t_test", x = x, y = y, 
                         t = statistic, 
                         df = as.integer(parameter), 
                         p = p.value))
})


## ----line_164_-----------------------------------------------------------------------------------------------------------
setMethod("t_test", signature(x = "factor", y = "numeric"),
          function(x, y) {
            hold <- y
            lev <- levels(x)
            y <- hold[x == lev[2]]
            x <- hold[x == lev[1]]
            callNextMethod(x, y)
          })

## If you put them in the wrong order...
setMethod("t_test", signature(x = "numeric", y = "factor"),
          function(x, y)
            callGeneric(y, x))

setMethod("t_test", signature(x = "formula", y = "data.frame"),
          function(x, y) {
            v <- eval(x[[2]], envir = y) ## the values
            f <- eval(x[[3]], envir = y) ## the factor
            callGeneric(f, v)
          })


## ----line_188_-----------------------------------------------------------------------------------------------------------
showClass("t_test")
showMethods("t_test")


## ----line_194_,out.lines=16----------------------------------------------------------------------------------------------
t_test(Days ~ Eth, quine)


## ----line_201_-----------------------------------------------------------------------------------------------------------
show

setMethod("show", signature(object = "t_test"), 
          function(object) {
            cat("t = ", signif(object@t, digits = 3), 
                ", df = ", object@df, 
                ", p = ", signif(object@p, digits = 2), "\n", sep="")
            invisible(object)
})
t_test(Days ~ Eth, quine)  ## that's better!


## ----line_215_,out.height="0.5\\textheight",fig.show="hold",fig.height=4,fig.width=5.5-----------------------------------
setGeneric("plot", graphics::plot)  ## technically unneeded.  If omitted ..
setMethod("plot", signature(x = "t_test"),  ## .. this -> implicit generic
          function(x, ..., col = c("#418A78", "#FC7115"), main = top, 
                   horizontal = TRUE, border = "black", lwd = 0.5,
                   xlab = if(horizontal) "sample value" else "sample") {
            dat <- rbind(data.frame(f = "x", value = x@x), 
                         data.frame(f = "y", value = x@y))
            top <- capture.output(show(x))
            oldPar <- par(cex.axis = 0.8, cex = 0.8, bty = "n")
            on.exit(par(oldPar))
            graphics::boxplot(value ~ f, dat, col = col, lwd = lwd,
                              horizontal = horizontal, border = border,
                              main = main, xlab = xlab, ...)
            invisible(x)
          })
t0 <- t_test(Days ~ Eth, quine)
plot(t0, names = levels(quine$Eth))
plot(t0, names = levels(quine$Eth), horizontal = FALSE)


## ----line_243_,include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)


## ----line_247_-----------------------------------------------------------------------------------------------------------
#' An S4 class to represent alternavive complex, matrix or list input forms.
#' @export
setClassUnion("xy", c("complex", "matrix", "list"))


## ----line_254_-----------------------------------------------------------------------------------------------------------
#' Arrows
#'
#' Front end to the \code{graphics::arrows} function allowing
#' simplified specification of end points
#'
#' @param x0,y0 two numeric vectors of a single object of class \code{"xy"}
#' @param x1,y1 two numeric vectors of a single object of class \code{"xy"}
#' @param length length of the arrow head barb, in MILLIMETRES, 
#'        see \code{\link{in2mm}}
#' @param angle as for \code{graphics::arrows}
#' @param gap numeric of length 1 or 2 giving the size of any gap to be left
#'        between the arrow and the points which it connects, in MILLIMETRES
#' @param circular logical: should the arrows link up with the initial point? 
#'        (Single location argument only.)
#' @param ... additional arguments passed on to \code{graphics::segments}
#'
#' @return invisible null value
#' @export
#' @examples
#' z <- with(roundTrip, setNames(complex(real = Longitude, 
#'                                       imaginary = Latitude), Locality))
#' plot(z, asp = 1, pch = 20, cex = 0.7, xlab = "Longitude", ylab = "Latitude")
#' arrows(z, cyc(z), col = "red", gap = c(0,1.5))
setGeneric("arrows", function(x0, y0, ...)
  standardGeneric("arrows"))


## ----line_283_-----------------------------------------------------------------------------------------------------------
#' @rdname arrows
#' @export
setMethod("arrows", signature(x0 = "numeric", y0 = "numeric"),
          function(x0, y0, x1, y1, length = 4, angle = 15, gap, ...) {
            length <- mm2in(length)
            if(!missing(gap)) {
              stopifnot(is.numeric(gap) && length(gap) > 0 && 
                          length(gap) < 3 && all(gap >= 0))
              gap <- rep_len(mm2in(gap), length.out = 2)
              if(any(gap > 0)) {
                z0 <- usr2in(x0, y0); z1 <- usr2in(x1, y1)
                gp0 <- gp1 <- z1 - z0
                Mod(gp0) <- pmin(gap[1], Mod(gp0)/3)
                Mod(gp1) <- pmin(gap[2], Mod(gp1)/3)
                xy0 <- in2usr(z0 + gp0); xy1 <- in2usr(z1 - gp1)
                x0 <- Re(xy0); y0 <- Im(xy0)
                x1 <- Re(xy1); y1 <- Im(xy1)
              }
            }
            graphics::arrows(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                             length = length, angle = angle, ... )
})


## ----line_309_-----------------------------------------------------------------------------------------------------------
#' @rdname arrows
#' @export
setMethod("arrows", signature(x0 = "xy", y0 = "xy"),
          function(x0, y0, ...) {
            xy0 <- grDevices::xy.coords(x0)
            xy1 <- grDevices::xy.coords(y0)
            callGeneric(xy0$x, xy0$y, xy1$x, xy1$y, ...)
          })


## ----line_321_-----------------------------------------------------------------------------------------------------------
#' @rdname arrows
#' @export
setMethod("arrows", signature(x0 = "xy", y0 = "missing"),
          function(x0, y0, ..., circular = FALSE) {
            z <- with(grDevices::xy.coords(x0), complex(real = x, imaginary = y))
            if(length(z) < 2) {
              return(invisible(z))
            }
            if(circular) {
              z0 <- z
              z1 <- c(z[-1], z[1])
            } else {
              z1 <- z[-1]
              z0 <- z[-length(z)]
            }
            callGeneric(Re(z0), Im(z0), Re(z1), Im(z1), ...)
          })


## ----line_344_,eval=TRUE,include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)
library(WWRGraphics)

## ----line_348_-----------------------------------------------------------------------------------------------------------
set.seed(1234)
n <- 10
z <- complex(real = runif(n), imaginary = runif(n))
z <- z[order(Arg(z - mean(z)))]
z1 <- complex(argument = Arg(z - mean(z)))/4
plot(c(z, z+z1), asp = 1, type = "n", xlab = "x", ylab = "y")
points(z)
arrows(z, z+z1, col = "red", gap = 2, length = 2)
arrows(z, col = "blue", circular = TRUE, gap = 3, length = 2)
arrows(mean(z), z, gap = 3, length = 1, col = "grey")


## ----line_362_-----------------------------------------------------------------------------------------------------------
z <- with(roundTrip, setNames(complex(real = Longitude, 
                                      imaginary = Latitude), 
                              Locality))
par(cex.axis = 0.8, cex.lab = 0.8, font.main = 1)
plot(z, asp = 1, pch = 20, cex = 0.7, 
     xlim = c(110, 160), ylim = c(-45, -8),
     xlab = "Longitude", ylab = "Latitude", bty = "n")
grid(lty = "solid")
lines(Oz, col = alpha("darkgreen", 0.5))
text(z, names(z), pos = avoid(z), offset = 0.25, cex = 0.7)
arrows(rev(z), col = "red", gap = c(0,1.5), length = 2)
text("top left", c("Rockhampton to Brisbane:", 
                   "The Scenic Route"), family = "serif")


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(Sys.Date()), "\n}") 
toLatex(sessionInfo(), locale = FALSE)

