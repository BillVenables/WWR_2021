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


## ----e1,out.height="1.75in",fig.height=4,fig.width=5.5-------------------------------------------------------------------
e1 <- new.env()
e1$x <- 1:10
e1$y <- runif(10)
ls(envir = e1)
evalq(plot(x, y, type = "b", ylim = 0:1,
           col = "steel blue", main = "Random"), e1); grid(lty="solid")
e2 <- e1
e2$y <- sort(e2$y)
form <- y ~ x
environment(form) <- e1  ## yes, e1.  This is not a typo ...
form  ## enclosing environment given an address, not a name.
plot(form, ylim=0:1, col="blue", type="b", main="Sorted"); grid(lty="solid")


## ----e2------------------------------------------------------------------------------------------------------------------
tBoxCox <- local({  ## An explicit enclosing environment...
  t1 <- function(y, lambda) (y^lambda - 1)/lambda ## |lambda| > eps
  t2 <- function(y, lambda) {  ## |lambda| <= eps, use power series...
    logy <- log(y)
    ly <- lambda*logy
    logy*(1 + ly/2*(1 + ly/3*(1 + ly/4*(1 + ly/5))))
  }
  function(y, lambda, eps = 1/250) 
      with(data.frame(y = y, lambda = lambda),  ## ensure equal length
           ifelse(abs(lambda) > eps, t1(y, lambda), t2(y, lambda)))
})


## ----e3------------------------------------------------------------------------------------------------------------------
library(fractional)
(lambda <- seq(-2, 5/3, length.out = 12))
tst <- lm(outer(MPG.city, lambda, tBoxCox) ~ Weight+Type+Origin, Cars93)
dim(rs <- resid(tst))  ## residual matrix
par(mfrow=n2mfrow(length(lambda)), mar=rep(0,4), bg=alpha("alice blue", 0.5))
for(j in seq_along(lambda)) {
  qqnorm(rs[,j], axes = FALSE, ann = FALSE)
  qqline(rs[,j], col = "red")
  grid(lty = "solid")
  par(usr=c(0,1,0,1))
  text(0.05, 0.95, bquote(lambda == .(as.character(fractional(lambda[j])))),
       cex = 1.5, adj = 0)
  box(col = "grey")
}


## ----e4,fig.show="hold"--------------------------------------------------------------------------------------------------
with(e1, {  ## works with environments too!
  linInt <<- approxfun(x, y, rule = 2)  ## "flat" extrapolation
  splInt <<- splinefun(x, y, method = "hyman") ## monotonic spline
})
linInt
ls(envir = environment(linInt))  ## in the sack...
plot(form, xlim = c(0, 11), ylim = c(-0.15, 1.15))
curve(linInt, add = TRUE, col="steel blue", n = 500)
curve(splInt, add = TRUE, col="rosy brown", n = 500)
legend("topleft", c("linear interpolation", "monotonic spline"), 
       bty = "n", lty = "solid", lwd = 2.5, 
       col = c("steel blue", "rosy brown"))
grid(lty="solid")


## ----a0,include=FALSE----------------------------------------------------------------------------------------------------
library(english)
english.object_size <- function(x, ...) {
  x <- unclass(x)
  v <- english:::english.numeric(x)
  cat(as.character(v), "bytes\n")
  invisible(x)
}

## ----a1------------------------------------------------------------------------------------------------------------------
str(Oz)
Aus <- local({
  Oz <- Oz  ## keep a copy here!
  
  function (add = FALSE, xlim = c(112.913865, 154.417058),
                         ylim = c(-43.738846, -9),
            xlab = "", ylab = "", axes = FALSE, ...) {
### the xy-list 'Oz' is held in the local environment of this function
    if (!add) {
      plot(xlim, ylim, asp = 1, type = "n", xlab = xlab, ylab = ylab, 
           axes = axes, ...)
    }
    lines(Oz, ...)
  }
})

ls(envir = environment(Aus))

english(object.size(Aus))                  ## not the full story!
english(object.size(serialize(Aus, NULL))) ## more honest!

## what it actually does!
Aus(xlim = c(140, 155), ylim = c(-30, -9), col = "rosy brown")
box()


## ----sa1,eval=FALSE------------------------------------------------------------------------------------------------------
## x <<- sqrt(y)


## ----sa2-----------------------------------------------------------------------------------------------------------------
fn <- local({
  x <- 0     ## to receive 
  n <- 0     ## super-assigned values
  function(y = x) {
    x <<- y
    n <<- n + 1
    return(c(No = n, Value = x, Square = y^2))
  }
})
fn()
fn(3:5)      
fn()


## ----s1,out.height="0.5\\textheight",fig.height=7,fig.width=7------------------------------------------------------------
library(sudokuAlt)
fetchAUGame(difficulty = "tough") %>% solve %>% plot  


## ----s2,out.height="0.75\\textheight",fig.height=8,fig.width=8,x=set.seed(1234)------------------------------------------
seedGame(4) %>% solve %>% plot


## ----s3,eval=FALSE,tidy=FALSE--------------------------------------------------------------------------------------------
## solveGame <- function(game) {
##   n <- as.integer(round(sqrt(nrow(game))));  stopifnot(n > 1 && n < 6)
## 
##   set <- if(n <= 3) as.character(1:n^2) else LETTERS[1:n^2] ## legals
##   storage.mode(game) <- "character"
##   is.na(game[!(game %in% set)]) <- TRUE
##   nSet <- 1:n^2  ## work in integers
## 
##   toMatrix <- function(game) {  ## inverse of toArray
##     game[] <- set[game]
##     dim(game) <- c(n, n)^2
##     game
##   }
## 
##   toArray <- function(mat) {  ## inverse of toMatrix
##     array(as.integer(match(mat, set)), dim = c(n,n,n,n))
##   }
## 
##   conflict <- function(section)
##       any(duplicated(na.omit(as.vector(section))))
## 
##   invalid <- function(game) {
##     for(i in 1:n) for(j in 1:n) {
##       if(conflict(game[,i,,j]) ||  ## 'same block'
##          conflict(game[i,j,,]) ||  ## 'same row'
##          conflict(game[,,i,j]))    ## 'same column'
##           return(TRUE)
##     }
##     FALSE
##   }
##   findSolution <- function(game) {
##     if(invalid(game)) return(FALSE)  ## dead end.  Go back.
##     while(anyNA(game)) {  ## anyNA() is only  in R 3.0.2 and later.
##       holes <- which(is.na(game), arr.ind = TRUE)  ## a splendid trick!
##       nr <- nrow(holes)
##       fills <- vector("list", nr)
##       lengths <- integer(nr)
##       for(j in 1:nr) {
##         i <- holes[j,]
##         fills[[j]] <- setdiff(nSet, c(game[    ,i[2],    ,i[4]],
##                                       game[i[1],i[2],    ,    ],
##                                       game[    ,    ,i[3],i[4]]))
## 
##         lengths[j] <- length(fills[[j]])
##         if(lengths[j] == 0) return(FALSE)
##       }
##       if(any(h <- which(lengths == 1))) {
##         game[holes[h,,drop = FALSE]] <- unlist(fills[h])
##         if(invalid(game)) return(FALSE)
##       } else {  ## only holes with multiple alternatives
##         m <- which.min(lengths)
##         entries <- fills[[m]]
##         pos <- holes[m,,drop = FALSE]
##         for(e in entries) {
##           game[pos] <- e
##           h <- findSolution(game)   ## recursive call
##           if(!isFALSE(h)) return(h) ## Bingo!
##         }
##         return(FALSE)  ## dud game, no solutions!
##       }
##     }
##     game  ## should never reach this point of the code.
##   }
## 
##   ## the business starts here
##   solution <- findSolution(toArray(game))
##   if(isFALSE(solution)) NULL else
##   structure(toMatrix(solution), game = game, class = "sudoku")
## }


## ----line_371_-----------------------------------------------------------------------------------------------------------
base::solve
sudokuAlt:::solve.sudoku


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo())

