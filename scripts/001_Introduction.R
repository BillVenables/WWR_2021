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


## ----line_032_,include=FALSE---------------------------------------------------------------------------------------------
rm(session)


## ----fileops, eval=FALSE-------------------------------------------------------------------------------------------------
## if(!dir.exists("data")) dir.create("data")
## file.copy("myData.csv", "data/")
## file.remove("myData.csv")  ## now that a saved copy is available in data/


## ----search,out.lines=6--------------------------------------------------------------------------------------------------
search() %>% noquote()             ## no "quotation" marks, please!


## ----line_248_,out.lines=6-----------------------------------------------------------------------------------------------
ls("package:WWRUtilities") %>% noquote()


## ----line_252_-----------------------------------------------------------------------------------------------------------
pi <- 22/7                    ## good enough for gevernment purposes
find("pi", numeric = TRUE)    ## gives position in search path
print(c(pi, base::pi), digits = 20)


## ----line_266_,out.lines=6-----------------------------------------------------------------------------------------------
search()[1:3]
myList <- list(x = 3, y = rnorm(10000), z = TRUE)
attach(myList)
ls()
search()[1:4]
x <- 4
find("x")
rm(x)
x


## ----line_279_-----------------------------------------------------------------------------------------------------------
detach("myList")
search()[1:3]
x
ls()
myList$x


## ----line_317_-----------------------------------------------------------------------------------------------------------
find("quine"); head(quine, 2)
##
## compare:
tapply(quine$Days, list(quine$Sex, quine$Age), mean)
##
## with using with()..
with(quine, {
  tapply(Days, list(Sex, Age), mean)
})


## ----line_330_-----------------------------------------------------------------------------------------------------------
quine_ext <- within(quine, {
  LSE         <- Lrn:Sex:Eth  
  levels(Age) <- paste0("Form", 0:3)
  Means       <- ave(Days, LSE, Age, FUN = mean)
  Residuals   <- Days - Means
}) %>% select(Eth:Lrn, LSE, Days, Means, Residuals)  ## pipes will occur a lot!
head(quine_ext, 3)
with(quine_ext, {
  tapply(Days, list(Age, LSE), median)
})


## ----line_374_,eval=FALSE------------------------------------------------------------------------------------------------
## Attach(lib = .Robjects)
##                      ## check for duplicate dataset copies
## ._objects <- intersect(ls(".Robjects"), ## could have used Ls(lib=.Robjects)
##                        ls("package:WWRCourse"))
## if(length(._objects) > 0) {
##   Remove(list = ._objects, lib = .Robjects)  ## remove duplicates
## }
## rm(._objects)        ## a hidden object, seen only by ls(all = TRUE)
## 
## Attach()             ## the .R_Cache folder, same as Attach(lib = .R_Cache)
## Remove(list = Ls())  ## clean up working .R_Cache,
## rm(list = ls())      ## clean up workspace ... for now.


## ----line_434_-----------------------------------------------------------------------------------------------------------
(x <- structure(1:20, names = letters[1:20]))
## compare (names attribute retained)
x[5]
## with (names attribute discarded)
x[[5]]
## give it a class
class(x) <- "shuffle"; x
## now provide a pring method for such objects
print  ## the generic function in the base package
print.shuffle <- function(x, ...) {
  cat("This shuffle is:\n")
  print.default(sample(x))
  invisible(x)
}
x            ## implicit call to print(x)
x            ## implicit call to print(x) again


## ----line_455_,out.height="0.6\\textheight",out.lines=6------------------------------------------------------------------
find("janka")
# head(janka)
plot(Hardness ~ Density, janka, pch=20, bty="n") ## plot with formula
fm <- lm(janka$Hardness ~ janka$Density)  ## poor. Cannot predict from fm
fm <- with(janka, lm(Hardness ~ Density)) ## bad. Object not fully self-describing
fm <- lm(Hardness ~ Density, data = janka)## good. Self-describing and prediction OK
class(fm)
methods(class = class(fm))
summary(fm)$coefficients
abline(coef(fm), col = "red")

## ----line_467_,out.height="0.8\\textheight"------------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(fm)   ## call to the "lm" method function of plot()


## ----line_560_-----------------------------------------------------------------------------------------------------------
library(WWRGraphics)
brownish <- alpha("rosy brown", 0.5)
pinkish <- alpha("hot pink", 0.5)
z <- with(roundTrip, 
          complex(real = Longitude, imaginary = Latitude) %>% 
            setNames(Locality))
plot(z, asp = 1, ann = FALSE, bty = "n", pch = 20)
lines(Oz, col = brownish)
text(z, names(z), cex = 0.7, pos = avoid(z), offset = 0.25, col="dark grey")
arrows(z, col = "steel blue", gap = 1, circular = TRUE, length = 2)
dists <- gcd_km(z, cyc(z)) %>% round()
text((z + cyc(z))/2, dists, col = "dark green", cex = 0.7, font = 4)
##
## add city sizes
## 
circles(Latitude ~ Longitude, roundTrip, radii = sqrt(Population),
       fill = pinkish, colour = pinkish, maxradius = 0.5)


## ----line_581_,out.height="0.45\\textheight"-----------------------------------------------------------------------------
par(mar = rep(0, 4))
set.seed(20200211)
z <- complex(real = runif(300), imaginary = runif(300))
z <- z[order(Arg(z - mean(z)))]
plot(z, asp = 1, ann = FALSE, axes = FALSE, pch = ".", 
     xlim = 0:1, ylim = 0:1)
greenish <- alpha("dark green", 0.7)
rect(0, 0, 1, 1, fill = greenish, colour = greenish)
polygon(z, fill = getColors("French beige"), colour = "beige")


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(Sys.Date()), "\n}") 
toLatex(sessionInfo(), locale = FALSE)

