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


## ----c0------------------------------------------------------------------------------------------------------------------
rvball <- local({
  pi <- base::pi  ## make sure it is the right one

  function(d)
  structure(exp(d/2 * log(pi) - d * log(2) - lgamma(d/2 + 1)),
            names = as.character(d))
})
floor(1/rvball(1:10))


## ----c1------------------------------------------------------------------------------------------------------------------
mcrvball <- function(d, N = 100000, blocksize = 10000) {
  n2 <- inside <- 0
  while(n2 < N) {
    n1 <- n2
    n2 <- min(n2 + blocksize, N)
    No <- n2 - n1
    samp <- matrix(runif(No * d, -1, 1), No, d)
    inside <- inside + sum(rowSums(samp^2) < 1)
  }
  res <- list(dimensions = d, inside = inside,
              total = N, call = match.call())
  class(res) <- "mcrvball"
  res
}


## ----c1a-----------------------------------------------------------------------------------------------------------------
mcrvball2 <- function(d, N = 100000, blocksize = 10000) {
  chunks <- idiv(N, chunkSize = blocksize) ## division iterator
  inside <- foreach(No = chunks, .combine = sum) %dopar% {
    samp <- matrix(runif(No*d, -1, 1), No, d)
    sum(rowSums(samp^2) < 1)
  }
  structure(list(dimensions = d, inside = inside,
                 total = N, call = match.call()),
            class = "mcrvball")
}


## ------------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(doParallel))
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)              ## not needed in "parallel" package

tst <- mcrvball2(10, N = 1e7, blocksize = 1e5)

stopCluster(cl)
rm(cl)


## ----c2------------------------------------------------------------------------------------------------------------------
print.mcrvball <- function(x, ...) {
  with(x, cat("Dim.:", dimensions,
              "Estimated:", signif(inside/total, 4),
              "Actual:", signif(rvball(dimensions), 4), "\n"))
  invisible(x)
}

Ops.mcrvball <- function(e1, e2) {  ## group generic
  if(inherits(e1, "mcrvball"))
  e1 <- with(e1, inside/total)

  if(!missing(e2) && inherits(e2, "mcrvball"))
  e2 <- with(e2, inside/total)
  NextMethod()
}


## ----c3------------------------------------------------------------------------------------------------------------------
for(i in 4:10) print(mcrvball(i, 1000000))


## ----c4------------------------------------------------------------------------------------------------------------------
r <- numeric(7)
for(d in 4:10) r[d-3] <- floor(1/mcrvball(d)) ; r


## ----c4a-----------------------------------------------------------------------------------------------------------------
p10 <- mcrvball(10)
p10a <- update(p10, N = 1000000, blocksize = 100000)
c(1/p10, 1/p10a)
"%+%" <- function(e1, e2)
    UseMethod("%+%")
"%+%.mcrvball" <- function(e1, e2) {
    if(e1$dimensions != e2$dimensions) stop("ball dimensions differ!")
    res <- list(dimensions = e1$dimensions, inside = e1$inside + e2$inside,
                total = e1$total + e2$total, call = e1$call)
    class(res) <- "mcrvball"
    res
}
# p10 %+% p10a
floor(1/(p10 %+% p10a))


## ------------------------------------------------------------------------------------------------------------------------
Mcrvball <- Vectorize(mcrvball, vectorize.args = c("d", "N"), SIMPLIFY = FALSE)
Mcrvball(4:8, N = 1e6, blocksize = 1e5)


## ----tp1-----------------------------------------------------------------------------------------------------------------
subdir <- system.file("extdata", package = "WWRCourse")
dir(subdir, pattern = "\\.txt.gz$")    ## NB regular expression


## ----tp2-----------------------------------------------------------------------------------------------------------------
gen <- file.path(subdir, "Genesis.txt.gz")
rev <- file.path(subdir, "Revelations.txt.gz")
Genesis <- scan(gzfile(gen), what = "")  ## no need to gunzip()
Revelations <- scan(gzfile(rev), what = "")
rbind(head(Genesis), head(Revelations)) %>% noquote


## ----tp3-----------------------------------------------------------------------------------------------------------------
grep("^Gen\\.[[:digit:]]+$", Genesis)[1:5] ## the idea
chapterLengths <- function(txt,   ## cement it in a function & check
            book = substring(deparse(substitute(txt)), 1, 3)) {
  regex <- paste0("^", book, "\\.[[:digit:]]+$")
  where <- grep(regex, txt)
  diff(where - seq_along(where))
}
GenC <- chapterLengths(Genesis)
RevC <- chapterLengths(Revelations)
rbind(Gen = c(n = length(GenC), median = median(GenC)),
      Rev = c(n = length(RevC), median = median(RevC)))


## ----tp4,out.height="0.9\\textheight",fig.width=10,fig.height=7----------------------------------------------------------
plot(GenC, type="h", col = "#418A78", xlab = "Chapter #",
     ylab="Words", ylim = range(0, GenC, RevC))
points(RevC, pch=20, col="#FC7115", cex=1.5)
legend("topright", c("Genesis", "Revelations"), lty = c("solid", NA),
       pch=c(NA, 20), col = c("#418A78", "#FC7115"), bty="n")


## ----line_270_,fig.height=7,fig.width=11---------------------------------------------------------------------------------
gendat <- data.frame(x = c(1, rep(seq_along(GenC), each = 3)),
                     y = c(0, rbind(0, GenC, 0)))
revdat <- data.frame(x = seq_along(RevC), y = RevC)
ggplot() + aes(x,y) + geom_path(data = gendat, colour = "#418A78") +
  geom_point(data = revdat, colour = "#FC7115", size = 2) +
  geom_hline(yintercept = 0, colour = "grey", size = 0.5) +
  labs(x = "Chapter #", y = "Words") +
  annotate("text", x = 50, y = 1800, label = "Genesis",
           colour = "#418A78", hjust = 1) +
  annotate("text", x = 50, y = 1750, label = "Revelations",
           colour = "#FC7115", hjust = 1) + theme_bw()


## ----tp5-----------------------------------------------------------------------------------------------------------------
Gen <- grep("^Gen\\.[[:digit:]]+$", Genesis, invert = TRUE, value = TRUE)
Rev <- grep("^Rev\\.[[:digit:]]+$", Revelations, invert = TRUE, value = TRUE)
noquote(rbind(head(Gen), head(Rev)))


## ----tp6-----------------------------------------------------------------------------------------------------------------
GenP <- diff(c(0, grep("[[:punct:]]$", Gen)))
RevP <- diff(c(0, grep("[[:punct:]]$", Rev)))
rbind(Gen = head(GenP), Rev = head(RevP))


## ----tp7,out.lines=8,fig.show="hold"-------------------------------------------------------------------------------------
Phrase <- rbind(data.frame(Book = "Gen", Length = GenP),
                data.frame(Book = "Rev", Length = RevP))
tab <- with(Phrase, table(Book, Length)); t(tab)
colours <- c(Genesis = "#418A78", Revelations = "#FC7115")
par(cex.axis = 0.8)
barplot(tab/rowSums(tab), beside = TRUE, main = "Phrase Length",
        fill = colours, colour = colours)
grid()


## ----tp8,out.lines=4,fig.show="hold"-------------------------------------------------------------------------------------
GenW <- nchar(gsub("[^[:alpha:]]", "", Gen))
RevW <- nchar(gsub("[^[:alpha:]]", "", Rev))
noquote(rbind(head(Rev), head(RevW)))  ## word 5 comma excluded
Words <- rbind(data.frame(Book = "Gen", Length = GenW),
               data.frame(Book = "Rev", Length = RevW))
tab <- with(Words, table(Book, Length)); t(tab)
par(cex.axis = 0.8)
barplot(tab/rowSums(tab), beside = TRUE, main = "Word Length",
        fill = colours, colour = colours)
grid()


## ----c7------------------------------------------------------------------------------------------------------------------
convolve0 <- function(a, b) {
  ab <- rep(0, length(a) + length(b) - 1)
  for(i in 1:length(a))
    for(j in 1:length(b))
      ab[i+j-1] <- ab[i+j-1] + a[i]*b[j]
  ab
}
###
convolve1 <- function(a, b) {
  ab <- rep(0, length(a) + length(b) - 1)
  ind <- 1:length(a)
  for(j in 1:length(b)) {
    ab[ind] <- ab[ind] + a*b[j]
    ind <- ind + 1
  }
  ab
}


## ----c8------------------------------------------------------------------------------------------------------------------
convolve1a <- function(a, b) {
  if(length(a) < length(b)) Recall(b,a) else {
    ab <- rep(0, length(a) + length(b) - 1)
    ind <- 1:length(a)
    for(j in 1:length(b)) {
      ab[ind] <- ab[ind] + a*b[j]
      ind <- ind + 1
    }
    ab
  }
}
###
convolve2 <- function(a, b) {
  p <- outer(a, b)
  as.vector(tapply(p, row(p) + col(p), sum))
}
### Uses much less memory than convolve2
convolve2a <- function(a, b)
  as.vector(tapply(outer(a, b),
            outer(seq(along = a), seq(along = b), "+"), sum))


## ----hw------------------------------------------------------------------------------------------------------------------
convolve_hw <- function(a, b) {
  stopifnot(require(dplyr))
  data.frame(x = as.vector(outer(a, b)),
             g = as.vector(outer(seq_along(a),
                                 seq_along(b), "+"))) %>%
    group_by(g) %>%
    summarise(conv = sum(x), .groups = 'drop') %>%
    .[["conv"]]
}


## ----c9------------------------------------------------------------------------------------------------------------------
convolve3 <- function(a, b) {
  if(!is.loaded("VR_convolve")) {
    path <- file.path("SharedObjects",
                      paste("VR_convolve",
                            .Platform$dynlib.ext, sep=""))
    dyn.load(path)
  }
  storage.mode(a) <- "double"
  storage.mode(b) <- "double"

  .C("VR_convolve",
     a,
     length(a),
     b, length(b),
     ab = double(length(a) + length(b) - 1))$ab
}


## #include <Rcpp.h>

## using namespace Rcpp;

## 
## // [[Rcpp::export]]

## NumericVector convolve3a(NumericVector x, NumericVector y)

## {

##     int nx = x.size(), ny = y.size(), nz = nx + ny - 1;

##     NumericVector z(nz);  // set to 0 on creation. NB z() not z[] here!

## 
##     for(int i = 0; i < nx; ++i) {

##         for(int j = 0; j < ny; ++j) {

##             z[i+j] += x[i]*y[j];

##         }

##     }

##     return z;

## }

## ----showit,eval=FALSE---------------------------------------------------------------------------------------------------
## #include <Rcpp.h>
## using namespace Rcpp;
## 
## // [[Rcpp::export]]
## NumericVector convolve3a(NumericVector x, NumericVector y)
## {
##     int nx = x.size(), ny = y.size(), nz = nx + ny - 1;
##     NumericVector z(nz);  // set to 0 on creation. NB z() not z[] here!
## 
##     for(int i = 0; i < nx; ++i) {
##         for(int j = 0; j < ny; ++j) {
##             z[i+j] += x[i]*y[j];
##         }
##     }
##     return z;
## }


## ----c3make,eval=FALSE---------------------------------------------------------------------------------------------------
## library(Rcpp)
## sourceCpp("src/convolve3a.cpp")

## ----line_508_,include=FALSE---------------------------------------------------------------------------------------------
library(Rcpp)


## ----line_512_-----------------------------------------------------------------------------------------------------------
convolve3a


## ------------------------------------------------------------------------------------------------------------------------
Rcpp::cppFunction('
NumericVector convolve3a(NumericVector x, NumericVector y)
{
    int nx = x.size(), ny = y.size(), nz = nx + ny - 1;
    NumericVector z(nz);  // set to 0 on creation. NB z() not z[] here!
    for(int i = 0; i < nx; ++i) {
        for(int j = 0; j < ny; ++j) {
          z[i+j] += x[i]*y[j];
        }
    }
    return z;
}
')


## ----c10-----------------------------------------------------------------------------------------------------------------
a <- 1:3; b <- 4:7
rbind(convolve0(a,b), convolve1(a,b), convolve1a(a,b),
      convolve2(a,b), convolve2a(a,b), convolve3(a,b),
      convolve3a(a,b), convolve_hw(a, b))


## ----c10a,out.height="2.5in",fig.width=7,fig.height=5--------------------------------------------------------------------
library(microbenchmark)
a <- 1:300; b <- 4:7
(b <- microbenchmark(convolve_hw(a, b),
                     convolve0(a,b),
                     convolve1(a,b),
                     convolve1a(a,b),
                     convolve2(a,b),
                     convolve2a(a,b),
                     convolve3(a,b),
                     convolve3a(a,b))) %>% summary() %>% arrange(median) %>%
                                .[, cs(expr, min, median, mean, max, cld)]
suppressMessages(autoplot(b)) + theme_economist()


## ----c11,cache=TRUE------------------------------------------------------------------------------------------------------
a <- 1:1000; b <- 1:10000
library(rbenchmark)
benchmark(convolve0 (a,b), convolve1 (a,b), convolve1 (b,a),
          convolve1a(a,b), convolve1a(b,a), convolve2 (a,b),
          convolve2a(a,b), convolve3 (a,b), convolve3a(a,b),
          columns = c("test", "replications", "elapsed", "relative"),
          order = "relative", relative = "elapsed", replications = 20)


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo(), locale = FALSE)

