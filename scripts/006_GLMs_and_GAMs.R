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
knitr::opts_chunk$set(
  fig.path = paste0('Fig/', .infile, "_"),
  message = FALSE,
  warning = FALSE) #$
session <- sub("^0+", "", sub("[^[:digit:][:alpha:]].*$", "", .infile))


## ----c1aa,eval=TRUE,size="tiny"------------------------------------------------------------------------------------------
attach(birthwt)
race <- factor(race, labels = c("white", "black", "other"))
table(ptl)
ptd <- factor(ptl > 0)
table(ftv)
ftv <- factor(ftv)
levels(ftv)[-(1:2)] <- "2+"
table(ftv)  # as a check
bwt <- data.frame(low = factor(low), age, lwt, race,
   smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
detach(); rm(race, ptd, ftv)


## ----c2aa,cache=FALSE----------------------------------------------------------------------------------------------------
BirthWt <- birthwt %>% 
  within({
    low <-   case_when(low == 0 ~ "Normal",
                       low == 1 ~ "Low") %>% factor() ## why? why factor??
    race <-  case_when(race == 1 ~ "White",
                       race == 2 ~ "Black",
                       race == 3 ~ "Other")
    smoke <- ifelse(smoke > 0, "Some", "None")
    ptl <-   ifelse(ptl > 0, "1+", "0")
    ht <-    ifelse(ht > 0, "Yes", "No")
    ui <-    ifelse(ui > 0, "Yes", "No")
    ftv <-   ifelse(ftv > 1, "2+", ftv)  ## change numeric -> character
  }) %>% 
  select(-bwt)  ## not useful for modelling.
Store(BirthWt)
head(BirthWt, 2)

## ----include=FALSE-------------------------------------------------------------------------------------------------------
if("package:MASS" %in% search()) detach("package:MASS")


## ----turnoff-------------------------------------------------------------------------------------------------------------
options(show.signif.stars = FALSE)
ls("package:WWRUtilities", pattern = "^(step|drop)") %>% noquote()


## ----c1------------------------------------------------------------------------------------------------------------------
BW0 <- glm(low ~ ., binomial, BirthWt)
dropterm(BW0)


## ----c2------------------------------------------------------------------------------------------------------------------
sBW0 <- step_AIC(BW0, scope = list(
  lower = ~1, 
  upper = ~.^2+poly(age, 2)+poly(lwt,2)))
dropterm(sBW0)


## ----c3,cache=FALSE, message=TRUE----------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(mgcv))
BW1 <- bam(low ~ smoke*ui + ht + s(lwt) + ptl + s(age) +
           poly(age, 2)*ftv, family = binomial, data = BirthWt, 
           control = gam.control(trace = TRUE))
BW1 <- update(BW1, control = gam.control(trace = FALSE))  ## turn off tracing

## ----include=FALSE-------------------------------------------------------------------------------------------------------
if("package:MASS" %in% search()) detach("package:MASS")


## ----c3a,eval=FALSE------------------------------------------------------------------------------------------------------
## anova(BW1)


## ----c3b,echo=FALSE------------------------------------------------------------------------------------------------------
txt <- capture.output(anova(BW1))
txt <- txt[nchar(txt) > 0]
cat(txt, sep="\n")
rm(txt)


## ----mfv-----------------------------------------------------------------------------------------------------------------
mostFreq <- function(x, ...) {
  UseMethod("mostFreq")
}

mostFreq.logical <- function(x, ...) {
  tx <- as.vector(table(x))
  tx[2] > tx[1]
}

mostFreq.character <- function(x, ...) {
  tx <- table(x)
  names(tx)[which.max(tx)]
}

mostFreq.factor <- function(x, ...)
    mostFreq.character(as.character(x))

mostFreq.numeric <- stats::median.default  ## check argument names
                                           ## covers class 'integer' as well  
# Store(list = ls(pattern = "^mostFreq"), lib = .Robjects)


## ----c6------------------------------------------------------------------------------------------------------------------
all.vars(formula(BW1))
pBirthWt <- with(BirthWt,
    expand.grid(smoke = mostFreq(smoke), 
                ui = mostFreq(ui),
                ht = mostFreq(ht), 
                lwt = mostFreq(lwt),
                ptl = mostFreq(ptl), 
                age = min(age):max(age),   ## first branch
                ftv = sort(unique(ftv))))  ## second branch 
pBirthWt$pBW1 <- predict(BW1, pBirthWt, type = "response")
p0 <- ggplot(pBirthWt) + aes(x = age, y = pBW1) +
  geom_line() + ylim(0,1) + ylab("Pr(low birth weight)") + 
  facet_grid(. ~ ftv)


## ----c6a-----------------------------------------------------------------------------------------------------------------
pBirthWt <- within(pBirthWt, {
    rngs <- do.call(cbind, with(BirthWt, tapply(age, ftv, range)))
    pBW1a <- pBW1
    is.na(pBW1a[age < rngs[1, ftv] | age > rngs[2, ftv]]) <- TRUE
    rm(rngs)
})
p1 <- ggplot(na.omit(pBirthWt)) + aes(x = age, y = pBW1a) +
    geom_line() + ylim(0,1) + # theme_bw() +
    ylab("Pr(low birth weight)") + facet_grid(. ~ ftv)

# gridExtra::grid.arrange(p0, p1, nrow=2)
p0/p1


## ----line_302_,fig.height=10,fig.width=12--------------------------------------------------------------------------------
library(visreg)
p0 <- visreg(BW1, xvar = "age", by = "ftv", scale = "resp", ylim = 0:1, plot = FALSE)
p1 <- visreg(BW1, xvar = "smoke", by = "ui", scale = "resp", ylim = 0:1, plot = FALSE)
plot(p0, gg = TRUE)/plot(p1, gg = TRUE)


## ----line_312_-----------------------------------------------------------------------------------------------------------
fname <- system.file("extdata", "churnData.csv.gz", package = "WWRCourse")
churnData <- read_csv(gzfile(fname),     ## neater read (for eventual notebook)
                      col_types = cols(.default = col_double(),
                                       state = col_character(),
                                       area_code = col_character(),
                                       international_plan = col_character(),
                                       voice_mail_plan = col_character(),
                                       churn = col_character(),
                                       sample = col_character())) %>% 
  within({
    area_code <- sub("^area_code_", "", area_code) %>% factor()
    churn <- (churn == "no") + 0      ## replace by a binary response
  }) %>% 
  untibble()
names(churnData) <- sub("^(total|number)_", "", names(churnData)) ## neater


## ----line_332_,out.lines=8-----------------------------------------------------------------------------------------------
names(churnData) %>% noquote()
churn_train <- churnData %>% 
  filter(sample == "train") %>% 
  select(-sample, -ends_with("charge"))
churn_test  <- churnData %>% 
  filter(sample == "test" ) %>% 
  select(-sample, -ends_with("charge"))


## ----line_343_,cache=FALSE-----------------------------------------------------------------------------------------------
churnAIC <- glm(churn ~ ., binomial, churn_train) %>% 
  step_AIC(scope = list(lower = ~state))
churnBIC <- glm(churn ~ ., binomial, churn_train) %>% 
  step_BIC(scope = list(lower = ~state))

## ----include=FALSE-------------------------------------------------------------------------------------------------------
if("package:MASS" %in% search()) detach("package:MASS")


## ----line_352_,eval=FALSE------------------------------------------------------------------------------------------------
## dropterm(churnAIC) %>% booktabs(digits = c(0,0,2,2,2,6))

## ----l375,results="asis",echo=FALSE--------------------------------------------------------------------------------------
dropterm(churnAIC) %>% booktabs(digits = c(0,0,2,2,2,6))


## ----line_358_,eval=FALSE------------------------------------------------------------------------------------------------
## dropterm(churnBIC) %>% booktabs(digits = c(0,0,2,2,2,6))

## ----l385,echo=FALSE,results="asis"--------------------------------------------------------------------------------------
dropterm(churnBIC) %>% booktabs(digits = c(0,0,2,2,2,6))


## ----line_364_-----------------------------------------------------------------------------------------------------------
(confAIC <- table(actual = churn_test$churn, 
                  predicted = (predict(churnAIC, churn_test) > 0) + 0))
(confBIC <- table(actual = churn_test$churn, 
                  predicted = (predict(churnBIC, churn_test) > 0) + 0))


## ----line_371_-----------------------------------------------------------------------------------------------------------
c(AIC = (1 - sum(diag(confAIC))/sum(confAIC))*100,
  BIC = (1 - sum(diag(confBIC))/sum(confBIC))*100) %>% round(2)

## ------------------------------------------------------------------------------------------------------------------------



## ----d1,echo=FALSE,out.height="2.25in"-----------------------------------------------------------------------------------
Attach(lib = .Robjects)
pal <- colorRampPalette(c("white","lemonchiffon","orange","red"))

NPF0(col = grey(0.95))
ncols <- with(teff, 1 + ceiling(log(1 + TigerE)))
cols <- pal(max(ncols))[ncols]

points(Latitude ~ Longitude, teff, pch = 15, cex = 0.5, col=cols)
lines(ShortCC, col="blue", lwd=3)
lines(Oz)


## ----d2------------------------------------------------------------------------------------------------------------------
Annual <- function (day, k = 4) {  ## day of the year, starting from 0
  theta <- Arg(complex(argument = 2*base::pi*day/364.25))
  X <- matrix(0, length(theta), 2 * k)
  nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
  dimnames(X) <- list(names(day), nam)
  m <- 0
  for (j in 1:k) {
    X[, (m <- m + 1)] <- cos(j * theta)
    X[, (m <- m + 1)] <- sin(j * theta)
  }
  X
}
Store(Annual,Tigers, lib = .Robjects)


## ----d3,cache=FALSE,.k=1,message=TRUE------------------------------------------------------------------------------------
TModelGLM <- glm(Psem/Total ~ ns(Coast, 10) +
                   ns(Sea, 5) + 
                   ns(Depth, 5) +
                   ns(Mud, 4) + 
                   Annual(DayOfYear, 4)*Sea, 
                 family = quasibinomial,
                 data = Tigers, weights = Total/mean(Total), 
                 trace = TRUE)
TModelGLM$call$trace <- NULL  ## for future updating
Store(TModelGLM, lib = .Robjects)
# (nam <- names(model.frame(TModelGLM)))  ## for term plotting
# nam <- nam[2:7]                         ## terms to plot

## ----include=FALSE-------------------------------------------------------------------------------------------------------
if("package:MASS" %in% search()) detach("package:MASS")


## ----d4,out.height="45%",fig.width=10,fig.height=5,fig.show="hold"-------------------------------------------------------
# layout(matrix(1:6, 2, 3, byrow=TRUE))   ## 2 x 3 array of plots
# termplot(TModelGLM, terms = nam, se = TRUE, rug=TRUE,
#          ylim = c(-5, 5))
# layout(1)
visreg(TModelGLM, "Coast", scale = "resp") ## issues warning...
visreg(TModelGLM, "Sea", by = "DayOfYear", scale = "resp")

## ----line_525_,include=FALSE---------------------------------------------------------------------------------------------
Attach()  ## memory restorer


## ----d6,cache=FALSE,.k=1-------------------------------------------------------------------------------------------------
TM2 <- update(TModelGLM, . ~ . + ns(ElapsedDays, 7))
Store(TM2, lib = .Robjects)

## ----include=FALSE-------------------------------------------------------------------------------------------------------
if("package:MASS" %in% search()) detach("package:MASS")


## ----d6anova,comment=""--------------------------------------------------------------------------------------------------
anova(TModelGLM, TM2, test = "F")


## ----d7,fig.height=5, fig.width=12, out.width="100%"---------------------------------------------------------------------
visreg(TM2, "ElapsedDays", xlab = "Days since 1970-01-01")


## ----d8,out.lines=10,cache=FALSE,message=TRUE----------------------------------------------------------------------------
suppressPackageStartupMessages(library(mgcv))
TModelGAM <- bam(Psem/Total ~ s(Longitude, Latitude) +  ## bam(...) oscillates!
                 te(DayOfYear, Sea,   k = c(5, 5), bs = c("cc", "cs")) +
                 te(DayOfYear, Depth, k = c(5, 5), bs = c("cc", "cs")) +
                 te(Sea,       Depth, k = c(5, 5), bs = c("cs", "cs")) + ## NB!
                 s(Mud, k = 5), 
                 family = quasibinomial, data = Tigers,
                 knots = list(DayOfYear = seq(1, 365.25, length = 5)),
                 control = gam.control(trace = TRUE),    ## to check progress
                 weights = Total/mean(Total))              ## takes forever!
TModelGAM_NS <- update(TModelGAM, . ~ . + ns(ElapsedDays, 10),
                       mustart = fitted(TModelGAM))  ## non-stationary
Store(TModelGAM, TModelGAM_NS, lib = .Robjects)


## ----vis,out.width="95%",fig.width=10,fig.height=5.5,fig.show="hold"-----------------------------------------------------
layout(rbind(1:2))
plot(TModelGAM_NS, select = 1, asp = 1)
lines(Oz, col = alpha("dark green", 0.8))
title(main = "Lon x Lat, isotropic")
for(j in 2:6)
    plot(TModelGAM_NS, select = j)
layout(rbind(1:2))
vis.gam(TModelGAM_NS, view = c("Longitude","Latitude"),
        r = 1000, theta = 30, phi = 15)
title(main = "Lon x Lat, isotropic")
vis.gam(TModelGAM_NS, view = c("DayOfYear","Sea"),
        r = 1000, theta = 30, phi = 15)
title(main = "Day of year x Sea")
vis.gam(TModelGAM_NS, view = c("DayOfYear","Depth"),
        r = 1000, theta = 30, phi = 15)
title(main = "Day of year x Depth")
vis.gam(TModelGAM_NS, view = c("Depth","Mud"),
        r = 1000, theta = 30, phi = 15)
title(main = "Depth x Mud")


## ----e1,echo=FALSE,out.height="2.25in"-----------------------------------------------------------------------------------
Attach()
par(mar=c(0,0,0,0))
plot(cc, type = "n", ann=FALSE,axes=FALSE, asp=1, bty="n",
     xlim = c(135, 143), ylim = c(-18.6, -9.6))
points(Locz, pch=20, cex = 1.5, col="red")
text(Latitude ~ Longitude, data = data4, Place, pos = c(4,4,3,2),
     col = "navy")
lines(Oz, col=grey(0.5), xpd = NA)


## ----e2,echo=FALSE-------------------------------------------------------------------------------------------------------
Attach()
require(lattice)
trellis.par.set("grid.pars", list(fontfamily = "sans"))
xyplot(Fsemi ~ DayOfYear|Place, FSemiData4, type = "l",
       ylab=expression(italic(P.)~~italic(semisulcatus)~~plain(proportion)),
       xlab = "Day of the year", aspect = 0.7)


## ----line_665_,echo=TRUE,eval=FALSE--------------------------------------------------------------------------------------
## Attach()
## require(lattice)
## trellis.par.set("grid.pars", list(fontfamily = "sans"))
## xyplot(Fsemi ~ DayOfYear|Place, FSemiData4, type = "l",
##        ylab=expression(italic(P.)~~italic(semisulcatus)~~plain(proportion)),
##        xlab = "Day of the year", aspect = 0.7)


## ----line_671_-----------------------------------------------------------------------------------------------------------
ggplot(FSemiData4) + aes(x = DayOfYear, y = Fsemi) + 
  geom_line(colour = "steelblue") +
  labs(y = expression(italic(P.)~~italic(semisulcatus)~~plain(proportion)),
       x = "Day of year") + facet_wrap(~Place, as.table = FALSE) +
  theme_bw() + theme(strip.background = element_rect(fill = "#EED8AE"))  # "#FFE5CC"


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo(), locale = FALSE)

