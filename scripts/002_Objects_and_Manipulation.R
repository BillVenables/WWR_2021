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
options(xtable.include.rownames = FALSE)


## ----ex1-----------------------------------------------------------------------------------------------------------------
set.seed(12345)
x <- runif(16) %>% round(2)
dim(x) <- c(4,4)
dimnames(x) <- list(rows = letters[1:4], columns = LETTERS[1:4]); x[1:2, ]
attributes(x)
c(length(x), mode(x), class(x))

## ----ex1a,,out.lines=7---------------------------------------------------------------------------------------------------
tx <- as.table(x) %>% as.data.frame(responseName = "values")
tx              ## turn a numeric matrix into a 'long form' data frame
class(tx)
x <- as.vector(x)  ## clear all applied attributes
attributes(x)


## ----ex1b----------------------------------------------------------------------------------------------------------------
f <- factor(fill0(round(10*x)))  ## see note to follow on fill0(...)
table(f)
attributes(f); mode(f)
f+1       ## arithmetic with factors is inhibited!


## ----line_144_-----------------------------------------------------------------------------------------------------------
as.character(f)


## ----ex1c,eval=FALSE-----------------------------------------------------------------------------------------------------
## rbind(A = as.numeric(f),
##       B = as.numeric(as.character(f))) %>%
##   booktabs(digits=0)

## ----line_153_,include=FALSE---------------------------------------------------------------------------------------------
options(xtable.include.rownames = TRUE)


## ----line_158_,results="asis",echo=FALSE---------------------------------------------------------------------------------
rbind(A = as.numeric(f),
      B = as.numeric(as.character(f))) %>%
  booktabs(digits=0)


## ----line_163_,include=FALSE---------------------------------------------------------------------------------------------
options(xtable.include.rownames = FALSE)


## ----line_176_,eval=FALSE------------------------------------------------------------------------------------------------
## ##
## ## Method 1: using string substitution
## fill0 <- function(x) {
##   gsub(" ", "0", format(x, justify = "right"))
## }
## ##
## ## Method 2: using character size computations and paste
## zfill <- function(x) {
##   m <- max(n <- nchar(x <- as.character(x)))
##   paste0(strrep(0, m-n), x)
## }


## ----cx1,fig.show="hold"-------------------------------------------------------------------------------------------------
with(data4, {
  blueish <- alpha("steelblue", 0.5)
  z <- complex(real = Longitude, imaginary = Latitude)
  plot(z, asp = 1, xlim = c(135, 144), ylim = c(-18, -9),
       bty = "n", xlab = "Latitude", ylab = "Longitude")
  lines(Oz, col = blueish)
  text(z, Place, pos = c(2,2,4,4), cex=0.8)
  ij <- utils::combn(4,2)
  i <- ij[1, ]
  j <- ij[2, ]
  segments(z[i], z[j], lty = "dashed", col = alpha("blue", 0.5))
  text((z[i]+z[j])/2, round(gcd_km(z[i], z[j])), col="red", cex=0.8)
})


## ----readin, out.lines = 10----------------------------------------------------------------------------------------------
library(haven)
NPStudy <- read_stata(system.file("extdata", "stata", "NeckPainStudy.dta",
                                  package = "WWRCourse"))
sapply(NPStudy, class)


## ----readin3-------------------------------------------------------------------------------------------------------------
NPStudy <- read_stata(system.file("extdata", "stata", "NeckPainStudy.dta",
                                 package = "WWRCourse")) %>%
  lapply(function(x) if(is.labelled(x)) {
    as_factor(x)
  } else {
    as.vector(x)
  }) %>% untibble()
Store(NPStudy)  ## for safe keeping


## ----readin4-------------------------------------------------------------------------------------------------------------
NeckPain <- NPStudy %>%
    select(-contains("_cat_")) %>% ## remove categoricals
  within({
    Ident <-   paste0("S", fill0(idd))     %>% factor()
    Cluster <- paste0("C", fill0(cluster)) %>% factor()
  }) %>%
  rename(Sex = sex, Group = grp, Organisation = organisation,
         Industry = industry, Ergo = ergo, Age = age,
         BMI = bmi, Education = edu, Occupation = occ,
         Comorbidity = cm) %>%
  select(Ident, Cluster, Group, Organisation:Occupation,
                Comorbidity, pain_num_b:pain_num_9m)


## ----plug----------------------------------------------------------------------------------------------------------------
colSums(is.na(NeckPain))


## ----plug2, cache=FALSE--------------------------------------------------------------------------------------------------
set.seed(20200211)
Extract <- NeckPain %>% select(Group:Comorbidity) %>%  ## only these
  rfImputeUnsupervised() %>%            ## The black box plugger
  within({                              ## do a bit of cleaning-up
    Ergo <- round(Ergo)                 ## make things look innocent
    Age <- round(Age)
    BMI <- round(BMI, 2)
  })
NeckPain[, names(Extract)] <- Extract   ## plug the gaps
rm(Extract)                             ## destroy the evidence...
colSums(is.na(NeckPain))                ## check all is OK


## ----long----------------------------------------------------------------------------------------------------------------
longNeckPain <- NeckPain %>%
  pivot_longer(pain_num_b:pain_num_9m,
               names_to = "Time", values_to = "NPain") %>%
  within({
    time <- Time                  %>% ## currently a mess
      sub("b$", "0", .)           %>% ## final 'b' -> 0
      gsub("[^[:digit:]]", "", .) %>% ## ditch any non-digit
      as.numeric()                    ## coerce to a number
    Time <- ordered(paste0("T", fill0(time)))
    Treat <- ifelse(time == 0, "Base", paste0(substring(Group, 0, 1),
                                              substring(Time, 2)))
  }) %>% select(Ident:Group, Time, time, Treat,
                Organisation:Comorbidity, NPain) %>%
  arrange(Ident, Time) %>%  na.omit() %>% untibble()
Store(NeckPain, longNeckPain)


## ----long2,out.lines=12--------------------------------------------------------------------------------------------------
longNeckPain %>%
  select(Ident, Group:Treat, Ergo:Sex, NPain)


## ----line_390_,include=FALSE---------------------------------------------------------------------------------------------
options(xtable.include.rownames = TRUE)


## ----treat,eval=FALSE----------------------------------------------------------------------------------------------------
## with(longNeckPain,
##      tapply(Treat, list(Group, Time), unique)) %>%
##   booktabs()                            ## for a neat result!

## ----treat__,results="asis",echo=FALSE-----------------------------------------------------------------------------------
with(longNeckPain,
     tapply(Treat, list(Group, Time), unique)) %>%
  booktabs()                            ## for a neat result!


## ----line_401_,include=FALSE---------------------------------------------------------------------------------------------
options(xtable.include.rownames = FALSE)


## ----look,out.lines=9----------------------------------------------------------------------------------------------------
meanNPain <- longNeckPain                                  %>%
  group_by(Group, Time, Treat, Sex)                        %>%
  summarise(N = n(), Pain = mean(NPain), .groups = "drop") %>%
  ungroup()                                                %>%
  untibble()                                               %>%
  arrange(Treat, Sex)
meanNPain


## ----look2,out.height="0.5\\textheight"----------------------------------------------------------------------------------
sex_cols <- c(male = "steelblue", female = "rosybrown")
ggplot(meanNPain)                                    +
  aes(x = Time, y = Pain, colour = Sex, group = Sex) +
  geom_point(aes(size = N))                          +
  geom_line()                                        +
  facet_wrap(~Group, ncol=2)                         +
  theme(legend.position = "bottom")                  +
  ylim(0, 2)                                         +
  scale_colour_manual(values = sex_cols)


## ----look2a,out.height="0.6\\textheight",fig.height=6,fig.width=9--------------------------------------------------------
ggplot(meanNPain) + aes(x = Time, y = Pain, colour = Sex,
                        linetype = Group, group = Group) +
  geom_point(aes(size = N)) + geom_line() + facet_wrap(~Sex, ncol=2) +
  theme(legend.position = "bottom") + ylim(0, 2) +
  scale_colour_manual(values = sex_cols)


## ----look3,eval=FALSE----------------------------------------------------------------------------------------------------
## meanNPain                       %>%
##   pivot_wider(id_cols = c(Group, Sex),  ## These columns identify rows
##               names_from = Time,
##               values_from = N) %>% booktabs

## ----look3__,echo=FALSE,results="asis"-----------------------------------------------------------------------------------
meanNPain                       %>%
  pivot_wider(id_cols = c(Group, Sex),  ## These columns identify rows
              names_from = Time, 
              values_from = N) %>% booktabs


## ----look4,out.height="0.7\\textheight"----------------------------------------------------------------------------------
mNPain <- meanNPain %>% within({
  Time <- factor(as.character(Time), levels = rev(levels(Time)))
})
ggplot(mNPain) + aes(x = Time, y = N,  fill = Sex) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip() +
  facet_wrap(~ Group) + theme(legend.position = "bottom") +
  scale_fill_manual(values = sex_cols)


## ----look5,eval=FALSE----------------------------------------------------------------------------------------------------
## percentNPain <- longNeckPain                          %>%
##   group_by(Group, Time, Treat, Sex)                   %>%
##   summarise(N = n(), Severe = mean(NPain >= 3) * 100,
##             .groups = "drop")                         %>%
##   ungroup()                                           %>%
##   arrange(Treat, Sex)                                 %>%
##   untibble()
## percentNPain                                          %>%
##   pivot_wider(id_cols = Group:Treat, names_from = Sex,
##               values_from = Severe)                   %>% booktabs()

## ----look5a,out.height="80%",results="asis",echo=FALSE-------------------------------------------------------------------
percentNPain <- longNeckPain                          %>%
  group_by(Group, Time, Treat, Sex)                   %>%
  summarise(N = n(), Severe = mean(NPain >= 3) * 100,
            .groups = "drop")                         %>%
  ungroup()                                           %>%
  arrange(Treat, Sex)                                 %>%
  untibble()
percentNPain                                          %>%
  pivot_wider(id_cols = Group:Treat, names_from = Sex, 
              values_from = Severe)                   %>% booktabs()


## ----look6,out.height="0.6\\textheight",fig.height=6,fig.width=9---------------------------------------------------------
ggplot(percentNPain) + aes(x = Time, y = Severe, colour = Sex, group = Sex) +
  geom_point(aes(size = N)) + geom_line() + facet_wrap(~Group, ncol=2) +
  theme(legend.position="bottom") + ylab("% score 3 or higher") +
  ylim(0, 40) + scale_colour_manual(values = c(male = "steelblue",
                                               female = "rosybrown"))


## ----look7,out.height="0.6\\textheight",fig.height=6,fig.width=9---------------------------------------------------------
ggplot(percentNPain) + aes(x = Time, y = Severe, colour = Sex,
                           linetype = Group, group = Group) +
  geom_point(aes(size = N)) + geom_line() + facet_wrap(~Sex, ncol=2) +
  theme(legend.position="bottom") + ylab("% score 3 or higher") +
  ylim(0, 40) + scale_colour_manual(values = sex_cols)


## ----dutch1, out.lines = 10----------------------------------------------------------------------------------------------
dutchSpeakers <- talkers %>% within({
  id <- paste0("S", zfill(id))   ## function in WWRSoftware
  sex <- recode_factor(sex, `0` = "Female", `1` = "Male")
  AgeGroup <- factor(ifelse(age > 42, "45+", "40-"),
                     levels = c("40-", "45+"))
  region <- factor(as.character(region),
                   levels = c("N",     "M",      "W",    "S"),
                   labels = c("North", "Middle", "West", "South"))
}) %>%
  rename(Ident = id, Region = region, Sex = sex, ## New = old
         Drawl = syldur, Verbose = nsyl) %>%
  arrange(Region, Sex, age)


## ----dutch4, out.lines=13------------------------------------------------------------------------------------------------
dutchSpeakers  ### look at it


## ----dutch4a,out.width='1.0\\linewidth',fig.height=7,fig.width=10--------------------------------------------------------
ggplot(dutchSpeakers) + aes(x = age, y = Drawl, colour = Sex) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ (x < 43)) +
  theme(legend.position = "top") + 
  geom_vline(xintercept = c(41, 45), linetype = "longdash", 
             colour = "dark green") + xlab("\nAge of respondent") +
  facet_wrap( ~ Region) +
  scale_colour_viridis_d()


## ----dutch5, out.width='0.8\\textwidth', fig.height = 4, fig.width = 8---------------------------------------------------
p0 <- ggplot(dutchSpeakers) + aes(Verbose, Drawl) + 
  geom_point(colour="brown") + 
  geom_smooth(method="lm",formula=y~x, size=0.7) 
p0 + facet_grid(Sex ~ Region)


## ----dutch6, out.width='0.8\\textwidth', fig.width = 8, fig.height = 6---------------------------------------------------
ggplot(dutchSpeakers) + aes(x = Verbose, y = Drawl) + 
    geom_point(colour = "brown", size = 1) + 
    geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, size=0.5,
                formula = y~x) + 
    facet_grid(AgeGroup + Sex ~ Region)


## ----dutch7, eval=FALSE--------------------------------------------------------------------------------------------------
## dsModel <- aov(Drawl ~ Verbose+Sex+AgeGroup+Region, dutchSpeakers)
## anova(dsModel) %>%
##   booktabs(digits = c(0,0,4,4,2,4)) ## for display only.  Cut these bits

## ----dutch7__, out.lines = Inf, results = "asis", echo=FALSE-------------------------------------------------------------
dsModel <- aov(Drawl ~ Verbose+Sex+AgeGroup+Region, dutchSpeakers)
anova(dsModel) %>% 
  booktabs(digits = c(0,0,4,4,2,4)) ## for display only.  Cut these bits


## ----dutch8, out.lines = Inf---------------------------------------------------------------------------------------------
dropterm(dsModel)  ## WWRUtilities enhanced version


## ----dutch7a, out.lines = Inf--------------------------------------------------------------------------------------------
round(summary.lm(dsModel)$coeff, 4) ## check on signs and sizes

#### manual residual analysis
diagDutchSpeakers <- data.frame(rs = scale(resid(dsModel)),
                                fv = fitted(dsModel))


## ----dutch8a,eval=FALSE--------------------------------------------------------------------------------------------------
## layout(rbind(1:2, 3:4))  ## 2 x 2 array of plots, filled by rows
## plot(dsModel)


## ----echo=FALSE,out.width = '100%'---------------------------------------------------------------------------------------
par(bg = "#F4FCFE")
layout(rbind(1:2, 3:4))  ## 2 x 2 array of plots, filled by rows
plot(dsModel)


## ----ducth9,out.width='1.0\\textwidth', fig.width=10, fig.height=5-------------------------------------------------------
layout(rbind(1:2))
par(bg = "#F4FCFE", family="mono")  ## mundane, old-fashioned look...
plot(rs ~ fv, diagDutchSpeakers,  col = "steel blue",
     xlab = "fitted values", ylab = "scaled residuals", 
     main = "Residuals vs Fitted Values") 
abline(h = 0, col = "red", lty = "dashed")
with(diagDutchSpeakers, {
  qqnorm(rs, xlab = "normal scores", col = "steel blue",
         ylab = "sorted scaled residuals",
         main = "Normal Scores Plot")
  qqline(rs, col = "red", lty = "dashed")
})


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo(), locale = FALSE)

