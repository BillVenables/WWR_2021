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


## ----line_074_,eval=FALSE------------------------------------------------------------------------------------------------
## ?petrol
## petrol  ## print the whole data!


## ----line_079_,out.lines=10,echo=FALSE-----------------------------------------------------------------------------------
petrol


## ----c0a,eval=FALSE------------------------------------------------------------------------------------------------------
## ggplot(petrol) + aes(x = EP, y=Y) +
##   geom_point(colour = "red") +
##   stat_smooth(method = "lm", se = FALSE, fullrange = TRUE,
##               size=0.5, colour = "blue",formula=y~x) + facet_wrap( ~ No) +
##   labs(title = "Petrol refining data of N. L. Prater",
##        x = "Refining end poing (EP)",
##        y = "Petroleum yield as a % of crude, (Y)")


## ----c0b-----------------------------------------------------------------------------------------------------------------
petrol <- petrol %>%
  within(EPc <- scale(EP))  ### for convenience
Store(petrol)


## ----c0show,echo=FALSE,fig.height=7,fig.width=9--------------------------------------------------------------------------
ggplot(petrol) + aes(x = EP, y=Y) +
  geom_point(colour = "red") +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE,
              size=0.5, colour = "blue",formula=y~x) + facet_wrap( ~ No) +
  labs(title = "Petrol refining data of N. L. Prater",
       x = "Refining end poing (EP)",
       y = "Petroleum yield as a % of crude, (Y)")


## ----c1------------------------------------------------------------------------------------------------------------------
options(show.signif.stars = FALSE)
m3 <- lm(Y ~ 0 + No/EPc, petrol)            ## 10 ints + 10 slopes
m2 <- lm(Y ~ 0 + No+EPc, petrol)            ## 10 ints + 1 slope
m1 <- lm(Y ~ 1 + SG+VP+V10+EPc, petrol)     ## (1 int + 3 coeffs) + 1 slope
anova(m1, m2, m3)


## ----line_135_,echo=FALSE------------------------------------------------------------------------------------------------
requireData(SOAR)
Attach()


## ----c2------------------------------------------------------------------------------------------------------------------
requireData(lme4)     ## alt. nlme
Rm1 <- lmer(Y ~ 1 + SG+VP+V10 + EPc + (1|No),     data = petrol)
Rm2 <- lmer(Y ~ 1 + SG+VP+V10 + EPc + (1+EPc|No), data = petrol)
anova(Rm1, Rm2)       ## automatic re-fitting


## ----c3------------------------------------------------------------------------------------------------------------------
print(summary(Rm1), correlation = FALSE)


## ----c3a-----------------------------------------------------------------------------------------------------------------
print(summary(Rm2), correlation = FALSE)


## ----c4------------------------------------------------------------------------------------------------------------------
cbind(Rm1 = ranef(Rm1)$No, Rm2 = ranef(Rm2)$No)


## ----c5------------------------------------------------------------------------------------------------------------------
VarCorr(Rm2)          ## an unhelpful print method.
names(VarCorr(Rm2)) %>% noquote()
VarCorr(Rm2)[["No"]]  ## The pieces are all accessible


## ----c6------------------------------------------------------------------------------------------------------------------
dim(Headrope)
head(Headrope, 2)
Headrope <- Headrope %>% within(YearF <- factor(YearF)) ## needed
Store(Headrope)


## ----c7,cache=FALSE------------------------------------------------------------------------------------------------------
HRmodel1 <- lmer(log(Catch) ~ 0 + log(Days) + Y2K + log(Head) +
                 log(Power) + log(Hull) + Stock +
                 (1|Vessel) + (1|YearF/Stock), data = Headrope,
                 control = lmerControl(optimizer = "bobyqa"))
HRmodel1_ML <- update(HRmodel1, REML = FALSE)
Store(HRmodel1, HRmodel1_ML, lib = .Robjects)


## ----c8,cache=TRUE-------------------------------------------------------------------------------------------------------
HRmodel2 <- lmer(log(Catch) ~ 0 + log(Days) + Y2K + log(Head) +
                 log(Power) + log(Hull) + Stock +
                 (1|Vessel) + (0+Stock|YearF), data = Headrope,
                 control = lmerControl(optimizer = "bobyqa"))
HRmodel2_ML <- update(HRmodel2, REML = FALSE)
Store(HRmodel2, HRmodel2_ML, lib = .Robjects)

## ----include=FALSE-------------------------------------------------------------------------------------------------------
  if("package:MASS" %in% search()) detach("package:MASS")


## ----c10-----------------------------------------------------------------------------------------------------------------
anova(HRmodel1_ML, HRmodel2_ML)


## ----c9------------------------------------------------------------------------------------------------------------------
cbind(m1 = fixef(HRmodel1), m2 = fixef(HRmodel2))


## ----c10a----------------------------------------------------------------------------------------------------------------
print(summary(HRmodel2), correlation = FALSE)


## ----help,echo=FALSE-----------------------------------------------------------------------------------------------------
Harm <- function (theta, k = 4) {
  X <- matrix(0, length(theta), 2 * k)
  nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
  dimnames(X) <- list(names(theta), nam)
  m <- 0
  for (j in 1:k) {
    X[, (m <- m + 1)] <- cos(j * theta)
    X[, (m <- m + 1)] <- sin(j * theta)
  }
  X
}
Hyear <- function(x, k = 4)
    Harm(2*base::pi*x/365.25, k)
twoWay <- local({
  `%star%` <- function(X, Y) { ## all column-products
    X <- as.matrix(X)
    Y <- as.matrix(Y)
    stopifnot(is.numeric(X), is.numeric(Y),
              nrow(X) == nrow(Y))
    XY <- matrix(NA, nrow(X), ncol(X)*ncol(Y))
    k <- 0
    for(i in 1:ncol(X))
        for(j in 1:ncol(Y)) {
          k <- k+1
          XY[, k] <- X[, i] * Y[, j]
        }
    XY
  }
  function(day, sea, k = c(3,2)) {
    Hyear(day, k[1]) %star% splines::ns(sea, k[2])
  }
})
Store(Harm, Hyear, twoWay, lib = .Robjects)


## ----g1,cache=TRUE-------------------------------------------------------------------------------------------------------
TModelGLMM <- glmmPQL(Psem/Total ~ ns(Coast, 6) + ns(Sea, 5) +
                      twoWay(DayOfYear, Sea) + ns(Depth, k=5) +
                      Hyear(DayOfYear, 2) + ns(Mud, k=5),
                      random = ~1|Survey,
                      family = quasibinomial, data= Tigers,
                      niter = 40, weights = Total)
Store(TModelGLMM, lib = .Robjects)


## ----include=FALSE-------------------------------------------------------------------------------------------------------
  if("package:MASS" %in% search()) detach("package:MASS")


## ----g2,cache=TRUE-------------------------------------------------------------------------------------------------------
requireData(mgcv)
TModelGAMM <- gamm(formula = Psem/Total ~ s(Coast, k=5) + s(Sea,k=5) +
                   twoWay(DayOfYear, Sea) +
                   s(DayOfYear, k=5, bs="cc") + s(Depth,k=5) +
                   s(Mud, k=5),
                   knots = list(DayOfYear = seq(0, 364.25, length = 5)),
                   random = list(Survey = ~1),
                   family = quasibinomial, data = Tigers,
                   niterPQL = 40,
                   weights = Total)
Store(TModelGAMM, lib = .Robjects)


## ----include=FALSE-------------------------------------------------------------------------------------------------------
  if("package:MASS" %in% search()) detach("package:MASS")


## ----g3------------------------------------------------------------------------------------------------------------------
re1 <- setNames(ranef(TModelGLMM), "GLMM")
re2 <- setNames(ranef(TModelGAMM$lme)$Survey, "GAMM")  ## obscure
(re12 <- cbind(re1, re2))


## ----g4------------------------------------------------------------------------------------------------------------------
layout(rbind(1:2), widths = c(3.5,0.5), heights = c(3.5, 3.5),
       respect = TRUE)
with(re12, {
  nos <- seq_along(GLMM)
  lim <- range(GLMM, GAMM)
  plot(GLMM ~ GAMM, pch = 20, col="red", bty="n",
       xlim = lim, ylim = lim, asp = 1)
  abline(0, 1, col = "grey", lty = "dashed")
  box(col = "grey")
  grid()
  pos <- avoid(GAMM, GLMM) ## minimise clashes
  text(GLMM ~ GAMM, labels = nos, xpd = NA, pos=pos)
  par(mar = c(0,0,0,0), xpd = NA)
  frame()
  legend("center", paste(format(nos), rownames(re12),
                         sep = ": "), bty="n")
})


## ----g4ggplot------------------------------------------------------------------------------------------------------------
re12 <- re12 %>% within({
  Survey <- factor(rownames(re1)) %>% reorder(-(GLMM+GAMM), I)
})
ggplot(re12) + aes(x = GAMM, y = GLMM) +
    coord_equal() + xlim(-1.5, 1.2) + ylim(-1.5, 1.2) +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed", colour = "grey") +
    geom_point(colour = "red")  +
    ggrepel::geom_label_repel(aes(label = Survey),
                              fill = alpha("sky blue", 0.5)) +
    labs(x = "Generalises Additive Mixed Model",
         y = "Generlaised Linear Mixed Model") +
    theme_bw() +
    theme(text = element_text(family = "sans"))


## ----q1------------------------------------------------------------------------------------------------------------------
head(quine, 2)
q0 <- glm(Days ~ Age*Sex*Eth*Lrn, poisson, quine)
c(deviance = deviance(q0), "residual d.f." = df.residual(q0))


## ----q2------------------------------------------------------------------------------------------------------------------
quine %>%
  group_by(Age, Sex, Eth, Lrn) %>%
  summarise(n = n(), mu = mean(Days), sigma = sd(Days), .groups = 'drop') %>%
  na.omit %>%
  unclass %>%
  data.frame -> quine_sum
head(quine_sum, 4)
ggplot(quine_sum) + aes(x = mu, y = sigma, size = n) +
  geom_point(colour = "steel blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "red") +
  theme(legend.position = "none")


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
if("package:MASS" %in% search()) detach("package:MASS")


## ----q3,results="asis"---------------------------------------------------------------------------------------------------
quine_full <- glm.nb(Days ~ Age*Sex*Eth*Lrn, quine)
quine_step <- step_BIC(quine_full) ## BIC penalty
dropterm(quine_step) %>%  booktabs(digits = c(0,0,2,2,6))


## ----glad1---------------------------------------------------------------------------------------------------------------
Bream <- subset(GladstoneBream, select = c(Trip, Site:Rock, Casts, Bream))
names(Bream) %>% noquote


## ----glad2---------------------------------------------------------------------------------------------------------------
NB_Bream0 <- glmer.nb(Bream ~ offset(log(Casts)) + Depth + Rock + Month +
                        (1|Site) + (1|Year/Site), data = Bream, nAGQ = 0)
getME(NB_Bream0, "glmer.nb.theta")


## ----glad3---------------------------------------------------------------------------------------------------------------
NB_Bream <- glmer(Bream ~ offset(log(Casts)) + Depth + Rock + Month + (1|Site) +
                    (1|Year/Site), family = negative.binomial(theta = 2),
                  mustart = fitted(NB_Bream0), data = Bream, nAGQ = 1)


## ----glad4---------------------------------------------------------------------------------------------------------------
(v <- VarCorr(NB_Bream) %>% unlist ) ## these are variances!
sigS <- sqrt(v[["Site"]])
sigYS <- sqrt(v[["Year"]] + v[["Site:Year"]])
c(Site = sigS, Year_x_Site = sigYS)


## ----glad5---------------------------------------------------------------------------------------------------------------
(RE <- ranef(NB_Bream)) %>% names


## ----glad6---------------------------------------------------------------------------------------------------------------
x <- RE[["Site"]]
(S <- data.frame(Site = factor(rownames(x), levels = rownames(x)),
                 Score = pnorm(x[["(Intercept)"]], sd = sigS)))  %>% head(2)
x <- RE[["Year"]]
(Y <- data.frame(Year = factor(rownames(x), levels = rownames(x)),
                 Y_BLUP = x[["(Intercept)"]]))                   %>% head(2)
(x <- RE[["Site:Year"]])                                         %>% head(3)


## ----glad7,results="hide"------------------------------------------------------------------------------------------------
YS <- data.frame(nam = rownames(x), stringsAsFactors = FALSE) %>%
  separate(nam, c("Site", "Year"), sep = ":") %>%
  within({
    Site <- factor(Site, levels = levels(S$Site))
    Year <- factor(Year, levels = levels(Y$Year))
    YS_BLUP <- x[["(Intercept)"]]
  }) %>%
  left_join(Y, by = "Year") %>%
  within({
    YS <- pnorm(Y_BLUP + YS_BLUP, sd = sigYS)
    Y_BLUP <- YS_BLUP <- NULL
  }) %>% arrange(Year) %>% 
  pivot_wider(names_from = Year, values_from = YS) %>% 
  arrange(Site)
booktabs(YS)


## ----line_630_,results="asis",echo=FALSE---------------------------------------------------------------------------------
booktabs(YS)


## ----glad8,eval=FALSE----------------------------------------------------------------------------------------------------
## Scores <- merge(S, YS, by = "Site") %>%
##   within({
##     Site <- factor(as.character(Site), levels = levels(Bream$Site))
##   }) %>% arrange(Site)
## Scores


## ----glad8a,results="asis",echo=FALSE------------------------------------------------------------------------------------
Scores <- merge(S, YS, by = "Site") %>%
  within({
    Site <- factor(as.character(Site), levels = levels(Bream$Site))
  }) %>% arrange(Site)
Scores %>% booktabs(align = c("l", "l", rep("c", 6), "c@{}")) %>% print(include.rownames = FALSE)


## ----themes--------------------------------------------------------------------------------------------------------------
apropos("^(theme$|theme_)") %>% noquote



## ----g4a,eval=FALSE------------------------------------------------------------------------------------------------------
## Harm <- function (theta, k = 4) {
##   X <- matrix(0, length(theta), 2 * k)
##   nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
##   dimnames(X) <- list(names(theta), nam)
##   m <- 0
##   for (j in 1:k) {
##     X[, (m <- m + 1)] <- cos(j * theta)
##     X[, (m <- m + 1)] <- sin(j * theta)
##   }
##   X
## }
## Hyear <- function(x, k = 4)
##     Harm(2*base::pi*x/365.25, k)
## twoWay <- local({
##   `%star%` <- function(X, Y) { ## all column-products
##     X <- as.matrix(X)
##     Y <- as.matrix(Y)
##     stopifnot(is.numeric(X), is.numeric(Y),
##               nrow(X) == nrow(Y))
##     XY <- matrix(NA, nrow(X), ncol(X)*ncol(Y))
##     k <- 0
##     for(i in 1:ncol(X))
##         for(j in 1:ncol(Y)) {
##           k <- k+1
##           XY[, k] <- X[, i] * Y[, j]
##         }
##     XY
##   }
##   function(day, sea, k = c(3,2)) {
##     Hyear(day, k[1]) %star% splines::ns(sea, k[2])
##   }
## })
## Store(Harm, Hyear, twoWay, lib = .Robjects)


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo(), locale = FALSE)

