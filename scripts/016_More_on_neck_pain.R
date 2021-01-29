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


## ----libs,include=FALSE--------------------------------------------------------------------------------------------------
library(WWRCourse)
library(haven)


## ----makeData,include=FALSE,cache=TRUE-----------------------------------------------------------------------------------

###########################################################################
###########################################################################
###                                                                     ###
###           THIS SECTION RE-MAKES THE NECK PAIN DATA SETS,            ###
###                INCLUDING PLUGGING THE MISSING VALUES                ###
###                                                                     ###
###########################################################################
###########################################################################

##---------------------------------------------------------------
##                        A useful helper                       -
##---------------------------------------------------------------

zfill <- function(x) {  ## handy for making uniform stringwidth.
  m <- max(n <- nchar(x <- as.character(x)))
  paste0(strrep(0, m-n), x)
}

#################################################################
##           Read in data and do basic manipulations           ##
#################################################################

NeckPain <- read_stata(system.file("extdata", "stata", "NeckPainStudy.dta",
                                   package = "WWRCourse")) %>%
  lapply(function(x) if(is.labelled(x)) {
    as_factor(x)
  } else {
    as.vector(x)
  }) %>% data.frame %>% 
  select(-contains("_cat_")) %>% ## remove categoricals
  within({
    Ident <-   paste0("S", zfill(idd))     %>% factor()
    Cluster <- paste0("C", zfill(cluster)) %>% factor()
  }) %>% 
  rename(Sex = sex, Group = grp, Organisation = organisation,
         Industry = industry, Ergo = ergo, Age = age,
         BMI = bmi, Education = edu, Occupation = occ,
         Comorbidity = cm) %>% 
  select(Ident, Cluster, Group, Organisation:Occupation,
         Comorbidity, pain_num_b:pain_num_9m)


#################################################################
##                     Plug missing values                     ##
#################################################################

set.seed(20190205)
Extract <- NeckPain %>% 
  select(Group:Comorbidity) %>%  ## only these...
  rfImputeUnsupervised() %>%     ## The black box plugger
  within({                       ## do a bit of cleaning-up
    Ergo <- round(Ergo)
    Age <- round(Age)
    BMI <- round(BMI, 2)
  })
NeckPain[, names(Extract)] <- Extract   ## plut the gaps 
rm(Extract)                             ## destroy the evidence...

#################################################################
##     Make a long form version of the data and make Treat     ##
#################################################################

longNeckPain <- NeckPain %>% 
  gather(key = Time, value = NPain, pain_num_b:pain_num_9m) %>% 
  within({
    time <- Time                  %>% ## currently a mess
      sub("b$", "0", .)           %>% ## final 'b' -> 0
      gsub("[^[:digit:]]", "", .) %>% ## ditch any non-digit
      as.numeric()                    ## coerce to a number
    Time <- ordered(paste0("T", zfill(time)))
    Treat <- ifelse(time == 0, "Base", paste0(substring(Group, 0, 1),
                                              substring(Time, 2))) 
  }) %>% select(Ident:Group, Time, time, Treat,
                Organisation:Comorbidity, NPain) %>% 
  arrange(Ident, Time) %>% 
  na.omit

#################################################################
##                     End of data section                     ##
#################################################################


## ----miss----------------------------------------------------------------------------------------------------------------
colSums(is.na(longNeckPain))


## ----mod1,results="asis"-------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(lme4))
mod_0 <- lmer(NPain ~ Sex*(Time*Group + Age + 
                             BMI + Comorbidity + Ergo + Industry) +
                (1|Ident), data = longNeckPain)
dropterm(mod_0) %>% 
  booktabs(digits=c(0, 0, 2, 4, 4))


## ----mod2,results="asis",cache=TRUE,xx=0---------------------------------------------------------------------------------
mod_0s <- step_down(mod_0)
dropterm(mod_0s) %>% 
  booktabs(digits=c(0, 0, 2, 4, 4))


## ----mod3----------------------------------------------------------------------------------------------------------------
pNeckPain <- with(longNeckPain, 
                  expand.grid(Time = levels(Time),
                              Group = levels(Group),
                              Sex = levels(Sex),
                              Comorbidity = c(0, 2),
                              BMI = quantile(BMI, c(0.25, 0.75)) %>% round))
pNeckPain$NPain <- predict(mod_0s, pNeckPain, re.form = ~0)
ggplot(pNeckPain) + aes(x = Time, y = NPain, group = Group,
                        linetype = Group, colour = Sex) + 
  geom_point() + geom_line() + facet_grid(Sex ~ BMI + Comorbidity) +
  theme(legend.position = "bottom") + ylab("Mean neck pain score") +
  scale_colour_manual(values = c(male = "steelblue", 
                                 female = "rosybrown"))


## ----mod4----------------------------------------------------------------------------------------------------------------
mod_1s <- update(mod_0s, . ~ . - Time - Group + Treat) %>% step_down
pNeckPain <- pNeckPain %>% 
  within({
    Treat <- ifelse(Time == "T00", "Base", 
                    paste0(substring(Group, 0, 1),
                           substring(Time, 2)))
  })
pNeckPain$NPainT <- predict(mod_1s, pNeckPain, re.form = ~0)
ggplot(pNeckPain) + aes(x = Time, y = NPainT, group = Group,
                        linetype = Group, colour = Sex) + 
  geom_point() + geom_line() + facet_grid(Sex ~ BMI + Comorbidity) +
  theme(legend.position = "bottom")  + ylab("Mean neck pain score") +
  scale_colour_manual(values = c(male = "steelblue", 
                                 female = "rosybrown"))


## ----mod5,results="asis"-------------------------------------------------------------------------------------------------
dropterm(mod_1s) %>% 
  booktabs(digits=c(0, 0, 2, 4, 4))


## ----bin1,results="asis"-------------------------------------------------------------------------------------------------
longNeckPain <- within(longNeckPain, {
  Severe <- (NPain >= 3) + 0 ## coerce logical to binary
})
bin_0 <- glmer(Severe ~ Sex*(Time*Group + Age + BMI + Comorbidity +
                             Ergo + Industry) + (1|Ident), 
               family = binomial, data = longNeckPain, nAGQ = 0)
dropterm(bin_0) %>% 
  booktabs(digits=c(0, 0, 2, 4, 4))


## ----bin2,results="asis",cache=TRUE,xx=0---------------------------------------------------------------------------------
bin_0s <- step_down(bin_0)
dropterm(bin_0s) %>% 
  booktabs(digits=c(0, 0, 2, 4, 4))


## ----bin3----------------------------------------------------------------------------------------------------------------
pNeckPain$pSevere <- predict(bin_0s, pNeckPain,
                             type = "response", re.form = ~0)
ggplot(pNeckPain) + aes(x = Time, y = pSevere, group = Group,
                        linetype = Group, colour = Sex) + 
  geom_point() + geom_line() + facet_grid(Sex ~ BMI + Comorbidity) +
  theme(legend.position = "bottom")  + ylab("Pr(severe neck pain)") +
  scale_colour_manual(values = c(male = "steelblue", 
                                 female = "rosybrown"))


## ----bin4,results="asis"-------------------------------------------------------------------------------------------------
bin_1s <- glmer(Severe ~ Sex*(Treat + Comorbidity) + (1|Ident),
                family = binomial, data = longNeckPain, nAGQ = 0) %>%
  step_down()
dropterm(bin_1s, test = "Chisq", sorted = TRUE) %>% 
  booktabs(digits=c(0, 0, 2, 4, 4))
pNeckPain$pSevereT <- predict(bin_1s, pNeckPain,
                              type = "response", re.form = ~0)
ggplot(pNeckPain) + aes(x = Time, y = pSevereT, group = Group,
                        linetype = Group, colour = Sex) + ylim(0, 0.3) +
  geom_point() + geom_line() + facet_grid(Sex ~ Comorbidity) +  ## BMI is gone!
  theme(legend.position = "bottom")  + ylab("Pr(severe neck pain)") +
  scale_colour_manual(values = c(male = "steelblue", female = "rosybrown"))


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo(), locale = FALSE)

