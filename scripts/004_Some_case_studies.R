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


## ------------------------------------------------------------------------------------------------------------------------
find("DarwinsFinches")
str(DarwinsFinches)


## ------------------------------------------------------------------------------------------------------------------------
with(DarwinsFinches, levels(Species))


## ------------------------------------------------------------------------------------------------------------------------
## choose species colours
spCols <- c(`Geospiza fortis fortis` =             "#006400",  ## "darkgreen"
            `Geospiza fortis platyrhyncha` =       "#9BCD9B",  ## "darkseagreen3"
            `Geospiza heliobates`  =               "#B22222",  ## "firebrick"
            `Geospiza fuliginosa parvula` =        "#8B4513",  ## "chocolate4"
            `Geospiza prosthemelas prosthemelas` = "#F4A460")  ## "sandybrown"
sub_text <-  ## for a citation line
  "(Darwins Finch data, Hopkins-Stanford Galapagos Expedition, 1898-99)"


## ------------------------------------------------------------------------------------------------------------------------
Species <- with(DarwinsFinches, Species) ## Species factor
n <- length(Species)                     ## sample size (= 146)
species <- levels(Species)               ## species names
p <- length(species)                     ## number of species (= 5)

Y <- as.matrix(select(DarwinsFinches, BodyL:TarsusL)) ## responses

M0 <- apply(Y, 2, ave)                   ## grand mean
M1 <- apply(Y, 2, ave, Species)          ## species means

B <- crossprod(M1 - M0)/(p - 1)          ## between species MSq
W <- crossprod(Y  - M1)/(n - p)          ## withing species MSq


## ------------------------------------------------------------------------------------------------------------------------
ev <- eigen2(B, W)                            ## critical computation
print(with(ev, zapsmall(values)), digits = 3) ## "F-statistics" should be (p-1)

alpha <- with(ev, vectors[, 1:(p-1)])                 ## coefficients
Scores <- (Y - M0) %*% alpha                          ## discriminant functions
Finch <- cbind(DarwinsFinches, data.frame(Scores))    ## augmented data
noquote(setdiff(names(Finch), names(DarwinsFinches))) ## extra names


## ------------------------------------------------------------------------------------------------------------------------
with(Finch, plot(X1, X2, xlab="First DF", ylab="Second DF", las=1,
                 col=spCols[Species], pch=16, cex=0.7,
                 panel.first=grid(lty="dashed", lwd=0.5, nx=7)))
by(Finch, Species, FUN=function(dat) with(dat, {
  spColour <- spCols[Species[1]]                ## get species col
  z <- complex(real=X1, imaginary=X2)           ## complex trick!
  points(mean(z), pch=3, cex=1.5, col=spColour) ## centroid
  polygon(z[chull(z)], col="transparent",
          border=spColour)                      ## convex hulls
})) %>% invisible()                             ## toss dummy output
mtext(sub_text, side=1, line=4, adj=1, cex=0.75, family="serif")
par(family="serif")                           ## outside legene()
legend("topleft", species, pch=16, lty="solid", col=spCols, cex=0.8,
       title="Species", text.font=3, title.adj=0.025, bty="n",
       inset=c(0.0125, 0.0125))


## ------------------------------------------------------------------------------------------------------------------------
finch_centroids <- Finch %>%          ## for the central points
  group_by(Species) %>%
  summarise(X1 = mean(X1), X2 = mean(X2)) %>%
  ungroup()

finch_hulls <- Finch %>%              ## this is the tricky one
  group_by(Species) %>%
  do(., with(., {                     ## the complex trick does
    h <- chull(cbind(X1, X2))         ## not work with ggplot() 
    data.frame(Species = Species[h], X1 = X1[h], X2 = X2[h])
  })) %>%
  ungroup()


## ------------------------------------------------------------------------------------------------------------------------
separate_species <- split(Finch, Species)  ## a list of 5 data frames
finch_centroids_1 <- separate_species %>% 
  lapply(function(dat) with(dat, 
                            data.frame(Species = Species[1], 
                                       X1 = mean(X1), 
                                       X2 = mean(X2)))) %>%
  do.call(rbind, .)
finch_hulls_1 <- separate_species %>% 
  lapply(function(dat) with(dat, {
    h <- chull(cbind(X1, X2))
    data.frame(Species = Species[h],
               X1 = X1[h],
               X2 = X2[h])
  })) %>% do.call(rbind, .)


## ------------------------------------------------------------------------------------------------------------------------
plt <- ggplot() + 
  aes(x = X1, y = X2, color = Species) +  ## inherited to all
  geom_point(data = Finch) +
  geom_point(data = finch_centroids, shape = 3, size = 3) +
  geom_polygon(data = finch_hulls, fill = "transparent") +
  scale_color_manual(values = spCols) +
  labs(caption = sub_text, x = "First DF", y = "Second DF") +
  theme_bw() +
  theme(legend.position = c(0.19, 0.85),
        legend.text = element_text(family = "serif",
                                   face = "italic", size = 11))
plt


## ----line_066_-----------------------------------------------------------------------------------------------------------
dir(system.file("extdata", package = "WWRCourse"))


## ----line_073_,out.lines=6-----------------------------------------------------------------------------------------------
fname <- system.file("extdata", "churnData.csv.gz", package = "WWRCourse")
churnData <- read_csv(gzfile(fname))     ## initial read


## ----line_080_-----------------------------------------------------------------------------------------------------------
churnData <- read_csv(gzfile(fname),     ## neater read (for eventual notebook)
                      col_types = cols(.default = col_double(),
                                       state = col_character(),
                                       area_code = col_character(),
                                       international_plan = col_character(),
                                       voice_mail_plan = col_character(),
                                       churn = col_character(),
                                       sample = col_character()))
names(churnData) %>% noquote()


## ----line_102_-----------------------------------------------------------------------------------------------------------
dim(churnData)
with(churnData, table(churn, sample))


## ----line_114_-----------------------------------------------------------------------------------------------------------
churnData <- data.frame(unclass(churnData), stringsAsFactors = TRUE)


## ----line_119_,eval=FALSE------------------------------------------------------------------------------------------------
## churnData <- churnData %>%                ## start with the data...
##   unclass(.)           %>%                ## then remove the class()...
##   data.frame(., stringsAsFactors = TRUE)  ## then make it a data.frame()


## ----line_133_-----------------------------------------------------------------------------------------------------------
churnData <- churnData %>% 
  within({
    area_code <- sub("^area_code_", "", area_code) %>% 
      factor()
  })
with(churnData, table(area_code))


## ----line_145_,fig.keep="last"-------------------------------------------------------------------------------------------
names(churnData) <- sub("^(total|number)_", "", names(churnData)) ## neater
tots <- churnData %>% select(ends_with("_minutes"),
                             ends_with("_charge"),
                             ends_with("_calls"))
pairs(tots, pch = ".")         ## old technology! (result not shown)
######
## cut down the number of panels and group variables for effect
######
tots <- tots %>% 
  select(starts_with("day"),   starts_with("eve"),
         starts_with("night"), starts_with("intl"),
         -ends_with("_calls"))              ## remove the calls
pairs(tots, pch = ".", col = "cadet blue")  ## notice anything odd?


## ----line_170_, message=FALSE--------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(GGally)
})
ggpairs(data = tots,
        upper = list(continuous=wrap("cor", size=3, fontface="bold", 
                                     colour="saddlebrown")),
        diag = list(continuous=wrap("barDiag", bins=20)),
        lower = list(continuous=wrap("points", size=0.5,
                                     colour=alpha("skyblue", 1/10))))


## ----line_185_-----------------------------------------------------------------------------------------------------------
cor(churnData %>% select(ends_with("_minutes")),
    churnData %>% select(ends_with("_charge")))


## ----line_210_-----------------------------------------------------------------------------------------------------------
state_props <- churnData             %>% 
  group_by(state)                    %>% 
  summarise(n = n(),                         ## state total
            p = mean(churn == "no")) %>%     ## state proportion
  arrange(p)                                 ## 'league table' ordering
head(state_props %>% untibble(), 6)          ## Californians!


## ----line_220_,out.height="0.8\\textheight",out.width="1.07\\textheight"-------------------------------------------------
with(state_props,
     barplot(p, names.arg = state, las = 2, ylim = c(0,1),
             family = "mono", fill = pal_sea2sand)) ## colours are pointless!!


## ----line_227_,out.height="0.6\\textheight",out.width="0.8\\textheight"--------------------------------------------------
state_props <- state_props %>% 
  mutate(state = factor(as.character(state), levels = as.character(state)))
ggplot(state_props) + aes(x = state, y = p, fill = p) + ylim(0, 1) +
  geom_bar(stat = "identity") + scale_fill_viridis_c() + theme_bw() +
  theme(legend.position="none")


## ----line_236_,out.height="0.6\\textheight",out.width="0.8\\textheight"--------------------------------------------------
churn_tmp <- churnData %>% 
  mutate(ch = (churn == "no") + 0) ## binary numeric
ggplot(churn_tmp) + aes(x = day_minutes, y = ch) + 
  geom_point(colour = "steelblue") +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs"), 
              colour = "red")


## ----line_248_-----------------------------------------------------------------------------------------------------------
localWeighting <- function(x, y, scale) {
  y_wtd <- numeric(length(y))
  for(i in seq_along(y)) {
    y_wtd[i] <- weighted.mean(y, w = exp(-((x-x[i])/scale)^2))
  }
  y_wtd
}


## #include <Rcpp.h>

## using namespace Rcpp;

## 
## // [[Rcpp::export]]

## NumericVector localWeightCpp(NumericVector x,

##                              NumericVector y,

##                              double scale) {

##   int n = y.size();

##   NumericVector y_wtd(n);

## 

##   for(int i = 0; i < n; i++) {

##     double w, swy = 0.0, sw = 0.0;

##     for(int j = 0; j < n; j++) {

##       w = exp(-pow((x[i] - x[j])/scale, 2));

##       swy += w * y[j];

##       sw += w;

##     }

##     y_wtd[i] = swy/sw;

##   }

##   return y_wtd;

## }


## ----line_284_-----------------------------------------------------------------------------------------------------------
yw1 <- with(churn_tmp, localWeighting(day_minutes, ch, scale = 25))
yw2 <- with(churn_tmp, localWeightCpp(day_minutes, ch, scale = 25))
all.equal(yw1, yw2)


## ----line_291_,cache=FALSE-----------------------------------------------------------------------------------------------
library(rbenchmark)
with(churn_tmp, 
     benchmark(localWeighting(day_minutes, ch, scale = 25),
               localWeightCpp(day_minutes, ch, scale = 25),
     replications = 10, columns = c("test", "elapsed", "relative")))

## ----include=FALSE-------------------------------------------------------------------------------------------------------
if("package:MASS" %in% search()) detach("package:MASS")


## ----line_299_,fig.show="hold"-------------------------------------------------------------------------------------------
churn_tmp <- churn_tmp %>% 
  mutate(p_wt = localWeightCpp(day_minutes, ch, scale = 25))
ggplot(churn_tmp) + aes(x = day_minutes, y = ch) +
  geom_point(colour = "steelblue") + 
  geom_smooth(se = FALSE, 
              method = "gam", formula = y ~ s(x, bs = "cs"), 
              method.args = list(family = "binomial"), 
              colour = "red") +
  geom_line(aes(x = day_minutes, y = p_wt), colour = "dark green") +
  ylab("Proportion: no churn") + xlab("Daytime minutes")
###
### There seems to be broad qualitative agreement.


## ----line_315_-----------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(mgcv))
greyish <- grey(0.35)
ghostgrey <-  alpha("grey", 0.5)
churn_tmp %>% 
  arrange(day_minutes) %>%   # x now has to be ordered.
  with({ 
    plot(day_minutes, ch, pch = 20, axes = FALSE, bty = "n", type = "n",
         xlab = "Daytime minutes", ylab = "Proportion: no churn",
         cex.lab = 0.8)
    axis(1, col = "transparent", col.axis = greyish, 
         cex.axis = 0.7, col.ticks = ghostgrey)
    axis(2, col = "transparent", col.axis = greyish, 
         cex.axis = 0.7, col.ticks = ghostgrey)
    grid(lty = "solid", col = ghostgrey)
    points(day_minutes, ch, pch = 20, col = "steelblue")
    smooth <- fitted(gam(ch ~ s(day_minutes, bs = "cs"), binomial))
    lines(day_minutes, smooth, col = "red")
    lines(day_minutes, p_wt, col = "dark green")
  })


## ----line_346_,size="footnotesize"---------------------------------------------------------------------------------------
sample_column <- which(names(churnData) == "sample")  ## calculate it!
churn_train <- churnData[churnData$sample == "train", -sample_column]
churn_test  <- churnData[churnData$sample == "test" , -sample_column]


## ----line_353_-----------------------------------------------------------------------------------------------------------
churn_train <- subset(churnData, sample == "train", select = -sample)
churn_test  <- subset(churnData, sample == "test" , select = -sample)


## ----line_359_-----------------------------------------------------------------------------------------------------------
churn_train <- churnData %>% filter(sample == "train") %>% select(-sample)
churn_test  <- churnData %>% filter(sample == "test" ) %>% select(-sample)


## ----line_375_-----------------------------------------------------------------------------------------------------------
find("quine")  ## Check that you have it.  Otherwise quine <- MASS::quine
str(quine)


## ----line_380_,include=FALSE---------------------------------------------------------------------------------------------
options(xtable.include.rownames=FALSE)


## ----line_385_,eval=FALSE------------------------------------------------------------------------------------------------
## counts <- with(quine, table(Eth, Sex, Age, Lrn)) %>%
##   as.data.frame(responseName = "Count")
## pivot_wider(counts, names_from = Age, values_from = Count) %>% booktabs()


## ----line_385__,echo=FALSE,results="asis"--------------------------------------------------------------------------------
counts <- with(quine, table(Eth, Sex, Age, Lrn)) %>% 
  as.data.frame(responseName = "Count")
pivot_wider(counts, names_from = Age, values_from = Count) %>% booktabs()


## ----line_392_,eval=FALSE------------------------------------------------------------------------------------------------
## tab <- counts %>%
##   mutate(AgeSex = Age:Sex) %>%
##   pivot_wider(id_cols = c(Eth, Lrn),
##               names_from = AgeSex, values_from = Count) %>%
##   arrange(Lrn, Eth)
## booktabs(tab)


## ----line_392__,results="asis",echo=FALSE--------------------------------------------------------------------------------
tab <- counts %>% 
  mutate(AgeSex = Age:Sex) %>% 
  pivot_wider(id_cols = c(Eth, Lrn),
              names_from = AgeSex, values_from = Count) %>%
  arrange(Lrn, Eth)
booktabs(tab)


## ----line_403_,out.lines=7-----------------------------------------------------------------------------------------------
counts1 <- tab %>% 
  pivot_longer(names_to = "AgeSex", values_to = "Count", 
               `F0:F`:`F3:M`) %>%  ## note `...`
  separate(AgeSex, into = c("Age", "Sex"), sep = ":") %>% 
  select(names(counts)) %>% 
  unclass() %>% data.frame() %>% 
  arrange(Lrn, Age, Sex, Eth)
# cbind(counts, ".  " = "     ", counts1) ## crude check
all.equal(counts, counts1)


## ----line_485_,fig.show="hold"-------------------------------------------------------------------------------------------
Stats <- quine %>% 
  group_by(Age, Sex, Eth, Lrn) %>% 
  summarise(Count = n(), Mean = mean(Days), S2 = var(Days)) %>% 
  ungroup() %>% untibble()
Stats1 <- na.omit(Stats)  ## it will happen anyway!
#####
##### mean-variance plot
##### 
ggplot(Stats1) + aes(x=Mean, y=S2, size=Count, colour=Sex) +
  geom_point()  + ylab("Variance") +
  scale_colour_manual(values = c(F = "hotpink", M = "steelblue")) +
  stat_smooth(data=Stats1, aes(x=Mean, y=S2, weight = Count), 
              method="lm", formula = y ~ 0+offset(x)+I(x^2),
              colour="black", se=FALSE) + 
  guides(size = "none") + theme(legend.position = c(0.1, 0.85))


## ------------------------------------------------------------------------------------------------------------------------
mv0 <- lm(S2 ~ 0 + offset(Mean) + I(Mean^2), Stats, weights = Count)
summary(mv0)$coefficients
(theta_0 <- as.vector(1/coef(mv0)["I(Mean^2)"]))
(theta_ml <- with(quine, theta.ml(Days, ave(Days, Eth, Sex, Age, Lrn))))

as.vector(theta_0 - theta_ml)/attr(theta_ml, "SE")  ## "quasi-t-statistic" 



## ----line_521_,echo=FALSE,results="asis"---------------------------------------------------------------------------------
txt <- textConnection('
  Issue & Tools
  input & gzfile, read_csv, tibble
  chainint & %>% binary operator
  manipulation & five key functions:
  & select, filter, mutate, group_by, summarise
  & helper functions:
  & begins_with, ends_with, contains, everything, ...
  shaping & two key functions:
  & gather, spread
  & helper functions:
  & separate')


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(Sys.Date()), "\n}") 
toLatex(sessionInfo(), locale = FALSE)

