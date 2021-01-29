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


## ----line_034_,include=FALSE---------------------------------------------------------------------------------------------
rm(session)


## ----line_101_,echo=FALSE------------------------------------------------------------------------------------------------
plot.new()
eps <- 0.32
delta <- 0.125
theta <- 0.01190476
par(mar = rep(0, 4), usr = c(0, 1 + eps + theta, 0-theta*2, 1 + delta/2),
    family = "mono", font = 2, cex = 1)
x <- c(1/3, 2/3, 2/3, 1/3)
y <- c(0, 0, 1/5, 1/5)
polygon(x, y, density = 0)
text(mean(x), mean(y), "grDevices")
text(mean(x), mean(y)/2.5, "(R core)", cex = 0.7, family = "serif", font = 3)

segments(mean(x), 1/5, mean(x), max(y + 2/3) + delta, lwd = 3, col = "red")
text((min(x - 1/4 - 1/14) + mean(x))/2,
     max(y + 2/3) + delta/2,
     "Base Grahpics", family = "serif", font = 4, cex = 1)
text((max(x + 1/4 + 1/12 + eps) + mean(x))/2,
     max(y + 2/3) + delta/2,
     "Grid Grahpics", family = "serif", font = 4, cex = 1)

polygon(x - 1/4 - 1/14, y + 1/3, density = 0)
text(mean(x) - 1/4 - 1/14, mean(y) + 1/3, "graphics")
text(mean(x) - 1/4 - 1/14, mean(y)/2.5 + 1/3, "(R core)", cex = 0.7, family = "serif", font = 3)
arrows(1/4 - 1/14, 1/3, 1/3 + 1/12, 1/5, angle = 15, length=2, gap=2)

polygon(x + 1/4, y + 1/3, density = 0)
text(mean(x) + 1/4, mean(y) + 1/3, "grid")
text(mean(x) + 1/4, mean(y)/2.5 + 1/3, "(P. Murrell)", cex = 0.7, family = "serif", font = 3)
arrows(1/4 + 1/2, 1/3, 2/3 - 1/12, 1/5, angle = 15, length=2, gap=2)

polygon(x + 1/4 + 1/12 + eps, y + 1/3, density = 0)
text(mean(x) + 1/4 + 1/12 + eps, mean(y) + 1/3, "gridExtra")
text(mean(x) + 1/4 + 1/12 + eps, mean(y)/2.5 + 1/3, "(P. Murrell)", cex = 0.7, family = "serif", font = 3)
arrows(x[1] + 1/4 + 1/12 + eps, mean(y) + 1/3, max(x + 1/4), mean(y) + 1/3, angle = 15, length=2, gap=2)

polygon(x + 1/4 + 1/12 + eps, y + 2/3, density = 0)
text(mean(x) + 1/4 + 1/12 + eps, mean(y) + 2/3, "lattice")
text(mean(x) + 1/4 + 1/12 + eps, mean(y)/2.5 + 2/3, "(D. Sarkar)", cex = 0.7, family = "serif", font = 3)
arrows(1/4 + 1/2 + 1/12 + eps, 2/3, max(x + 1/4), 1/5 + 1/3, angle = 15, length=2, gap=2)

polygon(x - 1/6 + 1/10 + eps, y + 2/3, density = 0)
text(mean(x) - 1/6 + 1/10 + eps, mean(y) + 2/3, "ggplot2")
text(mean(x) - 1/6 + 1/10 + eps, mean(y)/2.5 + 2/3, "(H. Wickham)", cex = 0.7, family = "serif", font = 3)
## arrows(mean(x) - 1/6 + 1/10 + eps, 2/3, 2/3, 1/5 + 1/3, angle = 15, length=2, gap=2)
arrows(mean(x) - 1/6 + 1/10 + eps, 2/3, mean(x) - 1/6 + 1/10 + eps, 1/5 + 1/3, angle = 15, length=2, gap=2)

polygon(x - 1/4 - 1/14, y + 2/3, density = 0)
arrows(mean(x) - 1/4 - 1/14, 2/3, mean(x) - 1/4 - 1/14, 1/5 + 1/3, angle = 15, length=2, gap=2)
text(mean(x) - 1/4 - 1/14, mean(y) + 2/3 + 1/35, "Most plot()", cex = 0.8)
text(mean(x) - 1/4 - 1/14, mean(y) + 2/3 - 1/35, "methods", cex = 0.8)
# box()


## ----line_215_,out.height="0.45\\textheight"-----------------------------------------------------------------------------
boston <- Boston %>% ## ?Boston gives the full documentation
  within({
    Riverside <- recode(chas, `0` = "No", `1` = "Yes")
    Poverty <- sqrt(lstat)
  }) %>% 
  select(Price = medv, Rooms = rm, Poverty, Riverside) ## raname
gg_object <- ggplot(boston)  ## embryonic ggplot object;
gg_object + theme_grey()     ## blank canvas at this stage


## ----line_229_,fig.height=5,fig.width=7,out.height="0.65\\textheight"----------------------------------------------------
gg_object <- gg_object + aes(x = Rooms, y = Price)
gg_object + theme_grey()  ## we get an axis system, but ...


## ----line_235_,fig.height=5,fig.width=7,out.height="0.65\\textheight"----------------------------------------------------
gg_object <- gg_object + aes(colour = Poverty)       ## use colour, but how?
gg_object + geom_point() + scale_colour_viridis_c()  ## 'colour-blind friendly'


## ----line_241_,fig.height=5,fig.width=7,out.height="0.65\\textheight"----------------------------------------------------
ggplot(boston) + aes(x = Rooms, y = Price) + geom_point(colour = "steelblue") +
  labs(x = "Mean number of rooms", y = "Median house value in $000s")


## ----line_248_,fig.height=5,fig.width=7,out.height="0.65\\textheight"----------------------------------------------------
gg_object <- gg_object + labs(x = "Mean number of rooms", 
                              y = "Median house value in $000s")
gg_object + geom_hex(bins = 35) + scale_fill_viridis_c() 


## ----line_255_,fig.height=6,fig.width=7,out.height="0.65\\textheight"----------------------------------------------------
gg_object + stat_bin_hex(bins = 35) + scale_fill_distiller(palette = 3) +
  theme_bw() + theme(legend.position = "bottom")


## ----line_271_-----------------------------------------------------------------------------------------------------------
sapply(c("mtcars", "Cars93"), find) %>% noquote


## ----line_276_,fig.height=5,fig.width=10,out.height="0.6\\textheight"----------------------------------------------------
# library(gridExtra)  ## library(patchwork) is an alternative
plt73 <- ggplot(mtcars) + aes(x = wt, y = mpg) + 
  geom_point(colour = "steelblue") + labs(title="1973")
plt93 <- ggplot(Cars93) + aes(x = Weight, y = MPG.city) + 
  geom_point(colour = "brown") + labs(title="1993")
plt73 + plt93


## ----line_286_,fig.height=5,fig.width=10,out.height="0.6\\textheight"----------------------------------------------------
sm <- geom_smooth(method = "loess", se = FALSE,
                  colour = "darkgreen", formula = y ~ x)
(plt73 + sm) + (plt93 + sm)


## ----line_296_,out.height="0.8\\textheight"------------------------------------------------------------------------------
modl93 <- lm(MPG.city ~ Weight, Cars93)  ## assume we want a straight line
box_cox(modl93)                   ## traditional graphics, in WWRUtilities


## ----line_302_,fig.height=5,fig.width=10,out.height="0.6\\textheight"----------------------------------------------------
plt73 <- ggplot(mtcars) + aes(x = wt, y = 100/mpg) + 
  geom_point(colour = "steelblue") + labs(title="1973") 
plt93 <- ggplot(Cars93) + aes(x = Weight, y = 100/MPG.city) + 
  geom_point(colour = "brown") + labs(title="1993")
(plt73 + sm) + (plt93 + sm)


## ----line_313_,fig.height=5,fig.width=10,out.height="0.6\\textheight"----------------------------------------------------
sml <- geom_smooth(method = "lm", se = FALSE, 
                   colour = "darkgreen", formula = y ~ x)
(plt73 + sml) + (plt93 + sml)


## ----line_321_,out.lines=8-----------------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  ## no auto conversion
cu.in2litres <- 0.0163871 ## conversion factor
data73 <- with(mtcars, {
  data.frame(Make = row.names(mtcars), Year = "1973", GPM = 100/mpg, 
             Displacement = disp * cu.in2litres, Weight = wt * 1000)
})
data93 <- with(Cars93, {
  data.frame(Make = as.character(Make), Year = "1993", GPM = 100/MPG.city, 
             Displacement = EngineSize, Weight = Weight)
})
(Cars <- bind_rows(data73, data93) %>% arrange(GPM))


## ----line_336_,fig.height=5,fig.width=8,out.height="0.5\\textheight"-----------------------------------------------------
plt <- ggplot(Cars) + aes(x = Weight, y = GPM, colour = Year) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(title = "Fuel consumption rates, 1973 and 1993",
       x = "Weight of vehicle (in lbs)", y = "Fuel consumption (Gals/100 miles)") + 
  theme(plot.title = element_text(hjust = 0.5)) +  ## centre the title
  scale_colour_manual(values = c(`1973` = "steelblue", `1993` = "brown"))
plt


## ----line_348_,fig.height=5,fig.width=10,out.height="0.6\\textheight"----------------------------------------------------
plt + facet_wrap(~ Year, nrow = 1) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


## ----line_355_,fig.height=4.5,fig.width=7,out.height="0.55\\textheight"--------------------------------------------------
plt2 <- ggplot(Cars) + aes(x = Displacement, y = GPM, colour = Year) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(title = "Fuel consumption rates, 1973 and 1993",
       x = "Engine size (in litres)", y = "Fuel consumption (Gals/100 miles)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(values = c(`1973` = "steelblue", `1993` = "brown"))
plt2


## ----line_371_,out.height="0.8\\textheight"------------------------------------------------------------------------------
plt3 <- ggplot(Cars) + aes(x = Weight, y = Displacement, colour = Year) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(title = "Engine size vs vehicle weight, 1973 and 1993", 
       x = "Vehicle weight (in lbs)", y = "Engine size (in litres)") +
  scale_colour_manual(values = c(`1973` = "steelblue", `1993` = "brown")) +
  theme_minimal() +   ## take care of background
  theme(plot.title = element_text(hjust = 0.5)) ## must come after theme_minimal()
plt3


## ----line_387_,out.height="0.7\\textheight"------------------------------------------------------------------------------
library(ggrepel)  ## add-on package to ggplot2
bigCars <- Cars %>% filter(Displacement > 5.5)
plt3 + geom_text_repel(data = bigCars, aes(label = Make), 
                       size = 3, colour = "black") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


## ----storm, fig.height=6, fig.width=8,  out.height="0.45\\textheight"----------------------------------------------------
ggplot(mutate(Stormer, Weight=factor(Weight))) +
  aes(x=Viscosity, y=Time, group=Weight) + geom_line(colour="steelblue") + 
  geom_point(aes(shape=Weight), size=2) + theme(legend.position=c(0.1, 0.85))


## ----line_404_-----------------------------------------------------------------------------------------------------------
fm <- nls(Time ~ beta*Viscosity/(Weight - theta), Stormer, 
          start = list(beta = 30, theta = 2))
summary(fm)$coef
bt <- coef(fm)
se <- sqrt(diag(vfm <- vcov(fm)))
cov2cor(vfm)


## ----line_416_-----------------------------------------------------------------------------------------------------------
ssq <- function(beta, theta) {
  sum((Time - beta*Viscosity/(Weight - theta))^2)
}
environment(ssq) <- as.environment(Stormer) ## get Weight &c
parent.env(environment(ssq)) <- baseenv() ## get sum(), ^, &c
SSQ <- Vectorize(ssq)   ## vectorization on the fly!


## ----line_428_-----------------------------------------------------------------------------------------------------------
beta0  <- bt[["beta"]];  bspan <- 5*se[[ "beta"]]
theta0 <- bt[["theta"]]; tspan <- 5*se[["theta"]]

Beta  <- seq(beta0 - bspan, beta0 + bspan, length.out = 201)
Theta <- seq(theta0- tspan, theta0+ tspan, length.out = 201)

RSSq <- expand.grid(beta = Beta, theta = Theta) %>% 
  within({
    Rssq <- SSQ(beta, theta)
    logF <- log(Rssq) - log(ssq(beta0, theta0))
  })


## ----line_441_-----------------------------------------------------------------------------------------------------------
ggplot(RSSq) + aes(x = beta, y = theta) +
  geom_raster(aes(fill = logF)) + 
  geom_point(x = beta0, y = theta0, shape = 3, size = 3)+
  scale_fill_continuous(high = "lemon chiffon", low = "#DF536B", 
                        name = "log(RSS/min(RSS))") +
  stat_contour(aes(z = logF), 
               colour = "black", 
               size = 0.25,
               breaks = c(1/8, (1:7)/2)) +
  labs(x = expression(beta), y = expression(theta),
       title = expression(Time == ' '*frac(beta%*%Viscosity, 
                                           (Weight-' '*theta))+epsilon)) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")


## ----trad----------------------------------------------------------------------------------------------------------------
pal <- colorRampPalette(c("#DF536B", "lemon chiffon"))
logF <- log(outer(Beta, Theta, SSQ))
logF <- logF - log(ssq(beta0, theta0))
image(x = Beta, y = Theta, z = logF, col = pal(250), 
      xlab = expression(beta), ylab = expression(theta),
      main = expression(Time == ' '*frac(beta%*%Viscosity,
                                         (Weight-' '*theta))+epsilon))
contour(x = Beta, y = Theta, z = logF, levels = c(1/8, (1:7)/2), add = TRUE)
points(beta0, theta0, pch = 3, cex = 2)


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(Sys.Date()), "\n}") 
toLatex(sessionInfo(), locale = FALSE)

