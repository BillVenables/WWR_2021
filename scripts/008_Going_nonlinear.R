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


## ----include=FALSE-------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(lme4))


## ------------------------------------------------------------------------------------------------------------------------
p0 <- ggplot(Stormer) + 
  aes(x = Viscosity, y = Time, group = factor(Weight), colour = factor(Weight)) +
  geom_point(size = 1.25)  + theme_bw() + 
  geom_line(linetype = "dashed", colour = "grey") + 
  scale_color_brewer(palette = "Dark2", name = "Weight (gm)") +
  theme(legend.position = "bottom")

p0  + geom_point(aes(x = 0, y = 0), inherit.aes = FALSE, size = 0.7)


## ------------------------------------------------------------------------------------------------------------------------
storm_iv <- lm(I(Time*Weight) ~ 0 + Viscosity + Time, Stormer)
(init <- setNames(coef(storm_iv), c("beta", "theta")))


## ------------------------------------------------------------------------------------------------------------------------
storm_nl <- nls(Time ~ beta*Viscosity/(Weight-theta), data=Stormer, start=init)
round(summary(storm_nl)$coefficients, digits = 3)


## ------------------------------------------------------------------------------------------------------------------------
m <- with(Stormer, tapply(Viscosity, Weight, max))
w <- as.numeric(names(m))
pStormer <- rbind(cbind(Viscosity = 0,         Weight = w), 
                  cbind(Viscosity = unname(m), Weight = w)) %>% 
  data.frame()
pStormer$Time <- predict(storm_nl, newdata = pStormer)
pStormer

p0 + geom_line(data = pStormer)



## ------------------------------------------------------------------------------------------------------------------------
(stormer <- deriv(~ b*V/(W - t), namevec = c("b", "t"), 
                  function.arg = function(V, W, b, t) {}) %>% fix_deriv())


## ------------------------------------------------------------------------------------------------------------------------
storm_nl2 <- nls(Time ~ stormer(Viscosity, Weight, beta, theta), Stormer, 
                 start = init)
summary(storm_nl2)


## ------------------------------------------------------------------------------------------------------------------------
SSstormer <- selfStart(model = ~ b*v/(w - c), parameters = c("b", "c"),
                       initial = function(mCall, data, LHS, ...) {## '...' needed
                         t <- eval(LHS, data)                     ## in R 4.0.4
                         v <- eval(mCall[["v"]], data)
                         w <- eval(mCall[["w"]], data)
                         b <- coef(lm(I(w*t) ~ 0 + v + t))
                         setNames(b, mCall[c("b", "c")])
                       },
                       template = function(v, w, b, c) {})
storm_nl3 <- nls(Time ~ SSstormer(Viscosity, Weight, beta, theta),
                 data = Stormer)
coef(storm_nl3)


## ------------------------------------------------------------------------------------------------------------------------
SSstormer2 <- selfStart(~ b*(v + f)/(w - c), 
                        parameters = c("b", "c", "f"),
                        initial = function (mCall, data, LHS, ...) {
                          t <- eval(LHS, data)
                          v <- eval(mCall[["v"]], data)
                          w <- eval(mCall[["w"]], data)
                          b <- coef(lm(I(w * t) ~ 1 + v + t))  ## incl intercept
                          b <- c(b[2:3], b[1]/b[2])            ## change back
                          setNames(b, mCall[c("b", "c", "f")])
                        },
                        template = function(v, w, b, c, f) {}) %>% fix_deriv()
storm_nl4 <- nls(Time ~ SSstormer2(Viscosity, Weight, beta, theta, phi), 
                 data = Stormer)
coef(storm_nl4)


## ------------------------------------------------------------------------------------------------------------------------
storm_nle <- nlmer(
  Time ~ SSstormer2(Viscosity,Weight,beta,theta,phi) ~ (phi|Viscosity), ## 3-form
  data = Stormer, start = coef(storm_nl4))                              ## start!
rbind(`base model`            = c(coef(storm_nl), phi = 0), 
      perturbed               = coef(storm_nl4), 
      `nlmer (fixed effects)` = fixef(storm_nle)) %>% round(4)


## ------------------------------------------------------------------------------------------------------------------------
(B <- coef(storm_nle)$Viscosity)


## ------------------------------------------------------------------------------------------------------------------------
phi <- setNames(B[["phi"]], rownames(B))
Stormer <- within(Stormer, {
  Viscosity_phi <- Viscosity + phi[factor(Viscosity)]
})


## ------------------------------------------------------------------------------------------------------------------------
p0 <- ggplot(Stormer) + aes(x = Viscosity_phi, y = Time) + 
  geom_point(aes(colour = factor(Weight)), size = 0.7) +
  geom_line(aes(colour = factor(Weight))) +
  geom_point(aes(x = Viscosity), size = 0.7) +
  geom_segment(aes(xend = Viscosity, yend = Time), colour = "grey") +
  geom_point(aes(x = 0, y = 0)) +
  xlab(expression(Viscosity + (phi + delta))) + 
  scale_colour_brewer(palette = "Dark2", name = "Weight (gm)") + 
  theme_bw() + theme(legend.position = "bottom")
p0


## ------------------------------------------------------------------------------------------------------------------------
p0 <- ggplot(wtloss) + aes(x = Days, y = Weight) + geom_point() + theme_bw()
p0


## ------------------------------------------------------------------------------------------------------------------------
pWtloss <- data.frame(Days = 0:730)  ## two years
pWtloss <- within(pWtloss, {
  quadratic <- predict(lm(Weight ~ poly(Days, 2), wtloss), pWtloss)
  cubic     <- predict(lm(Weight ~ poly(Days, 3), wtloss), pWtloss)
})
pwt <- pWtloss %>% pivot_longer(cols = c(quadratic, cubic), 
                                names_to = "degree", 
                                values_to = "Weight")%>% 
  mutate(degree = factor(degree, levels = cq(quadratic, cubic))) ## cq() ...

p0 + geom_line(data = pwt, aes(colour = degree)) +
  scale_colour_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom")


## ------------------------------------------------------------------------------------------------------------------------
SSnegexp <- selfStart(model = ~ b0 + b1*exp(-t/theta),
                      parameters = c("b0", "b1", "theta"),
                      initial = function(mCall, data, LHS, ...) {
                        t <- eval(mCall[["t"]], data)
                        y <- eval(LHS, data)
                        r <- range(t)
                        d <- (r[2] - r[1])/4
                        yf <- predict(lm(y ~ poly(t, 2)), 
                                      data.frame(t = r[1] + (1:3)*d))
                        ratio <- (yf[1] - yf[2])/(yf[2] - yf[3])
                        stopifnot(ratio > 0)
                        theta <- d/log(ratio)
                        b <- coef(lm(y ~ 1 + exp(-t/theta)))
                        setNames(c(b, theta), mCall[c("b0", "b1", "theta")])
                      }) %>% fix_deriv()


## ------------------------------------------------------------------------------------------------------------------------
wtloss_ne <- nls(Weight ~ SSnegexp(Days, b0, b1, theta), data = wtloss, 
                 trace = TRUE)
summary(wtloss_ne)


## ------------------------------------------------------------------------------------------------------------------------
pWtloss$negexp <- predict(wtloss_ne, pWtloss)
pwt <- pWtloss %>% pivot_longer(cols = c(quadratic, cubic, negexp), 
                                names_to = "degree", 
                                values_to = "Weight") %>% 
  mutate(degree = factor(degree, levels = cq(quadratic, cubic, negexp))) ## cq()
p0 + geom_line(data = pwt, aes(colour = degree)) +
  geom_hline(yintercept = coef(wtloss_ne)[["b0"]],colour = "grey",
             linetype = "dashed")+
  scale_colour_brewer(palette = "Dark2") + theme(legend.position = "bottom")


## ------------------------------------------------------------------------------------------------------------------------
period <- 28 ## Recalibrate every 4 weeks

W0 <- with(wtloss, Weight[Days == 0])
tab <- cbind(Days          = 0, 
             n             = 1, 
             Weight        = W0, 
             Final         = NA, 
             `Yet to lose` = NA, 
             `Half life`   = NA) 

end <- with(wtloss, max(Days))  ## Latest data available

full_model <- nls(Weight ~ SSnegexp(Days, W0, w, theta), wtloss)



## ----tab, eval=FALSE-----------------------------------------------------------------------------------------------------
## days <- 0
## while(days < end) {
##   days <- min(end, days + period)
##   n <- with(wtloss, sum(Days <= days))  ## sample size
##   m <- update(full_model, data = filter(wtloss, Days <= days))
##   b <- coef(m)
##   w <- predict(m, data.frame(Days = days))
##   tab <- rbind(tab, unname(c(days, n, w, b[1], w-b[1], log(2)*b[3])))
## }
## tab <- data.frame(tab, check.names = FALSE) %>%
##   within({
##     Change <- c(NA, diff(Weight))
##   }) %>%
##   select(Days, n, Weight, Change, everything())
## 
## booktabs(tab, digits = 0) %>% print(include.rownames = FALSE)


## ----results="asis", echo=FALSE------------------------------------------------------------------------------------------
days <- 0
while(days < end) {
  days <- min(end, days + period)
  n <- with(wtloss, sum(Days <= days))  ## sample size
  m <- update(full_model, data = filter(wtloss, Days <= days)) 
  b <- coef(m)
  w <- predict(m, data.frame(Days = days))
  tab <- rbind(tab, unname(c(days, n, w, b[1], w-b[1], log(2)*b[3])))
}
tab <- data.frame(tab, check.names = FALSE) %>% 
  within({
    Change <- c(NA, diff(Weight))
  }) %>% 
  select(Days, n, Weight, Change, everything())

booktabs(tab, digits = 0) %>% print(include.rownames = FALSE)


## ------------------------------------------------------------------------------------------------------------------------
p0 <- ggplot(muscle) + aes(x = Conc, y = Length) + 
  geom_point(size = 0.7) +
  xlab("CaCl concentration") + ylab("Contraction (mm)") +
  facet_wrap(~ Strip, nrow = 3) +
  theme_bw() + 
  theme(strip.background = element_rect(fill = "wheat"))
p0 + geom_line(colour = "sky blue") 


## ------------------------------------------------------------------------------------------------------------------------
mfix <- lm(Length ~ 0 + Strip/Conc + I(Conc^2/2), data = muscle)
mran <- lmer(Length ~ Conc + I(Conc^2/2) + (1 + Conc|Strip), data = muscle)
pMuscle <- with(muscle, expand.grid(
  Strip = levels(Strip), Conc = seq(min(Conc), max(Conc), length = 100))
)
pMuscle <- within(pMuscle, {
  fixed  <- predict(mfix, pMuscle)
  random <- predict(mran, pMuscle, re.form = ~(1 + Conc|Strip))
}) 
pms <- pMuscle %>% pivot_longer(cols = c(fixed, random), names_to = "Model", 
                                values_to = "Length")
p0 + geom_line(data = pms, aes(colour = Model)) +
  scale_colour_brewer(palette = "Set2") + ylim(0, 45) +
  theme(legend.position = "bottom") + labs(title = "Quadratic Polynomial Models")


## ------------------------------------------------------------------------------------------------------------------------
SSgompertz2 <- selfStart(model = ~ exp(b0 - b1*rho^x), 
                         parameters = c("b0", "b1", "rho"),
                         initial = function(mCall, data, LHS, ...) {
                           x <- eval(mCall[["x"]], data)
                           y <- eval(LHS, data)
                           r <- range(x)
                           d <- (r[2] - r[1])/4
                           yf <- predict(lm(log(y) ~ poly(x, 2)),
                                         data.frame(x = r[1] + (1:3)*d))
                           ratio <- ((yf[3] - yf[2])/(yf[2] - yf[1]))
                           stopifnot(ratio > 0)
                           rho <- ratio^(1/d)
                           b <- coef(lm(log(y) ~ 1 + I(-rho^x)))
                           setNames(c(b, rho), mCall[c("b0", "b1", "rho")])
                         }) %>% fix_deriv()


## ------------------------------------------------------------------------------------------------------------------------
m0 <- nls(Length ~ SSgompertz2(Conc, b0, b1, rho), data = muscle) ## Ignore Strip
b <- coef(m0)
init <- list(b0 = rep(b[["b0"]], 21),  ## must be a list for indexed parameters
             b1 = rep(b[["b1"]], 21), 
             rho = b[["rho"]])
fgomp <- nls(Length ~ exp(b0[Strip] - b1[Strip]*rho^Conc), data = muscle, 
             start = init)
rgomp <- nlmer(Length ~ SSgompertz2(Conc, b0, b1, rho) ~ b0 + b1|Strip,
               data = muscle, start = coef(m0))


## ------------------------------------------------------------------------------------------------------------------------
pMuscle <- within(pMuscle, {
  fixed <- predict(fgomp, pMuscle)  ## replaces the previous component
})


## ----out.lines=10--------------------------------------------------------------------------------------------------------
fixef(rgomp)
ranef(rgomp)$Strip


## ----out.lines=5---------------------------------------------------------------------------------------------------------
(B <- coef(rgomp)$Strip)


## ------------------------------------------------------------------------------------------------------------------------
B <- B %>% rownames_to_column("Strip") %>% untibble()
pMuscle <- pMuscle %>% left_join(B, by = "Strip") %>% 
  mutate(random = exp(b0 - b1*rho^Conc), Strip = factor(Strip)) %>% 
  select(Strip, Conc, fixed, random)


## ------------------------------------------------------------------------------------------------------------------------
pms <- pMuscle %>% pivot_longer(cols = c(fixed, random), names_to = "Model", 
                                values_to = "Length")
p0 + geom_line(data = pms, aes(colour = Model)) +
  scale_colour_brewer(palette = "Set2") + ylim(0, 45) +
  theme(legend.position = "bottom") + labs(title = "Gompertz Growth Curve Models")


## ------------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(nlme)
})
rgomp <- nlme(Length ~ SSgompertz2(Conc, b0, b1, rho), data = muscle, 
           fixed = b0 + b1 + rho ~ 1, random = b0 + b1 ~ 1|Strip, 
           start = coef(m0))
pMuscle <- within(pMuscle, {
  fixed  <- predict(fgomp, pMuscle)  ## replaces the component (not needed!)
  random <- predict(rgomp, pMuscle, level = 1)  ## replaces the previous component
  common <- predict(rgomp, pMuscle, level = 0)  ## same for all strips (fixef)
})
pms <- pMuscle %>% pivot_longer(cols = c(fixed, random, common), 
                                names_to = "Model", values_to = "Length") %>% 
  mutate(Model = factor(Model, levels = cq(fixed, random, common)))
p0 + geom_line(data = pms, aes(colour = Model)) +
  scale_colour_brewer(palette = "Set2") + ylim(0, 45) +
  theme(legend.position = "bottom") + labs(title = "Gompertz Models (nlme)")


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(Sys.Date()), "\n}") 
toLatex(sessionInfo(), locale = FALSE)

