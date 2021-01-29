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
  comment = "",
  cache = FALSE
) #$
session <- sub("^0+", "", sub("[^[:digit:][:alpha:]].*$", "", .infile))


## ----c01,fig.height=5,fig.width=7,out.height="2.5in"---------------------------------------------------------------------
theme_set(theme_bw() + theme(plot.title = element_text(hjust=0.5)))

ggplot(janka) + aes(x = Density, y = Hardness) +
  geom_point(colour = "red") +  labs(title = "Janka Hardness")


## ----c02-----------------------------------------------------------------------------------------------------------------
m1 <- lm(Hardness ~ Density, janka)
m2 <- update(m1, . ~ . + I(Density^2))
m3 <- update(m2, . ~ . + I(Density^3))
round(summary(m3)$coef, 4)


## ----c03-----------------------------------------------------------------------------------------------------------------
m1a <- lm(Hardness ~ I(Density - 50), janka)
m2a <- update(m1a, . ~ . + I((Density - 50)^2))
m3a <- update(m2a, . ~ . + I((Density - 50)^3))
round(summary(m3a)$coef, 4)


## ----"jank-3xx",fig.height=6,fig.width=8---------------------------------------------------------------------------------
m0a <- lm(Hardness ~ 1, janka)  ## constant predictor model
m4a <- update(m3a, .~.+I((Density-50)^4))

pJanka <- data.frame(Density = -5:75)
pJanka <- within(pJanka, {
  constant  <- predict(m0a, pJanka)
  linear    <- predict(m1a, pJanka)
  quadratic <- predict(m2a, pJanka)
  cubic     <- predict(m3a, pJanka)
  quartic   <- predict(m4a, pJanka)
})

pJankaLong <- pJanka %>%
  pivot_longer(names_to="Model", values_to="Hardness", constant:quartic) %>%
  mutate(Model = factor(Model, levels = cs(constant, linear, quadratic,
                                           cubic, quartic)))

titlex <- expression("Model:"*'  '*italic(H) ==' '*italic(beta[0] +
    beta[1]*(D - alpha) + beta[2]*(D - alpha)^2 + cdots + epsilon))

p <- ggplot(janka) + aes(x = Density, y = Hardness) + geom_point()  +
  geom_line(aes(colour = Model), data = pJankaLong, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", size = 0.25) +
  geom_vline(xintercept = c(0,50), linetype = "solid",
             size = 0.25, colour = "blue") +
  annotate("text", x = c(0,50)-1, y = 3500, hjust = 1,
           label = paste("alpha ==", c(0,50)), parse = TRUE,
           size = 4) +
  labs(x = expression(italic(D):' Density'),
       y = expression(italic(H):' Hardness'),
       title = titlex) +
  theme(text = element_text(size = 12), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, vjust = 1, size = 12)) +
  scale_colour_brewer(palette = "Set1")
p


## ----c06,out.height="95%",fig.height=6,fig.width=6-----------------------------------------------------------------------
layout(matrix(1:4, 2, 2, byrow = TRUE))
plot(m2a)


## ----c07,fig.width=10,fig.height=5---------------------------------------------------------------------------------------
layout(matrix(1:2, 1))
box_cox(m1a)
title(main = "straight line model", line = 3)
box_cox(m2a)
title(main = "quadratic model", line = 3)


## ----c09,fig.width=9,fig.height=7----------------------------------------------------------------------------------------
m1 <- lm(sqrt(Hardness) ~ poly(Density, 1), janka)
m2 <- lm(log(Hardness) ~ poly(Density, 2), janka)
Janka <- within(janka, {
  r1 <- resid(m1);   r2 <- resid(m2)
  f1 <- fitted(m1);  f2 <- fitted(m2)
})
p0 <- ggplot(Janka)
p1 <- p0 + aes(x = f1, y = r1) + geom_point() +
  labs(x = "Fitted", y = "residuals", title = "sqrt - linear") +
  geom_smooth(se = FALSE, method = "loess", size = 0.5,formula = y~x)
p2 <- p0 + aes(x = f2, y = r2) + geom_point() +
  labs(x = "Fitted", y = "residuals", title = "log - quadratic") +
  geom_smooth(se = FALSE, method = "loess", size = 0.5, formula = y~x)
p3 <- p0 + aes(sample = r1) + stat_qq() + stat_qq_line()
p4 <- p0 + aes(sample = r2) + stat_qq() + stat_qq_line()

(p1 + p2)/(p3 + p4)


## ----c11-----------------------------------------------------------------------------------------------------------------
mGLM <- glm(Hardness ~ I(Density-50),
             family = Gamma(link = "sqrt"),
             data = janka)
mGLM2 <- update(mGLM, .~.+I((Density-50)^2))
round(summary(mGLM2)$coef, 4)


## ----c12,fig.width=9,fig.height=5----------------------------------------------------------------------------------------
rs <- resid(mGLM)
fv <- fitted(mGLM)
layout(matrix(1:2, 1))
plot(fv, rs, xlab = "fitted values", ylab = "residuals",
     pch = 20, col=grey(0.5), main = "Variance uniformity")
abline(h = 0, lty = "dashed", col="red"); grid()

qqnorm(rs, pch = 20, col = grey(0.5), xlab = "Normal scores",
       ylab = "sorted residuals", main = "Normal Q-Q plot")
qqline(rs, col="red", lty = "dashed"); grid()


## ----c14,eval=FALSE------------------------------------------------------------------------------------------------------
## ggplot(janka) + aes(x = Density, y = Hardness) +
##   geom_point() + scale_x_log10() + scale_y_log10() +
##   stat_smooth(method = "lm", size = 0.5, colour = "hot pink", formula = y~x)


## ----line_289_,echo=FALSE------------------------------------------------------------------------------------------------
ggplot(janka) + aes(x = Density, y = Hardness) +
  geom_point() + scale_x_log10() + scale_y_log10() +
  stat_smooth(method = "lm", size = 0.5, colour = "hot pink", formula = y~x)


## ----line_296_-----------------------------------------------------------------------------------------------------------
jankaLM <- lm(Hardness ~ poly(Density, 2), janka)
pJanka <- data.frame(Density = 24:70)
ext <- predict(jankaLM, pJanka, type = "resp", se.fit = TRUE) %>%
  as.data.frame() %>%
  within({
    lower <- fit - 2*se.fit
    upper <- fit + 2*se.fit
  })
pJanka <- cbind(pJanka, ext)
pLM <- ggplot(pJanka) + aes(x = Density) +
  geom_line(aes(y = fit), colour = "black") +
  geom_line(aes(y = lower), colour = "grey") +
  geom_line(aes(y = upper), colour = "grey") +
  geom_point(data = janka, aes(y = Hardness), colour = "red") +
  xlab("Density") + ylab("Hardness") + labs(title = "Naive quadratic")


## ----line_314_,fig.height=6,fig.width=12,out.height="80%"----------------------------------------------------------------
jankaGLM <- glm(Hardness ~ log(Density), Gamma(link = log), janka)
pJanka <- data.frame(Density = 24:70)
ext <- predict(jankaGLM, pJanka, type = "resp", se.fit = TRUE) %>%
  as.data.frame() %>%
  within({
    lower <- fit - 2*se.fit
    upper <- fit + 2*se.fit
  })
pJanka <- cbind(pJanka, ext)

pGLM <- ggplot(pJanka) + aes(x = Density) +
  geom_line(aes(y = fit), colour = "black") +
  geom_line(aes(y = lower), colour = "grey") +
  geom_line(aes(y = upper), colour = "grey") +
  geom_point(data = janka, aes(y = Hardness), colour = "red") +
  xlab("Density") + ylab("Hardness") + labs(title = "Gamma-log")
# 
# gridExtra::grid.arrange(pLM, pGLM, nrow = 1)
pLM + pGLM


## ----line_350_-----------------------------------------------------------------------------------------------------------
set.seed(20210202)
boot_sample <- function(data) data[sample(nrow(data), replace = TRUE), ]

ci <- replicate(500, {
  tmp <- update(jankaGLM, data = boot_sample(janka))  ## bootstrap data
  predict(tmp, pJanka, type = "resp")                 ## predictions for target
}) %>%
  apply(1, quantile, prob = c(0.025, 0.975))

pJanka <- pJanka %>%
  within({
    lowerBB <- ci[1, ]
    upperBB <- ci[2, ]
  })

greenish <- alpha("dark green", 0.5)
pGLM + labs(title = "Classical bootstrap") +
  geom_line(data = pJanka, aes(x = Density, y = lowerBB),
            colour = greenish, size = 0.5) +
  geom_line(data = pJanka, aes(x = Density, y = upperBB),
            colour = greenish, size = 0.5)



## ----cexp----------------------------------------------------------------------------------------------------------------
X <- matrix(sample(100, size = 1000*100, replace = TRUE), ncol = 100) %>%
  apply(1, tabulate, nbins = 100)
CB <- c(mean     = mean(colMeans(X)),
        variance = mean(colMeans((X-1)^2)))
W <- rexp(10000)
BB <- c(mean = mean(W), variance = var(W))
rbind(Classical = CB, Bayesian = BB)


## ----c20,cache=FALSE-----------------------------------------------------------------------------------------------------
set.seed(20210202)
Norm <- function(x) x/mean(x)

ci <- replicate(500, {
  tmp <- update(jankaGLM, weights = Norm(rexp(nrow(janka))))
  predict(tmp, pJanka, type = "resp")
}) %>%
  apply(1, quantile, prob = c(0.025, 0.975))

pJanka <- pJanka %>%
  within({
    lowerBB <- ci[1, ]
    upperBB <- ci[2, ]
  })

greenish <- alpha("dark green", 0.5)
pGLM + labs(title = "Bayesian bootstrap") +
  geom_line(data = pJanka, aes(x = Density, y = lowerBB),
            colour = greenish, size = 0.5) +
  geom_line(data = pJanka, aes(x = Density, y = upperBB),
            colour = greenish, size = 0.5)


## ----recent,fig.show="hold"----------------------------------------------------------------------------------------------
Janka <- within(Janka2012, {
  Type <- ifelse(grepl("Eucalyptus", Binomial), "Eucalpyt", "Other") 
})

jp <- ggplot(Janka) + aes(x = Density, y = Hardness) + geom_point() +
  aes(colour = Type) + # coord_trans("log10", "log10") +
  scale_x_log10() + scale_y_log10() +
  scale_colour_brewer(palette = "Set1", name = "Genus")
jp
jp  +  ## identify the light species
  ggrepel::geom_text_repel(data = filter(Janka, Density < 600),
                           aes(label = Name), size = 3,
                           colour = greenish,  ## some transparency
                           fontface = "bold", family = "serif") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, size = 0.5)


## ----recent2,results="asis"----------------------------------------------------------------------------------------------
oldJanka <- glm(Hardness~log(Density), Gamma(link="log"), janka)
newJanka <- update(oldJanka, data = Janka)
format(cbind(old = coef(oldJanka),
             recent = coef(newJanka)),
       digits = 5) %>% booktabs()


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo(), locale = FALSE)

