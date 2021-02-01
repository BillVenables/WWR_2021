## ----"prelim",child="00-Prelim.Rnw"------------------------------------------------------------------------------

## ----prelim,include=FALSE,tidy=FALSE-----------------------------------------------------------------------------



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






## ----setFigPath,include=FALSE------------------------------------------------------------------------------------
.infile <- sub("\\.Rnw$", "", knitr::current_input())
knitr::opts_chunk$set(fig.path = paste0('Fig/', .infile, "_")) #$
session <- sub("^0+", "", sub("[^[:digit:][:alpha:]].*$", "", .infile))


## ----eng,echo=FALSE----------------------------------------------------------------------------------------------
library(english)


## ----cc1---------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(WWRCourse))
CCf <- system.file("extdata", "creditCard.csv", package = "WWRCourse")
CC <- read.csv(CCf, na.strings="", stringsAsFactors = TRUE)
row.names(CC) <- fill0(CC$ident)

with(CC, c(cases = nrow(CC), known = sum(!is.na(credit.card.owner))))


## ----cc2---------------------------------------------------------------------------------------------------------
CC <- CC %>% within({
  p2 <- c("doctor", "engineer", "lawyer", "professor", "business")
  p1 <- c("teacher", "police", "service", "chemist", "nurse",
          "postman", "physical")
  p0 <- "none"
  pclass <- rep("", length(profession))
  pclass[profession %in% p0] <- "p0"
  pclass[profession %in% p1] <- "p1"
  pclass[profession %in% p2] <- "p2"
  pclass <- factor(pclass, levels = paste0("p", 0:2))
  swiss <- ifelse(nationality == "CH", "Swiss", "Foreigner")
  sex <- factor(sex)
  swiss <- factor(swiss)
  pclass <- factor(pclass)
  ident <- profession <- nationality <- p0 <- p1 <- p2 <- NULL
})
CCKnown <- na.omit(CC)
dim(CCKnown)


## ----cc3---------------------------------------------------------------------------------------------------------
set.seed(20200202)    ## for reproducibility
train <- sample(nrow(CCKnown), nrow(CCKnown)/2)

CCTrain <- CCKnown[train, ]
CCTest <- CCKnown[-train, ]


## ----t3,fig.height=6 * 1.5,fig.width=8 * 1.5,fig.show="hold"-----------------------------------------------------
requireData(rpart)
CCTree <- rpart(credit.card.owner ~ ., CCTrain)

## The first graphic shows the initial fitted tree:

plot(CCTree)
text(CCTree, xpd = NA)

## The next graphic is to check for the need to prune:

plotcp(CCTree)


## ----t5,fig.height=6 * 1.5,fig.width=8 * 1.5---------------------------------------------------------------------
oneSERule(CCTree)               ## optimal tuning paramater
CCPTree <- prune(CCTree, cp = oneSERule(CCTree))

plot(CCPTree)             ## should have 10 terminal nodes
text(CCPTree, xpd = NA)


## ----oneSE-------------------------------------------------------------------------------------------------------
WWRCourse::oneSERule         ## Generic function
methods("oneSERule")         %>% format() %>% noquote() ## some suppression
WWRCourse:::oneSERule.rpart  ## Registered method


## ----tbag--------------------------------------------------------------------------------------------------------
bagRpart <- local({
  bsample <- function(dataFrame) # bootstrap sampling
    dataFrame[sample(nrow(dataFrame), rep = TRUE),  ]
  function(object, data = eval.parent(object$call$data),
           nBags=200, type = c("standard", "bayesian"), ...) {
    type <- match.arg(type)
    bagsFull <- vector("list", nBags)
    if(type == "standard") {
      for(j in 1:nBags)
          bagsFull[[j]] <- update(object, data = bsample(data))
      } else {
        nCases <- nrow(data)
        for(j in 1:nBags)
            bagsFull[[j]] <- update(object, data = data, weights = rexp(nCases))
        }
    class(bagsFull) <- "bagRpart"
    bagsFull
  }
})

## a prediction method for the objects (somewhat tricky!)
predict.bagRpart <- function(object, newdata, ...) {
  X <- sapply(object, predict, newdata = newdata, type = "class")
  candidates <- levels(predict(object[[1]], type = "class"))
  X <- t(apply(X, 1, function(r) table(factor(r, levels = candidates))))
  factor(candidates[max.col(X)], levels = candidates)
}
.S3method("predict", "bagRpart")  ## >= R 4.0.0
Store(bagRpart, predict.bagRpart, lib = .Robjects)


## ----tbagsAlt,eval=FALSE-----------------------------------------------------------------------------------------
## bagRpart <- local({
## ###
##   bsample <- function(dataFrame) # bootstrap sampling
##     dataFrame[sample(nrow(dataFrame), rep = TRUE),  ]
## ###
##   function(object, data = eval.parent(object$call$data),
##            nBags=200, type = c("standard", "bayesian"), ...) {
##     type <- match.arg(type)
##     bagsFull <- if(type == "standard") {
##       replicate(nBags, update(object, data = bsample(data)),
##                 simplify = FALSE)
##     } else {
##       nCases <- nrow(data)
##       replicate(nBags,  update(object, data = data, weights = rexp(nCases)),
##                 simplify = FALSE)
##     }
##     class(bagsFull) <- "bagRpart"
##     bagsFull
##   }
## })


## ----tbag2,cache=TRUE--------------------------------------------------------------------------------------------
set.seed(4321) ## 
Obj <- update(CCTree, cp = 0.005, minsplit = 9)  ## expand the tree
(one <- rbind(standard = system.time(CCSBag <- bagRpart(Obj, nBags = 200)),
              Bayes = system.time(CCBBag <- bagRpart(Obj, nBags = 200,
                                                     type = "bayes"))))

## ----include=FALSE-----------------------------------------------------------------------------------------------
  if("package:MASS" %in% search()) detach("package:MASS")


## ----para1-------------------------------------------------------------------------------------------------------
## parallel backend; includes other pkgs
suppressPackageStartupMessages({
  library(doParallel)
})

(nc <- detectCores())  ## how many CPUs has your computer?

cl <- makeCluster(nc-1)
registerDoParallel(cl)


## ----para2-------------------------------------------------------------------------------------------------------
bagRpartParallel <- local({
  bsample <- function(dataFrame) # bootstrap sampling
    dataFrame[sample(nrow(dataFrame), rep = TRUE),  ]
  
  function(object, data = eval.parent(object$call$data),
           nBags = 200, type = c("standard", "bayesian"), ...,
           cores = detectCores() - 1, seed0 = as.numeric(Sys.Date())) {
    type <- match.arg(type)
    bagsFull <- foreach(j = idiv(nBags, chunks=cores), seed = seed0+seq(cores),
                        .combine = c, .packages = c("rpart", "stats"), 
                        .inorder = FALSE, .export = c("bsample")) %dopar% {
                          ## now inside a single core
                          set.seed(seed = seed)
                          if(type == "standard") {
                            replicate(j, simplify = FALSE, 
                                      update(object, data = bsample(data)))  
                          } else {
                            replicate(j, simplify = FALSE,
                                      update(object, data = data,
                                             weights = rexp(nrow(data))))
                          }
                          ## end of single core mode
                        }
    class(bagsFull) <- "bagRpart"
    bagsFull
  }
})


## ----para3,cache=TRUE--------------------------------------------------------------------------------------------
Obj <- update(CCTree, cp = 0.005, minsplit = 9)  ## expand the tree
rbind(one,
      standardP = system.time(CCSBagP <- bagRpartParallel(Obj, nBags = 200)),
      BayesP = system.time(CCBBagP    <- bagRpartParallel(Obj, nBags = 200, 
                                                          type = "bayes")))
rm(Obj, one)

## ----include=FALSE-----------------------------------------------------------------------------------------------
  if("package:MASS" %in% search()) detach("package:MASS")


## ----line_377_, include=FALSE------------------------------------------------------------------------------------
stopCluster(cl)  ## release resources
rm(cl)


## ----trf1--------------------------------------------------------------------------------------------------------
n <- nrow(CCTrain)
X <- replicate(200,
       table(factor(sample(n, rep=TRUE), levels = 1:n)))
(lims <- range(rowSums(X > 0)))
rm(n, X)


## ----trf2--------------------------------------------------------------------------------------------------------
requireData(randomForest)
(CCRf <- randomForest(credit.card.owner ~ ., CCTrain, ntree = 200))


## ----oob,include=FALSE-------------------------------------------------------------------------------------------
OOB <- function(obj, ...) UseMethod("OOB")
OOB.default <- function(obj, ...)
    stop(sprintf("No OOB method for objects of class %s currently exists.",
                 dQuote(class(obj))))
OOB.randomForest <- function(obj, ...)
    with(obj, {
      here <- with(sys.status(), max(sys.parents))
      if(type == "classification" && exists("confusion", frame = here))
          err.rate[ntree, "OOB"] else NA
    })
.S3method("OOB", "default")
.S3method("OOB", "randomForest")
Store(OOB, OOB.default, OOB.randomForest, lib = .Robjects)


## ----err---------------------------------------------------------------------------------------------------------
err <- as.data.frame(CCRf$err.rate) %>% 
  rownames_to_column("trees") %>% 
  mutate(trees = as.numeric(trees)) %>% 
  pivot_longer(cols = -trees, names_to = "Which", values_to = "Error")
ggplot(err) + aes(x = trees, y = Error, colour = Which) + geom_line() +
  scale_colour_brewer(palette = "Dark2", name = "Which one:") +
  ylim(0, max(err$Error)*1.05) + labs(title = "Credit Card - Training Data")  +
  guides(colour = guide_legend(ncol = 3)) +
  theme_bw() + theme(legend.position = c(0, 0)+0.01,
                     legend.justification = c("left", "bottom"),
                     plot.title = element_text(hjust = 0.5, face = "bold"))


## ----rfplot, eval=FALSE------------------------------------------------------------------------------------------
## ER <- CCRf$err.rate
## plot(CCRf, ylim = range(0, ER)*1.05, lty = "solid", las = 1,
##      panel.first = grid(lty = "dashed"), main = "Credit Card - Training Data")
## legend("bottomleft", colnames(ER), ncol = ncol(ER), bg = "white",
##        col = 1:3, lty = "solid", bty = "o", box.col = "transparent",
##        title = "Which one:")


## ----echo=FALSE--------------------------------------------------------------------------------------------------
pal <- palette()
pal[3] <- "dark green"
palette(pal)
ER <- CCRf$err.rate
plot(CCRf, ylim = range(0, ER)*1.05, lty = "solid", las = 1,
     panel.first = grid(lty = "dashed"), main = "Credit Card - Training Data")  
legend("bottomleft", colnames(ER), ncol = ncol(ER), bg = "white",
       col = 1:3, lty = "solid", bty = "o", box.col = "transparent",
       title = "Which one:")


## ----trf3--------------------------------------------------------------------------------------------------------
par(family="sans")
varImpPlot(CCRf, pch = 20, col = "navy")  ## causes a plot
(v <- sort(drop(importance(CCRf)), decreasing = TRUE))[1:6]
best_few <- names(v)[1:20] %>% print ## used later


## ----trfpp-------------------------------------------------------------------------------------------------------
partialPlot(CCRf, pred.data = CCTrain, x.var = best_few[1], xlab = best_few[1])


## ----glm0--------------------------------------------------------------------------------------------------------
(form <- as.formula(paste("credit.card.owner~",
                         paste(best_few, collapse="+"))))

Call <- substitute(glm(FORM, binomial, CCTrain), list(FORM = form))
CCGlmNaive <- eval(Call)

## ----g1,cache=TRUE-----------------------------------------------------------------------------------------------

upp <- paste("~", paste(setdiff(names(CCTrain),
                                "credit.card.owner"),
                        collapse="+")) %>% 
  as.formula()
scope <- list(upper=upp, lower=~1)
CCGlmAIC <- step_AIC(CCGlmNaive, scope = scope)
CCGlmGIC <- step_GIC(CCGlmNaive, scope = scope)
CCGlmBIC <- step_BIC(CCGlmNaive, scope = scope)

## ----include=FALSE-----------------------------------------------------------------------------------------------
  if("package:MASS" %in% search()) detach("package:MASS")


## ----line_511_,include=FALSE-------------------------------------------------------------------------------------
form <- as.formula(paste("credit.card.owner~",
                         paste(best_few, collapse="+")))

## ----boost,cache=TRUE--------------------------------------------------------------------------------------------
requireData(mboost)
Call <- substitute(glmboost(FORM, data = CCTrain, family=Binomial()),
                   list(FORM = form))  ## same as naive model
CCglmboost <- eval(Call)

  ## more packages needed!
CCblackboost <- blackboost(credit.card.owner ~ ., CCTrain, 
                           family = Binomial())


## ----quin--------------------------------------------------------------------------------------------------------
requireData(C50)  ## NB C-5-Zero
(CCc50 <- C5.0(credit.card.owner ~ ., CCTrain))


## ----quin2,fig.show="hold"---------------------------------------------------------------------------------------
obj <- C5imp(CCc50)
data.frame(importance = obj$Overall, variable = rownames(obj)) %>% 
  filter(importance > 0) %>% 
  arrange(importance) %>% 
  with(., dotchart(importance, as.character(variable), pch = 20, col = "navy"))
rm(obj)


## ----------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(gbm))
(CCgbm <- gbm(ifelse(credit.card.owner == "no", 0, 1) ~ ., data = CCTrain, 
              distribution = "bernoulli", n.trees = 200, cv.folds = 10))


## ----out.lines = 15----------------------------------------------------------------------------------------------
summary(CCgbm, plotit = FALSE) %>% filter(rel.inf > 0)


## ----out.height="75%"--------------------------------------------------------------------------------------------
par(mar = c(4,12,1,1), cex = 0.7)
tmp <- summary(CCgbm)  ## tmp is now the data frame displayed above.


## ----------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(gridExtra))

(vars <- tmp$var[1:4])                   ## four "most influential" variables
plots <- lapply(vars, . %>% plot(CCgbm, i.var = ., type = "response"))
grobs <- arrangeGrob(grobs = plots, layout_matrix = rbind(1:2, 3:4))

plot(grobs)


## ----Class-------------------------------------------------------------------------------------------------------
Class <- function(object, newdata, ...)
    UseMethod("Class")

Class.rpart <- function(object, newdata, ...)
    predict(object, newdata, type = "class")

Class.bagRpart <- function(object, newdata, ...)
    predict(object, newdata)
Class.randomForest <- Class.C5.0 <- predict

Class.glm <- Class.mboost <- Class.gbm <- function(object, newdata, ...) {
  ## only applies for binomial GLMs with symmetric links
  suppressMessages(predict(object, newdata) > 0)
}
for(k in sub("Class\\.", "", ls(pattern = "^Class\\."))) .S3method("Class", k)
Store(list = ls(pattern = "^Class"), lib = .Robjects)


## ----errors------------------------------------------------------------------------------------------------------
errorRate <- function(tab) 100*(1 - sum(diag(tab))/sum(tab))
true <- CCTest$credit.card.owner  #$
(res <- sort(sapply(list(Tree = CCTree,            PrunedTree = CCPTree,
                         SimpleBagging = CCSBag,   BayesBagging = CCBBag,
                         SimpleBaggingP = CCSBagP, BayesBaggingP = CCBBagP,
                         RandomForest = CCRf,      C5.0 = CCc50, 
                         GradientBoosting = CCgbm, BoostedGlm = CCglmboost,
                         NaiveGLM = CCGlmNaive,    BoostedTree = CCblackboost, 
                         Glm_AIC = CCGlmAIC,       GlmGIC = CCGlmGIC, 
                         Glm_BIC = CCGlmBIC),
                    function(x) errorRate(table(Class(x, CCTest), true)))))


## ----err2,out.height="80%"---------------------------------------------------------------------------------------
par(mar = c(3,8,3,1))
barplot(rev(res), horiz=TRUE, las=1, fill = pal_green2brown)
axis(3)


## ----sessionInfo,echo=FALSE,results="asis",out.lines=200---------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo(), locale = FALSE)

