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


## ----line_057_,include=FALSE---------------------------------------------------------------------------------------------
library(ggthemes)
oldOpt <- options(digits = 4)
theme_set(theme_bw())
knitr::opts_chunk$set(warning = TRUE)


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
library(parallel)


## ----results="hide", eval=FALSE------------------------------------------------------------------------------------------
## library(parallel)
## ?clusterApply


## ------------------------------------------------------------------------------------------------------------------------
nam <- c(setdiff(names(BirthWt), "low"), 
         "poly(age,2)", "smoke*ui", "age*ftv", "poly(lwt,2)")
g <- (2 + log(nrow(BirthWt)))/2  ## "Goldilocks" penalty

all_forms <- "low~1"
for(n in nam) {
  all_forms <- c(all_forms, paste(all_forms, n, sep = "+"))
}
all_forms <- sub("1\\+", "", all_forms)
all_forms[1:8]
length(all_forms)


## ------------------------------------------------------------------------------------------------------------------------
t1 <- system.time({
  res <- vapply(all_forms, function(form) {
    thisCall <- substitute(glm(FORM, binomial, BirthWt),
                           list(FORM = as.formula(form)))
    thisModel <- eval(thisCall)
    thisLogL <- logLik(thisModel)
    c(AIC = AIC(thisModel),
      BIC = BIC(thisModel),
      GIC = AIC(thisModel, k = g),
      logL = as.vector(thisLogL),
      DF = as.integer(attr(thisLogL, "df")))
  }, FUN.VALUE = c(AIC = 0.0, BIC = 0.0, GIC = 0.0,
                   logL = 0.0, DF = 0L))
})
t1  ## how long did that take?


## ------------------------------------------------------------------------------------------------------------------------
cl <- makePSOCKcluster(max(1, detectCores()-1))     ## set up cores-1 machines
clusterEvalQ(cl, library(WWRData)) %>% invisible()
clusterExport(cl, "g")                              ## the all have "g" 

t2 <- system.time({                                 ## farm out the jobs
  resP <- parSapply(cl, all_forms, function(form) {
    thisCall <- substitute(glm(FORM, binomial, BirthWt),
                           list(FORM = as.formula(form)))
    thisModel <- eval(thisCall)
    thisLogL <- logLik(thisModel)
    c(AIC = AIC(thisModel),
      BIC = BIC(thisModel),
      GIC = AIC(thisModel, k = g),
      logL = as.vector(thisLogL),
      DF = as.integer(attr(thisLogL, "df")))
  })
})
stopCluster(cl)
rm(cl)


## ------------------------------------------------------------------------------------------------------------------------
all.equal(res, resP)

rbind(t1, t2)[, c("user.self", "sys.self", "elapsed")]


## ------------------------------------------------------------------------------------------------------------------------
results <- data.frame(formula = all_forms, t(res), 
                      stringsAsFactors = FALSE) %>% 
  arrange(logL, DF) %>% 
  filter(!(c(1, diff(logL)) < 1e-6 & c(1, diff(DF)) == 0))

best10_AIC <- results %>% arrange(AIC) %>% filter(AIC < AIC[11])
best10_BIC <- results %>% arrange(BIC) %>% filter(BIC < BIC[11])
best10_GIC <- results %>% arrange(GIC) %>% filter(GIC < GIC[11])

list(AIC = as.formula(best10_AIC$formula[1]),
     BIC = as.formula(best10_BIC$formula[1]),
     GIC = as.formula(best10_GIC$formula[1]))


## ------------------------------------------------------------------------------------------------------------------------
BW0 <- glm(low ~ ., binomial, BirthWt)
scope <- list(lower = ~1, upper = ~.^2+poly(age, 2)+poly(lwt,2))
list(AIC = step_AIC(BW0, scope = scope),
     BIC = step_BIC(BW0, scope = scope),
     GIC = step_AIC(BW0, scope = scope, k = g)) %>% 
       lapply(formula)


## ----it1,out.lines=6-----------------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(doParallel)
})
search()


## ----line_124_-----------------------------------------------------------------------------------------------------------
X <- matrix(1:20, 5, 4) %>% print
itXrows <- iter(X, by = "row")
Xt <- foreach(x = itXrows, .combine = cbind) %do% rev(x) ## NOT parallel yet!
Xt


## ----it2,fig.height=5,fig.width=7.5,out.height="0.5\\textheight"---------------------------------------------------------
hp_model <- lm(log(medv) ~ ., Boston)
rdat  <- data.frame(res = resid(hp_model))
ggplot(rdat) + aes(sample = res) + geom_qq() + 
  labs(y = "residuals", x = "normal scores",
       title = "Boston House Price Data")


## ----line_159_-----------------------------------------------------------------------------------------------------------
simulate              ## An S3 generic function in package:stats
methods("simulate")   ## for what kinds of model does it cater?
########################  initialisation
library(doParallel)
cores <- detectCores() %>% print
cl <- makeCluster(cores - 1)
registerDoParallel(cl)         ## all set to go


## ----para1,cache=TRUE----------------------------------------------------------------------------------------------------
N <- 10000                         ## Number of simulations, go big time
chunks <- idiv(N, chunks = cores - 1)  ## farm one bit out to each core?
QR <- hp_model$qr
Res <- foreach(nsim = chunks,     ## fed out by the iterator.
               .combine = cbind,
               .inorder = FALSE,  ## allows load balancing
               .export = "Boston") %dopar% {
                 Sims <- simulate(hp_model, nsim = nsim)
                 Sims <- qr.resid(QR, as.matrix(Sims))

                 ## line below equivalent to: apply(Sims, 2, sort)
                 matrix(Sims[order(col(Sims), Sims)], nrow = nrow(Sims))
               }
itRes <- iter(Res, by = "row")
Res <- foreach(r = itRes, .combine = rbind) %dopar%
  ## quantile(r, prob = c(0.025, 0.975))  ## too stringent?
  sort(r)[c(50, length(r)-49)]              ## just clip off the fuzz

##################### finalisation
stopCluster(cl)
rm(cl)


## ----para2---------------------------------------------------------------------------------------------------------------
colnames(Res) <- c("Low", "Upp")
rdat <- cbind(rdat, Res) %>%
  within({
    ns <- qnorm(ppoints(length(res)))  ## normal scores
    res <- sort(res)
    range <- ifelse(res <  Low, "low", ifelse(res > Upp, "high", "normal"))
    range <- factor(range, levels = c("low", "normal", "high"))
  })

### show the results
ggplot(rdat) + aes(x = ns, y = res, colour = range) + geom_point() +
  geom_step(aes(y = Low), direction = "hv", col = "grey") +
  geom_step(aes(y = Upp), direction = "vh", col = "grey") +
  labs(x="Normal scores", y="Sorted residuald",
  title="Parametric Bootstrap Envelope for Boston HP Data Residuals") +
  theme(legend.position = c(0.1, 0.9)) +
  scale_colour_brewer(palette = "Set1")


## ----line_334_,out.lines=6-----------------------------------------------------------------------------------------------
fname <- system.file("extdata", "churnData.csv.gz", package = "WWRCourse")
churnData <- read_csv(gzfile(fname),     ## neater read (for eventual notebook)
                      col_types = cols(.default = col_double(),
                                       state = col_character(),
                                       area_code = col_character(),
                                       international_plan = col_character(),
                                       voice_mail_plan = col_character(),
                                       churn = col_character(),
                                       sample = col_character())) %>%
  unclass() %>% data.frame(stringsAsFactors = TRUE) %>% select(-sample) 
names(churnData) %>% noquote()


## ----line_349_-----------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(randomForest))
rfm <- randomForest(churn ~ ., churnData)
rfm
## note how redundant 'charge' and 'minutes'variables get level pegging
varImpPlot(rfm)


## ----line_359_-----------------------------------------------------------------------------------------------------------
cl <- makeCluster(cores - 2)
registerDoParallel(cl)         ## all set to go
ntree <- idiv(5000, chunks = max(5, cores - 2))
t1 <- system.time({
  bigRf <- foreach(ntree = ntree,
                   .combine = randomForest::combine, ## one in dplyr!
                   .packages = "randomForest",
                   .inorder = FALSE) %dopar% {
                     randomForest(churn ~ ., churnData, ntree = ntree)
                   }
})
stopCluster(cl)
rm(cl)
varImpPlot(bigRf)


## ----comb_tree-----------------------------------------------------------------------------------------------------------
ntree <- idiv(5000, chunks = max(5, cores - 2))
t2 <- system.time({
  bigRf <- foreach(ntree = ntree,
                   .combine = randomForest::combine, ## one in dplyr!
                   .packages = "randomForest",
                   .inorder = FALSE) %do% {       ### <<<--- only change!
                     randomForest(churn ~ ., churnData, ntree = ntree)
                   }
})
rbind(serial = t2, parallel = t1)[, c("user.self", "sys.self", "elapsed")]


## ----tbag1---------------------------------------------------------------------------------------------------------------
bagRpart <- local({
  bsample <- function(dataFrame) # bootstrap sampling
    dataFrame[sample(nrow(dataFrame), rep = TRUE),  ]
  function(object, data = eval.parent(object$call$data),
           nBags=600, type = c("standard", "bayesian"), ...) {
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


## ----bagg1,fig.height=3,fig.width=4.5,out.height="3.5in"-----------------------------------------------------------------
library(doParallel) ## parallel backend; includes other pkgs
(cores <- detectCores())  ## how many real cores?

cl <- makeCluster(cores-1)
registerDoParallel(cl)


## ----bagg2---------------------------------------------------------------------------------------------------------------
bagRpartParallel <- local({
  bsample <- function(dataFrame) # bootstrap sampling
    dataFrame[sample(nrow(dataFrame), rep = TRUE),  ]
  
  function(object, data = eval.parent(object$call$data),
           nBags = 600, type = c("standard", "bayesian"), ...,
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


## ----bagg3,cache=TRUE----------------------------------------------------------------------------------------------------
set.seed(12345) ##
library(rpart)
Obj <- rpart(medv ~ ., Boston, minsplit = 5, cp = 0.005)  ## expand the tree
rbind(
  simple    = system.time(HPSBag  <- bagRpart(Obj, nBags=600)),
  bayes     = system.time(HPBBag  <- bagRpart(Obj, nBags=600, type="bayes")),
  parallel_s= system.time(HPSBagP <- bagRpartParallel(Obj, nBags=600)),
  parallel_b= system.time(HPBBagP <- bagRpartParallel(Obj, nBags=600,
                                                   type="bayes")))[, 
                                  c("user.self", "sys.self", "elapsed")]
rm(Obj)

## ----bagg3a--------------------------------------------------------------------------------------------------------------
stopCluster(cl)  ## release resources
rm(cl)


## ------------------------------------------------------------------------------------------------------------------------
predict.bagRpart <- function(object, newdata, ...,
                             method = object[[1]]$method) {
  switch(method,
         class = {
           X <- sapply(object, predict, newdata = newdata, type = "class")
           candidates <- levels(predict(object[[1]], type = "class"))
           X <- t(apply(X, 1, function(r) {
             table(factor(r, levels = candidates))
           }))
           factor(candidates[max.col(X)], levels = candidates)
         },
         anova = {
           X <- sapply(object, predict, newdata = newdata, type = "vector")
           rowMeans(X)
         },
         NA)
}



## ----sessionInfo,echo=FALSE,results="asis",out.lines=200-----------------------------------------------------------------
cat("{\\bf\nDate: ", format(today()), "\n}")
toLatex(sessionInfo(), locale = FALSE)

