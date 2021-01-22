
###
### Working with R: Package installations 2021
###

if(!with(base::version, major >= 4 && minor >= 0)) {
  cat(" You have R version ", with(base::version, paste(major, minor, sep = ".")), "\n",
      "You need to update your R first to a more recent version,\n",
      "preferably R 4.0.3, (or later).\n")
} else {
  # 
  # favourite_CRAN_mirror <- "https://cloud.r-project.org"  ## or whatever one you use.
  oldOpt <- options()
  my_lib <- .libPaths()[1]                             ## packages go here

  .ipkgs <- rownames(installed.packages())  ## what do you have already?
  
  if(!("tools:rstudio" %in% search() || any(grepl("^rstudio", .ipkgs)))) {
    cat(" You appear not to have, or not to be using, RStudio.\n",
        "The workshop will use RStudio in many sessions.\n",
        "We recommend you install the latest preview version.\n")
  }
  
  options(repos = c(CRAN = "https://cloud.r-project.org", 
                    BioCsoft = "https://bioconductor.org/packages/3.11/bioc"))
  
  if(.Platform$OS.type == "windows") {
    options(pkgType = "win.binary")
  } else {
    options(Ncpus = max(1, parallel::detectCores() - 2))
  }
  
  if(file.exists(my_lib) && (file.access(my_lib, 2) == 0)) { ## Looks OK
    
    
    if(("curl" %in% .ipkgs) && !curl::has_internet()) {
      cat("Oops!  You don't appear not to have an internet connection!")
    } else {
      if(!is.null(old.packages())) {
        cat(" Some of your packages have newer versions available.  Update?\n",
            "Use\n\tupdate.packages(ask = FALSE, checkBuilt = TRUE)\n")
      }
      
      
      # Bill's original packages not including R Shiny related packages
      
      # .pkgs <- c("C50",            "GGally",         "MASSExtra",      "Rcpp",           
      #            "SOAR",           "devtools",       "data.table",     "doParallel",     
      #            "doRNG",          "english",        "fractional",     "gbm",           
      #            "ggrepel",        "ggthemes",       "gridExtra",      "knitr",          
      #            "lazyData",       "lme4",           "mboost",         "microbenchmark", 
      #            "mlbench",        "patchwork",      "pingr",          "randomForest", 
      #            "rbenchmark",     "scales",         "styler",         "sudokuAlt",  
      #            "tidyverse",      "visreg",         "xtable")  
      
      # Bill's original packages plus Rhetta's R Shiny related packages (take 1)
      # .pkgs <- c("C50",               "GGally",         "MASSExtra",    "Rcpp",           
      #            "SOAR",              "devtools",       "data.table",   "doParallel",     
      #            "doRNG",             "DT",             "english",      "fractional",     
      #            "gbm",               "ggrepel",        "ggthemes",     "golem",
      #            "gridExtra",         "knitr",          "lazyData",     "leaflet",
      #            "leaflet.providers", "lme4",           "mboost",       "microbenchmark",     
      #            "mlbench",           "patchwork",      "plotly",       "pingr",              
      #            "plumbr",            "randomForest",   "rbenchmark",   "rgdal",
      #            "scales",            "shiny",          "shinyAce",     "shinydashboard",
      #            "shinydashboardPlus","shinyEffects",   "shinyjqui",    "shiny.semantic",
      #            "shinythemes",        "shinyWidgets",  "styler",       "sudokuAlt", 
      #            "tidyverse",          "visreg",        "xtable", "shinycssloaders")   
      
      .pkgs <- c("C50",                "data.table",         "devtools",           "doParallel",        
                 "doRNG",              "DT",                 "english",            "fractional",        
                 "gbm",                "GGally",             "ggrepel",            "ggthemes",          
                 "golem",              "gridExtra",          "knitr",              "lazyData",          
                 "leaflet",            "leaflet.providers",  "lme4",               "MASSExtra",         
                 "mboost",             "microbenchmark",     "mlbench",            "patchwork",         
                 "pingr",              "plotly",             "plumbr",             "randomForest",      
                 "rbenchmark",         "Rcpp",               "rgdal",              "scales",            
                 "shiny",              "shiny.semantic",     "shinyAce",           "shinycssloaders",   
                 "shinydashboard",     "shinydashboardPlus", "shinyEffects",       "shinyjqui",         
                 "shinythemes",        "shinyWidgets",       "SOAR",               "styler",            
                 "sudokuAlt",          "tidyverse",          "visreg",             "xtable")
      
      .apkgs <- rownames(available.packages())
      .githubs <- c("searchpath", 
                    paste0("WWR", c("Data", "Graphics", "Utilities", "Course")))
      
      .extras <- setdiff(.pkgs, .ipkgs)
      .misspk <- setdiff(.extras, .apkgs)
      .githubs <- setdiff(.githubs, .ipkgs)
      
      if(length(.misspk) > 0) {
        cat("\nThe following packages will need to be manually installed:\n",
            paste(.misspk, sep = "\n"), "\n")
        .extras <- intersect(.extras, .apkgs)
      }
      if(length(.extras) > 0 || length(.githubs) > 0) {
        if(length(.extras > 0)) {
          install.packages(.extras, lib = my_lib)
        }
        if(length(.githubs) > 0) {
          .githubs <- paste0("BillVenables/", .githubs)
          remotes::install_github(.githubs, upgrade = FALSE, lib = my_lib,
                                  build_vignettes = FALSE)
        }
      } else {
        cat("All installed.  No other packages needed.\n")
      }
      
      rm(.pkgs, .extras, .apkgs, .misspk, .githubs)
      
    }
  } else {    ## no personal library or inadequate permissions
    cat("You appear not to have the write capability\n",
        "needed to install or update packages!\n")
  }
  options(oldOpt)
  rm(oldOpt, my_lib, .ipkgs)
}

###
###
###
