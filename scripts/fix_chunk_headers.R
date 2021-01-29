
make_labels <- local({

  fix_name <- function(lst) {
    if(is.null(names(lst))) {
      if(length(lst) == 1) return(quote(list(label = "")))
      if(length(lst) == 2) return(setNames(lst, c("", "label")))
    } else {
      if(!("label" %in% names(lst))) {
        if("" %in% names(lst)[-1]) {
          k <- which(names(lst) == "")[-1]
          names(lst)[k] <- "label"
        } else {
          lst$label <- ""
        }
      }
    }
    lab <- which(names(lst) == "label")
    oth <- setdiff(seq_along(lst), c(1, lab))
    lst[c(1, lab, oth)]
  }

  chunk_pattern <- "^<<line_[[:digit:]]+_[,]?"

  function(chunk_headers, line_nos) {
    lheads <- sub(chunk_pattern, "<<", chunk_headers)  ## remove previously inserted labels
    lheads <- sub("^<<(.*)>>=$", "list(\\1)", lheads)
    lheads <- lapply(lheads, function(x) fix_name(parse(text = x)[[1]]))
    dnames <- paste0("line_", gsub(" ", "0", format(line_nos)), "_")
    vacant <- sapply(lheads, function(x) x$label == "")
    if(any(vacant)) {
      for(i in which(vacant))
        lheads[[i]]$label <- as.name(dnames[[i]])
    }
    lheads <- lapply(lheads, function(x) {
      names(x)[names(x) == "label"] <- ""
      x
    })
    heads <- sapply(lheads, deparse, width.cutoff = 500)
    heads <- sub("^list\\((.*)\\)$", "<<\\1>>=", heads)
    gsub(" = ", "=", gsub(", ", ",", heads))
  }
})

fix_labels <- function(txt) {
  line_nos <- grep("^<<.*>>=$", txt)
  if(length(line_nos) == 0) return(txt)
  txt[line_nos] <- make_labels(txt[line_nos], line_nos)
  txt
}

make_script <- function(Rnw = dir(directory, pattern = "\\.Rnw$", full.names = TRUE),
                        directory = ".", backup = "./.backup") {
  if(length(Rnw) == 0) return("## No .Rnw files")
  init <- substitute({
    if(!dir.exists(BAKCUP))
      dir.create(BAKCUP)
  }, list(BAKCUP = backup))
  script <- capture.output(init)[2:3]

  for(f in Rnw) {
    thisFile <- substitute({
      file.copy(FILE, BACKUP, overwrite = TRUE)
      txt <- fix_labels(readLines(FILE))
      cat(txt, file = FILEC, sep = "\n")
    }, list(FILE = f, FILEC = basename(f), BACKUP = backup))

    script <- c(script, capture.output({
      cat("## ----\n")
      print(thisFile)
    }))
  }
  grep("^[{}]$", script, invert = TRUE, value = TRUE)
}


