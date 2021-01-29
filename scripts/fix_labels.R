    if (!dir.exists("./.backup")) 
        dir.create("./.backup")
## ----
    file.copy("RnwFiles/001_Introduction.Rnw", "./.backup", overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/001_Introduction.Rnw"))
    cat(txt, file = "001_Introduction.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/002_Objects_and_Manipulation.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/002_Objects_and_Manipulation.Rnw"))
    cat(txt, file = "002_Objects_and_Manipulation.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/003_Graphics_1.Rnw", "./.backup", overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/003_Graphics_1.Rnw"))
    cat(txt, file = "003_Graphics_1.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/004_Two_case_studies.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/004_Two_case_studies.Rnw"))
    cat(txt, file = "004_Two_case_studies.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/005_Elemenary_linear_models.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/005_Elemenary_linear_models.Rnw"))
    cat(txt, file = "005_Elemenary_linear_models.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/006_GLMs_and_GAMs.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/006_GLMs_and_GAMs.Rnw"))
    cat(txt, file = "006_GLMs_and_GAMs.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/007_Models_with_random_effects.Rnw", 
        "./.backup", overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/007_Models_with_random_effects.Rnw"))
    cat(txt, file = "007_Models_with_random_effects.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/008_Machine_learning_methods.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/008_Machine_learning_methods.Rnw"))
    cat(txt, file = "008_Machine_learning_methods.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/009_Some_Programming_Techniques.Rnw", 
        "./.backup", overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/009_Some_Programming_Techniques.Rnw"))
    cat(txt, file = "009_Some_Programming_Techniques.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/00A_template.Rnw", "./.backup", overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/00A_template.Rnw"))
    cat(txt, file = "00A_template.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/00B_using_Rcpp.Rnw", "./.backup", overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/00B_using_Rcpp.Rnw"))
    cat(txt, file = "00B_using_Rcpp.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/00-Prelim.Rnw", "./.backup", overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/00-Prelim.Rnw"))
    cat(txt, file = "00-Prelim.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/010_Introduction_to_S4.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/010_Introduction_to_S4.Rnw"))
    cat(txt, file = "010_Introduction_to_S4.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/011_An_Intro_to_parallel.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/011_An_Intro_to_parallel.Rnw"))
    cat(txt, file = "011_An_Intro_to_parallel.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/012_Making_packages_with_RStudio.Rnw", 
        "./.backup", overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/012_Making_packages_with_RStudio.Rnw"))
    cat(txt, file = "012_Making_packages_with_RStudio.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/013_Vulgar_fractions_in_R.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/013_Vulgar_fractions_in_R.Rnw"))
    cat(txt, file = "013_Vulgar_fractions_in_R.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/014_Solving_Sudoku_puzzles.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/014_Solving_Sudoku_puzzles.Rnw"))
    cat(txt, file = "014_Solving_Sudoku_puzzles.Rnw", sep = "\n")
## ----
    file.copy("RnwFiles/015_More_on_neck_pain.Rnw", "./.backup", 
        overwrite = TRUE)
    txt <- fix_labels(readLines("RnwFiles/015_More_on_neck_pain.Rnw"))
    cat(txt, file = "015_More_on_neck_pain.Rnw", sep = "\n")
