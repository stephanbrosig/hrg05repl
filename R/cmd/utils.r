# utils.r
# e.g. codebookentries(factorvars[1:5],db2)
# e.g. codebookentries("Q90",db2)
# potentiell als erstes argument: factorvars <- database %>% select_if(is.factor) %>% names()

codebookentries <- function(factor_var_names, df) {
  if (!is.data.frame(df)) {
    stop("The input must be a tibble or a data frame.")
  }
 for (factor_var_name in factor_var_names) {
    if (!factor_var_name %in% names(df)) {
      stop(paste("The specified variable", factor_var_name, "does not exist in the tibble."))
    }
    factor_var <- df[[factor_var_name]]
 
     cat("\n", factor_var_name,"[",attr(factor_var,"label"), "] (Length:",length(factor_var),", # of nonmissing elements:", 
          sum(!is.na(factor_var)),")\n",sep="")
  if(anyNA(factor_var))  {
  codebook_info <- data.frame(
  Frequency = as.vector(table(factor_var,useNA = "always")),
  PctShare  = as.vector(round(table(factor_var,useNA = "always")/sum(table(factor_var,useNA = "always"))*100) ),
  LevelCode = c(1:length(levels(factor_var)),NA),
  LevelText = c(levels(factor_var),NA))
   } else {
  codebook_info <- data.frame(
  Frequency = as.vector(table(factor_var)),
  PctShare  = as.vector(round(table(factor_var)/sum(table(factor_var))*100) ),
  LevelCode = 1:length(levels(factor_var)),
  LevelText = levels(factor_var))
   }
  print(codebook_info)
  }
}

## obsolet codebookentries <- function(factor_var_names, df) {
## obsolet   if (!is.data.frame(df)) {
## obsolet     stop("The input must be a tibble or a data frame.")
## obsolet   }
## obsolet  for (factor_var_name in factor_var_names) {
## obsolet     if (!factor_var_name %in% names(df)) {
## obsolet       stop(paste("The specified variable", factor_var_name, "does not exist in the tibble."))
## obsolet     }
## obsolet     factor_var <- df[[factor_var_name]]
## obsolet  
## obsolet      cat("\n", factor_var_name, "\n")
## obsolet      
## obsolet   codebook_info <- data.frame(
## obsolet   Frequency = as.vector(table(factor_var)),
## obsolet   LevelCode = 1:length(levels(factor_var)),
## obsolet   LevelText = levels(factor_var))
## obsolet  print(codebook_info)
## obsolet   }
## obsolet }

