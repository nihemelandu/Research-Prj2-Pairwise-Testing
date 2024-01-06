args = commandArgs(trailingOnly=TRUE)

# args[1] -- dataset
# args[2] -- measure
if (length(args)!=2) {
  stop("usage: R-scripts/01-margins.R <dataset> <measure>\n 
       A dataset and a measure must be supplied", call.=FALSE)
}

dataset = args[1]
measure = args[2]

source("R-scripts/Utility.R")

library(rio)
library(VineCopula)

path_margins = "scratch/01-margins"
path_out = "scratch/03-bicops"

# for(dataset in .DATASETS) {
#   for(measure in .MEASURES) {
dataset_meas = paste0(dataset, "_", measure)
eval_files = file.path("Evaluation_results", paste0(dataset_meas, ".csv"))

if(file.exists(eval_files)) { # Do we have this dataset-measure combination?
  path_margins_DatasetMeas = file.path(path_margins, dataset_meas)
  path_out_DatasetMeas = file.path(path_out, dataset_meas)
  dir.create(path_out_DatasetMeas, recursive = FALSE)
  cat(path_out_DatasetMeas, "\n"); flush.console()
  
  # Read data
  eval_data = import(eval_files)
  
  # Set user as index and remove the user column
  rownames(eval_data) = eval_data$user
  eval_data$user=NULL
  
  # Remove the special characters "(" and ")" from the column names
  colnames(eval_data)=gsub("\\)","",gsub("\\(","",colnames(eval_data)))
  
  #Drop all rows with na
  #eval_data = na.omit(eval_data). instead of dropping all rows with na, just use  na.rm=TRUE
  # remove bottom 10% and duplicates
  eval_data = removeDuplicates(eval_data)
  eval_data = removeBottom(eval_data, p = .1)

  set.seed(ncol(eval_data))
  
  #list of copulas to fit: only those where i < j
  g = expand.grid(i = 1:ncol(eval_data), j = 1:ncol(eval_data))
  g = g[g$i < g$j,]
  
  for(r in 1:nrow(g)) {
    i = g$i[r]
    j = g$j[r]
    print(c(i,j))
    
    x = eval_data[,c(i,j)]
    u = pobs(x, ties.method = "random") # compute speudo-observations
    
    out = tryCatch(
      {
        cop = BiCopSelect(u[,1], u[,2], selectioncrit = "AIC") # fit copula,
        export(cop, file.path(path_out_DatasetMeas,
                              paste0(colnames(eval_data)[i], "_", colnames(eval_data)[j], ".rds")))
      },
      error=function(cond){
        cat("\n\nError from BiCopSelect for: ", colnames(eval_data)[c(i,j)], "\n")
        message(cond)
        return(NA)
      }
    )
  }
} #file exists
  #}
#}