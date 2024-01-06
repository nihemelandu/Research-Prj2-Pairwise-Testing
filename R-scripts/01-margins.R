args = commandArgs(trailingOnly=TRUE)

# args[1] -- dataset
# args[2] -- measure
if (length(args)!=2) {
  stop("usage: R-scripts/01-margins.R <dataset> <measure>\n 
       A dataset and a measure must be supplied", call.=FALSE)
}

source("R-scripts/Utility.R")

library(rio)
library(simIReff)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

path_out = "scratch/01-margins"

#for(dataset in .DATASETS) {
  #for(measure in .MEASURES) {
dataset = args[1]
measure = args[2]

dataset_meas = paste0(dataset, "_", measure)
path_scores = file.path("Evaluation_results", paste0(dataset_meas, ".csv"))

if(file.exists(path_scores)) { # Do we have this dataset-measure combination?
  path_out_dataset_meas = file.path(path_out, dataset_meas)
  dir.create(path_out_dataset_meas, recursive = TRUE)
  cat(path_out_dataset_meas, "\n"); flush.console()
  
  # Read data and remove bottom 10% and duplicates
  dat = import(path_scores)
  cat("original ncol = ", ncol(dat), "\n")
  dat[is.na(dat)] = 0 # Remove nan (line added by Ngozi)
  cat("ncol after Remove nan = ", ncol(dat), "\n")
  dat = removeDuplicates(dat)
  dat = removeBottom(dat, p = .1)
  cat("ncol after Remove bottom = ", ncol(dat), "\n")
  
  # Set user as index and remove the user column
  rownames(dat) = dat$user
  dat$user=NULL
  
  # Remove the special characters "(" and ")" from the column names
  colnames(dat)=gsub("\\)","",gsub("\\(","",colnames(dat)))
  
  set.seed(ncol(dat))
  
  #for(i in 1:ncol(dat)) {
    #cat(i, " - ", colnames(dat)[i], "\n")
  foreach(i = 1:ncol(dat), .packages = c("rio", "simIReff")) %dopar% {
    # Fit margin, continuous or discrete
    if(measure %in% c("ndcg100")) {
      eff = effContFitAndSelect(dat[,i], method = "AIC", silent = FALSE)
    }else{ #hard-coded because of time constraint
      eff = effDiscFitAndSelect(dat[,i], support("rr", runLength = 100), method = "AIC", silent = TRUE)
    }
    
    export(eff, file.path(path_out_dataset_meas, paste0(colnames(dat)[i], ".rds")))
  }
}
  #}
#}

stopImplicitCluster()
