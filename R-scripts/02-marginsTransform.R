args = commandArgs(trailingOnly=TRUE)

# args[1] -- dataset
# args[2] -- measure

if (length(args)==0) {
  stop("usuage: R-scripts/02-marginsTransform.R <dataset> <measure>", call.=FALSE)
}

dataset = args[1]
measure = args[2]

source("R-scripts/Utility.R")
source("R-scripts/import.R")

library(rio)
library(simIReff)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

path_margins = "scratch/01-margins/"
path_out = "scratch/02-margins_transform"

#for(dataset in .DATASETS) {
#  for(measure in .MEASURES) {

dataset_meas = paste0(dataset, "_", measure)
eval_files = file.path("Evaluation_results", paste0(dataset_meas, ".csv"))
if(file.exists(eval_files)) {
  path_margins_DatasetMeas = file.path(path_margins, dataset_meas)
  path_out_DatasetMeas = file.path(path_out, dataset_meas)
  dir.create(path_out_DatasetMeas, recursive = TRUE)
  cat(path_out_DatasetMeas, "\n"); flush.console()
  
  # Import all marginal distributions
  effs = import_margins(measure, dataset)[[1]]
  set.seed(length(effs))
  
  # Expected values and indices of baseline systems (bottom 75% of runs)
  mu = sapply(effs, function(eff) eff$mean)
  baselines = order(mu, decreasing = TRUE)[-1:-round(length(mu)*.25)]
  
  for(delta in .DELTAS) {
    cat(path_out_DatasetMeas, delta, "\n"); flush.console()
    # for(i in baselines) {
      foreach(i = baselines, .packages = c("rio", "simIReff")) %dopar% { # for each baseline
        effi = effs[[i]] # baseline margin
        
        js = order(abs(mu - effi$mean - delta))
        js = js[js!=i]
        js = js[1:10] # 10 other margins closest in mean (+delta)
        for(j in js) {
          effj = effs[[j]]
          path_effj = file.path("scratch/02-margins_transform", dataset_meas,
                                 paste0(names(effs)[i], "_", delta, "_", names(effs)[j], ".rds"))
          effj = try(effTransform(effj, effi$mean + delta), silent = TRUE)
          if(!inherits(effj, "try-error")) { # if successful transformation, save
            export(effj, path_effj)
          }
          if(inherits(effj, "try-error")) { 
            cat(paste0(names(effs)[i], "_", delta, "_", names(effs)[j], "\n")); flush.console()
          }
        }
      }
    #}
  }
}
#  }
#}

stopImplicitCluster()