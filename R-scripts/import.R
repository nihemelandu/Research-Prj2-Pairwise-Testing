source("R-scripts/Utility.R")

library(rio)
library(simIReff)
library(VineCopula)
library(tools)
library(strex)

#' Import the marginal distributions for the given measure and collections.
#' If dataset is NULL, it imports from all datasets available for the given measure.
import_margins = function(measure, dataset = NULL, path = "scratch/01-margins/") {
  if(missing(dataset)) # if no dataset given, import them all
    datasets = .DATASETS

  effs = sapply(dataset, function(dat) { # for every dataset
    dataset_meas = paste0(dat, "_", measure)
    if(!file.exists(file.path("Evaluation_results/", paste0(dataset_meas, ".csv")))) # don't have this combination?
      return(NULL)
    runs = file_path_sans_ext(list.files(file.path(path, dataset_meas))) # runs with existing margin

    sapply(runs, function(run) { # for each run
      import(file.path(path, dataset_meas, paste0(run, ".rds")))
    }, simplify = FALSE)
  }, simplify = FALSE)

  effs = effs[!sapply(effs, is.null)] # remove datasets without that measure
  effs
}

#' Import the transformed marginal distributions for the given measure and datasets.
#' If dataset is NULL, it imports from all datasets available for the given measure.
import_margins_transform = function(measure, delta, dataset, generic_run_name,
                                         path = "scratch/02-margins_transform/") {
  if(missing(dataset)) # if no dataset given, import them all
    dataset = .DATASETS

  effs_t = sapply(dataset, function(dat) { # for every dataset
    dataset_meas = paste0(dat, "_", measure)
    if(!file.exists(file.path("Evaluation_results/", paste0(dataset_meas, ".csv")))) # don't have this combination?
      return(NULL)
    
    delta = as.character(delta)
    f = file_path_sans_ext(list.files(file.path(path,dataset_meas), pattern = paste0("_", delta, "_")))
    if (generic_run_name == "true")
      runs1 = unique(str_before_first(f, "_")) # all runs in 1st margin
    else
      runs1 = unique(str_before_nth(f, "_", n=2)) # all runs in 1st margin
    
      effs_t = sapply(runs1, function(run1) {
      runs2 = file_path_sans_ext(list.files(file.path(path, dataset_meas),
                                             pattern = paste0(run1, "_", delta, "_")))
      
      if (generic_run_name == "true")
        runs2 = str_after_last(runs2, "_") #grab all runs in 2nd margin
      else
        runs2 = str_after_nth(runs2, "_", n=3) #grab all runs in 2nd margin
      
      sapply(runs2, function(run2) {
        import(file.path(path, dataset_meas, paste0(run1, "_", delta, "_", run2, ".rds")))
      }, simplify = FALSE)
    }, simplify = FALSE)
  }, simplify = FALSE)

  effs_t = effs_t[!sapply(effs_t, is.null)] # remove datasets without that measure
  effs_t
}

#' Import the copulas for the given measure and datasets.
#' If dataset is NULL, it imports from all datasets available for the given measure.
import_bicops = function(measure, dataset, generic_run_name, path = "scratch/03-bicops/") {
  if(missing(dataset)) # if no dataset given, import them all
    dataset = .DATASETS

    cops = sapply(dataset, function(dat) { # for every dataset
    dataset_meas = paste0(dat, "_", measure)
    if(!file.exists(file.path("Evaluation_results/", paste0(dataset_meas, ".csv")))) # don't have this combination?
      return(NULL)

    f = file_path_sans_ext(list.files(file.path(path, dataset_meas)))
    if (generic_run_name == "true")
      runs1 = unique(str_before_first(f, "_")) # grab all first margins
    else
      runs1 = unique(str_before_nth(f, "_", n=2)) # grab all first margins
    
    sapply(runs1, function(run1) {
      runs2 = file_path_sans_ext(list.files(file.path(path, dataset_meas),
                                             pattern = paste0("^", run1, "_")))
      
      if (generic_run_name == "true")
        runs2 = str_after_first(runs2, "_") #grab all 2nd margins
      else
        runs2 = str_after_nth(runs2, "_", n=2) #grab all 2nd margins
      
      sapply(runs2, function(run2) {
        import(file.path(path, dataset_meas, paste0(run1, "_", run2, ".rds")))
      }, simplify = FALSE)
    }, simplify = FALSE)
  }, simplify = FALSE)

  cops = cops[!sapply(cops, is.null)] # remove datasets without that measure
  cops
}

# import_model <- function(measure, collection, check = FALSE) {
#   collmeas <- paste0(collection, "_", measure)
#   if(!file.exists(file.path("data/", paste0(collmeas, ".csv"))))
#     return(NULL)
#   effs <- import_margins(collection, measure)
#   effs_t <- sapply(as.character(.DELTAS), function(delta) {
#     import_margins_transform(collection, measure, delta)
#   }, simplify = FALSE)
#   cops <- import_bicops(collection, measure)
# 
#   # checks
#   deltas <- numeric(0)
#   if(check) {
#     stopifnot(length(effs)*(length(effs)-1)/2 == sum(sapply(cops, length)))
# 
#     for(delta in .DELTAS) {
#       sdelta <- as.character(delta)
# 
#       deltad <- effs_t[[sdelta]]
#       for(e1 in names(deltad)) {
#         eff1 <- effs[[e1]]
#         deltade <- deltad[[e1]]
#         for(e2 in names(deltade)) {
#           deltas <- c(deltas, eff1$mean + delta - deltade[[e2]]$mean)
#         }
#       }
#     }
#   }
# 
#   list(effs = effs, effs_t = effs_t, cops = cops, deltas = deltas)
# }
