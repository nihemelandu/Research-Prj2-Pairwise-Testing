
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("usage: Rscript 21-compute-type_1.R <dataset>", call.=FALSE)
}

dataset = args[1]

source("R-scripts/Utility.R")

library(rio)
library("tidyverse")

library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

path_out = file.path("output/type_1/final_use", dataset)
#if (!dir.exists(path_out))
dir.create(path_out, recursive = TRUE)

#foreach(dataset = .DATASETS, .packages = "rio") %dopar% {
path_in = paste0("scratch/11-type_1/",dataset) 

for(measure in .MEASURES) {
#foreach(measure = .MEASURES, .packages = "rio") %dopar% {
  for(n_users in .N_USERS){
    meauser = paste0(measure, "_", n_users)
    cat("path_in=", path_in, meauser, "\n")
    # Read data form all batches and compute Type I errors at all alpha levels
    res = import_list(list.files(file.path(path_in, meauser), full.names = TRUE), 
                      rbind = TRUE)
    
    res = res %>% 
      select(dataset, b, e, margin, cop, d,t2, w2, s2, b2, p2, blb2) %>%
      filter(dataset!="", b!="", e!="", margin!="", cop!="")
    
    type1 = sapply(c("t2", "w2", "s2", "b2", "p2", "blb2"), function(tn) {
      ecdf(res[,tn])(.ALPHAS)
    })
    type1 = cbind(alpha = .ALPHAS, type1)
    export(type1, file.path(path_out, paste0("type_1_", meauser, ".csv")))
  }
}
#}  
stopImplicitCluster()
