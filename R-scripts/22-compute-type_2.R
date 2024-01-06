args = commandArgs(trailingOnly=TRUE)
dataset = args[1]

if (length(args)==0) {
  stop("usage: Rscript 22-compute-type_2.R <dataset>", call.=FALSE)
}

source("R-scripts/Utility.R")

library(stringr)
library(rio)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

#path_in = "scratch/archive/12-type_2"
path_in = "scratch/12-type_2"

for(measure in .MEASURES) {
#foreach(measure = .MEASURES, .packages = c("rio", "stringr")) %dopar% {
  for(n_users in .N_USERS){
    meauser = paste0(measure, "_", n_users)
    
    # Read data from all batches and compute the ecdfs to compute power rates later on
    pvals = sapply(.DELTAS, function(delta) {
      res = import_list(list.files(file.path(path_in, dataset, paste0(meauser, "_", delta)),
                                    full.names = TRUE), rbind = TRUE)   
      sapply(c("t2", "w2", "s2", "b2", "p2"), function(tn) {
        ecdf(res[,tn])
      })
    }, simplify = FALSE)
    names(pvals) = sdelta(.DELTAS)
    
    # For each significance level, compute power
    for(alpha in .ALPHAS) {
      #path_out = file.path("output/archive/type_2", dataset, "by_delta", paste0("alpha", salpha(alpha)))
      path_out = file.path("output/type_2", dataset, "by_delta", paste0("alpha", salpha(alpha)))
      dir.create(path_out, recursive = TRUE)
    
      res = t(sapply(pvals, function(pval) {
        sapply(c("t2", "w2", "s2", "b2", "p2"), function(tn) {
          pval[[tn]](alpha)
        })
      }))
      res =  cbind(delta = .DELTAS, res)

      export(res, file.path(path_out, paste0("type_2_by_delta_", meauser, "_alpha", salpha(alpha), ".csv")))
    }
    
    # For each delta, compute power
    # for(delta in names(pvals)) {
    #   path_out = file.path("output/type_2", dataset, "by_alpha", paste0("delta", delta))
    #   dir.create(path_out, recursive = TRUE)
    #   
    #   res = sapply(pvals[[delta]], function(pval) {
    #     pval(.ALPHAS)
    #   })
    #   res = cbind(alpha = .ALPHAS, res)
    #   
    #   export(res, file.path(path_out, paste0("type_2_by_alpha_", meauser, "_delta", delta, ".csv")))
    # }
    
  }
}

stopImplicitCluster()