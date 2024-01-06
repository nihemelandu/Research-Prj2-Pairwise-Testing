args = commandArgs(trailingOnly=TRUE)

# args[1] -- dataset
# args[2] -- measure
# args[3] -- n_users
# args[4] -- generic_run_name

if (length(args)!=4) {
  stop("usage: R-scripts/12-pvals-type_2.R <dataset> <measure> <n_users> <generic_run_name> \n 
       A dataset, a measure, n_users and generic_run_name must be supplied", call.=FALSE)
}

source("R-scripts/Utility.R")
source("R-scripts/import.R")
source("R-scripts/ir_tests.R")

library(stringr)
library(rio)
library(simIReff)
library(VineCopula)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

dataset = args[1]
measure = args[2]
n_users = as.integer(args[3])
generic_run_name = args[4]

cat("measure=",measure,"dataset=",dataset,"n_users=",n_users, "\n"); flush.console()

path_out = "scratch/12-type_2"

#for(measure in .MEASURES) {
  
# Import margins of runs evaluated with this measure (will be baselines)
effs_b = import_margins(measure = measure, dataset = dataset)

# Import bivariate copulas
cops = import_bicops(measure = measure, dataset = dataset, 
                     generic_run_name = generic_run_name)

#for(n_users in .N_USERS) {
#for(delta in .DELTAS) {
for(delta in c(0.1)) {
  path_out_measure_n_delta = file.path(path_out, dataset 
                                       ,paste0(measure, "_", n_users, "_", delta))
  dir.create(path_out_measure_n_delta, recursive = TRUE)
  cat(path_out_measure_n_delta, "\n"); flush.console()
  
  # Import transformed margins with this measure (will be experimentals)
  effs_e = import_margins_transform(measure, delta, dataset=dataset, 
                                    generic_run_name = generic_run_name)
  #effs_c = sapply(effs_e, length) (not necessary for recsys: same number of runs in every dataset)
  
  #for(batch in 1:ceiling(.BATCHES / length(.DELTAS))) {
  foreach(batch = 1:5,
          .packages = c("rio", "simIReff", "VineCopula")) %dopar% {
            
    # initialize table to store all data from this batch
    res = data.frame(dataset = rep("", 20), b = "", e = "",
                      marginb = "", margine = "", cop = "", d = 0,
                      t1 = 0, w1 = 0, s1 = 0, b1 = 0, p1 = 0, blb1 = 0,
                      t2 = 0, w2 = 0, s2 = 0, b2 = 0, p2 = 0, blb2 = 0,
                      stringsAsFactors = FALSE)
    for(r in 1:nrow(res)) { # for every trial in the batch
      
      # sample dataset uniformly
      dataset = sample(names(effs_e), 1)
      # sample baseline run within THE dataset, uniformly
      run_b = sample(names(effs_e[[dataset]]), 1)
      eff_b = effs_b[[dataset]][[run_b]]

      # sample experimental run
      run_e = sample(names(effs_e[[dataset]][[run_b]]), 1)
      eff_e = effs_e[[dataset]][[run_b]][[run_e]]
      
      # and copula between them
      swap = FALSE
      cop = cops[[dataset]][[run_b]][[run_e]]
      if(is.null(cop)) { # we only compute copulas for (i,j) where i<j, so we may need to swap
        swap = TRUE
        cop = cops[[dataset]][[run_e]][[run_b]]
      }
      
      if(!is.null(cop)){ #if cop is still null then skip this trial (added by Ngozi)
        
        # simulate pseudo-observations
        u = BiCopSim(n_users, obj = cop)
        # pass through margins to get final scores
        if(swap){
          b = qeff(u[,2], eff_b)
          e = qeff(u[,1], eff_e)
        }else{
          b = qeff(u[,1], eff_b)
          e = qeff(u[,2], eff_e)
        }
        
        # tests
        d = round(mean(e-b), .SIGNIF) # difference d=e-b
        p_t = round(test_t(b, e), .SIGNIF)
        p_w = round(test_wilcoxon(b,e ), .SIGNIF)
        p_s = round(test_sign(b, e), .SIGNIF)
        p_b = round(test_bootstrap(b, e, .B), .SIGNIF)
        p_p = round(test_permutation(b, e, .B), .SIGNIF)
        p_blb = round(test_blb(b, e, gamma=0.9, .B), .SIGNIF)
        
        res[r,] <- list(dataset, run_b, run_e,
                        eff_b$model$type, eff_e$model$original$model$type,
                        BiCopName(cop$family, short = TRUE), d,
                        p_t[1], p_w[1], p_s[1], p_b[1], p_p[1], p_blb[1],
                        p_t[2], p_w[2], p_s[2], p_b[2], p_p[2], p_blb[2])
      }
    }
    #save batch to file
    path_out_batch = file.path(path_out_measure_n_delta, paste0(batch, ".csv"))
    export(res, path_out_batch)
  }
}
# }
#}
stopImplicitCluster()
