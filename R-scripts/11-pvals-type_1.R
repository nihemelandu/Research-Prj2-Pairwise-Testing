args = commandArgs(trailingOnly=TRUE)

# args[1] -- dataset
# args[2] -- measure
# args[3] -- n_users
# args[4] -- generic_run_name

if (length(args)!=4) {
  stop("usage: R-scripts/11-pvals-type_1.R <dataset> <measure> <n_users> <generic_run_name> \n 
       A dataset, a measure, n_users and generic_run_name must be supplied", call.=FALSE)
}

dataset = args[1]
measure = args[2]
n_users = as.integer(args[3])
generic_run_name = args[4]

source("R-scripts/Utility.R")
source("R-scripts/import.R")
source("R-scripts/ir_tests.R")

library(rio)
library(simIReff)
library(VineCopula)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

# Compute ------------------------------------------------------------------------------------------

path_out = "scratch/11-type_1/"

#for(measure in .MEASURES) {

# Import margins of runs evaluated with this measure
effs = import_margins(measure, dataset)
effs_c = sapply(effs, length) # count of margins per dataset, for sampling

# Import bivariate copulas
cops = import_bicops(measure, dataset, generic_run_name)

#for(n_users in .N_USERS) {
path_out_measure_n = file.path(path_out, dataset, paste0(measure, "_", n_users))

dir.create(path_out_measure_n, recursive = TRUE)
cat("\n", path_out_measure_n, "\n"); flush.console()

#for(batch in 1:20) {
foreach(batch = c(11:20), .packages = c("rio", "simIReff", "VineCopula")) %dopar% {
  #if (!file.exists(file.path(path_out_measure_n, paste0(batch, ".csv")))){
	  # initialize table to store all data from this batch
	  res = data.frame(dataset = rep("", .N_PER_BATCH), b = "", e = "",
						margin = "", cop = "", d = 0,
						t1 = 0, w1 = 0, s1 = 0, b1 = 0, p1 = 0, blb1 = 0,
						t2 = 0, w2 = 0, s2 = 0, b2 = 0, p2 = 0, blb2 = 0,
						stringsAsFactors = FALSE)
	  
	  for(r in 1:nrow(res)) { # for every trial in the batch
			# sample dataset, proportional to number of runs in it
			dataset = sample(names(effs), 1, prob = effs_c / sum(effs_c))
			# sample two runs within the collection, uniformly
			# Ngozi changed the code from the original code because a few pair of runs do not 
			# have copula
			runs_1 = sample(names(cops[[dataset]]), 1)
			runs_2 = sample(names(cops[[dataset]][[runs_1]]), 1)

			#cat(paste0(dataset, "-",runs_1,"-",runs_2), "\n"); flush.console()
			
			# margin of the 1st run and copula
			eff = effs[[dataset]][[runs_1]]
			cop = cops[[dataset]][[runs_1]][[runs_2]]
			
			if(is.null(cop)) # we only compute copulas for (i,j) where i<j, so we may need to swap
			  cop = cops[[dataset]][[runs_2]][[runs_1]]
			
			if(!(is.null(cop) | is.null(eff) )){ #if cop is still null or no marginal fit for selected run then skip this trial (added by Ngozi)
  			# simulate pseudo-observations
  			u = BiCopSim(n_users, obj = cop)
  			# pass through the same margin to get final scores
  			b = qeff(u[,1], eff)
  			e = qeff(u[,2], eff)
  			
  			# tests
  			d = round(mean(e-b), .SIGNIF) # difference d=e-b
  			p_t = round(test_t(b, e), .SIGNIF)
  			p_w = round(test_wilcoxon(b,e ), .SIGNIF)
  			p_s = round(test_sign(b, e), .SIGNIF)
  			p_b = round(test_bootstrap(b, e, .B), .SIGNIF)
  			p_p = round(test_permutation(b, e, .B), .SIGNIF)
  			p_blb = round(test_blb(b, e, gamma=0.9, .B), .SIGNIF)
  			
  			res[r,] = list(dataset, runs_1, runs_2,
  							eff$model$type, BiCopName(cop$family, short = TRUE), d,
  							p_t[1], p_w[1], p_s[1], p_b[1], p_p[1], p_blb[1],
  							p_t[2], p_w[2], p_s[2], p_b[2], p_p[2], p_blb[2])
			}
	  }

	  path_out_batch = file.path(path_out_measure_n, paste0(batch, ".csv"))
	  cat(path_out_batch, "\n"); flush.console()
	  export(res, path_out_batch)
  #} #!file.exists
}
#}
#}


stopImplicitCluster()
