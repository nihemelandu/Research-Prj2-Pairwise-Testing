library(stringr)

.DATASETS = c("ml-100k", "ml-25m", "amazon-instantvideo", "msmarco")
#.DATASETS = c("ml-100k", "ml-25m", "amazon-instantvideo")

#.MEASURES = c("recip_rank100", "ndcg100")
.MEASURES = c("recip_rank100")

.N_USERS = c(25, 50, 100, 500, 1000, 5000, 10000, 20000) # user set sizes
#.N_USERS = c(25, 50, 100, 500, 1000, 5000, 10000)

.DELTAS = seq(.01, .1, .01) # effect sizes
#.DELTAS = c(0.01, 0.05, 0.1) # effect sizes

.ALPHAS = c(1:9*.001, 1:9*.01, .1) # significance levels
#.ALPHAS = seq(0, 1, .001) #use for type-1 error chart

#.B = 1e6 # number of bootstrap/permutation samples
.B = 1e5 # for computational constrain we use 1e5
.SIGNIF <- 6 # number of decimal digits to save in scratch files

# computations will run in batches of a certain size, parallelizing over batches
.CORES = parallel::detectCores()

.BATCHES = 10
.N_PER_BATCH = 500

blb.parameters = list("25" = c("gamma" = 0.7, "n_subsample" = 2, "n_resample" = 100),
                      "50" = c("gamma" = 0.7, "n_subsample" = 3, "n_resample" = 100),
                      "100" = c("gamma" = 0.7, "n_subsample" = 3, "n_resample" = 100),
                      "500" = c("gamma" = 0.7, "n_subsample" = 5, "n_resample" = 100),
                      "1000" = c("gamma" = 0.7, "n_subsample" = 5, "n_resample" = 100))
# blb.par.search = list("25" = list("gamma" = c(0.5, 0.6, 0.7, 0.8),
#                                   "n_subsample" = c(1,2,3),
#                                   "n_resample" = c(20, 30, 40, 50, 60)),
#                       
#                       "100" = list(),
#                       "500" = list(),
#                       "1000" = list())

blb.par.search = list("25" = list("gamma" = c(0.5, 0.6, 0.7, 0.8),
                                  "n_subsample" = c(1,2),
                                  "n_resample" = c(20, 30, 40, 50, 60, 80)),
                      "50" = list("gamma" = c(0.5, 0.6, 0.7, 0.8),
                                  "n_subsample" = c(1,2),
                                  "n_resample" = c(20, 30, 40, 50, 60, 80)))
                      
# Utils ############################################################################################

#' Turn an alpha value into a fixed-width string
salpha <- function(alpha) {
  m <- max(str_length(.ALPHAS))
  str_pad(alpha, m, "right", "0")
}
#' Turn a delta vlaue into a fixed-width string
sdelta = function(delta) {
  m = max(str_length(.DELTAS))
  str_pad(delta, m, "right", "0")
}

#' Remove the bottom p% of runs
removeBottom = function(dat, p = .25) {
  mu = colMeans(dat)
  cat("mu-1 = ", length(mu), "\n")
  mu = mu[!is.na(mu)]
  cat("mu-2 = ", length(mu), "\n")
  i = mu >= quantile(mu, p)
  cat("i = ", sum(i), "\n")
  dat[,i]
}

#' Remove duplicate runs by their per-topic score
removeDuplicates <- function(dat, tol = 1e-5) {
  toremove <- integer(0)
  colremoved = c()
  for(i in seq(ncol(dat)-1)) {
    x <- dat[,i]
    for(j in seq(i+1,ncol(dat))) {
      y <- dat[,j]
      #if(all(abs(x-y)<=tol))
      if(abs(sum(x-y, na.rm = TRUE))<=tol){
        toremove <- c(toremove, j)
        colremoved = c(colremoved, colnames(dat)[j])
      }
    }
  }
  print(colremoved)
  if(length(toremove) > 0)
    dat[-toremove]
  else
    dat
}

