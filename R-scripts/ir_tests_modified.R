Rcpp::sourceCpp("R-scripts/test.cpp")

test_t <- function(b, e) {
  ttest = t.test(b, e, paired = TRUE, alternative = "two")
  
  p2 = ttest$p.value
  statistic = ttest$statistic
  parameter = ttest$parameter
  estimate = ttest$estimate
  null.value = ttest$null.value
  stderr = ttest$stderr
  method = ttest$method
  data.name = ttest$data.name
  
  p1 <- t.test(b, e, paired = TRUE, alternative = "less")$p.value

  c("p1"=p1,"p2"=p2, "statistic"=statistic, "parameter"=parameter, 
    "estimate"=estimate, "null.value"=null.value, "stderr"=stderr,
    "method"=method, "data.name"=data.name)
}

test_wilcoxon <- function(b, e) {
  p2 <- wilcox.test(b, e, paired = TRUE, alternative = "two")$p.value
  p1 <- wilcox.test(b, e, paired = TRUE, alternative = "less")$p.value
  p3 <- wilcox.test(b, e, paired = TRUE, alternative = "two")$statistic
  p4 = length(b) - sum((e - b)==0)

  c(p1, p2, p3, p4)
}

test_sign <- function(b, e, min_d = .01) {
  n <- length(b)
  pos <- sum(e - b > min_d)
  eq <- sum(abs(e - b) <= min_d)
  d = (e - b)[abs(e - b) > min_d]
  mean_d = mean(d)
  median_d = median(d)
  #e1 = e[abs(e - b) > min_d]
  #b1 = b[abs(e - b) > min_d]
  
  if(eq >= n)
    return(c(NA, NA))

  p2 <- binom.test(pos, n - eq, alternative = "two")$p.value
  p1 <- binom.test(pos, n - eq, alternative = "greater")$p.value
  c(p1, p2, pos, n - eq, mean_d, median_d)
}

test_permutation <- function(b, e, B = 1e5) {
  d <- e-b
  testCpp_permutation(d, B)
}

test_bootstrap <- function(b, e, B = 1e5) {
  d <- e-b
  testCpp_bootstrap(d, B)
}
