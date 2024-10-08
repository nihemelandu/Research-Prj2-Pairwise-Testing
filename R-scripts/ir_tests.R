Rcpp::sourceCpp("R-scripts/test.cpp")
source("R-scripts/blb.test.R")

test_t <- function(b, e) {
  p2 <- t.test(b, e, paired = TRUE, alternative = "two")$p.value
  p1 <- t.test(b, e, paired = TRUE, alternative = "less")$p.value

    c(p1, p2)
}

test_wilcoxon <- function(b, e) {
  p2 <- wilcox.test(b, e, paired = TRUE, alternative = "two")$p.value
  p1 <- wilcox.test(b, e, paired = TRUE, alternative = "less")$p.value

  c(p1, p2)
}

test_sign <- function(b, e, min_d = .01) {
  n <- length(b)
  pos <- sum(e - b > min_d)
  eq <- sum(abs(e - b) <= min_d)

  if(eq >= n)
    return(c(NA, NA))

  p2 <- binom.test(pos, n - eq, alternative = "two")$p.value
  p1 <- binom.test(pos, n - eq, alternative = "greater")$p.value
  c(p1, p2)
}

test_permutation <- function(b, e, B = 1e5) {
  d <- e-b
  testCpp_permutation(d, B)
}

test_bootstrap <- function(b, e, B = 1e5) {
  d <- e-b
  testCpp_bootstrap(d, B)
}

test_blb = function(b, e, gamma, B = 1e5) {
  d = e-b
  b_size = ceiling(length(d)^gamma)
  
  s = floor(length(d)/b_size)
  
  blb.test(d, b_size, s=s, r=B)
}

