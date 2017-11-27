p <- sample(1:100, 1)
q <- sample(1:100, 1)
k1 <- data.table(v1 = c(rep(TRUE, p), rep(FALSE, q)), v2 = TRUE)
k2 <- data.table(v1 = c(rep(TRUE, p), rep(FALSE, q)), v2 = FALSE)
k <- rbind(k1, k2)


my_sims <- rbindlist(lapply(1:1000, function(x) {
  p1 <- runif(1)
  p2 <- runif(1)
  k <- data.table(v1 = sample(c(TRUE, FALSE), 50, prob = c(p1, 1 - p1), replace = TRUE),
                  v2 = sample(c(TRUE, FALSE), 50, prob = c(p2, 1 - p2), replace = TRUE))
  h1 <- entropy(k$v1)
  h2 <- entropy(k$v2)
  ig <- info_gain(k$v1, k$v2)
  
  data.table(p1, p2, h1, h2, ig) 
}))

summary(my_sims)
