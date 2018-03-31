# Entropy function for a single boolean variable, H(Y)
entropy <- function(y) {
  if(length(y) == 0) return(1)
  p1y <- sum(y) / length(y)
  p2y <- sum(!y) / length(y)
  if(p1y == 0 | p2y == 0) return(0)
  - p1y * log2(p1y) - p2y * log2(p2y)
}

# Conditional entropy function for two boolean variables, H(Y|X)
cond_entropy <- function(y, x) {
  stopifnot(length(x) == length(y))
  p1x <- sum(x) / length(x)
  p2x <- sum(!x) / length(x)
  p1x * entropy(y[x]) + p2x * entropy(y[!x])
}

# Information gained about boolean y by boolean x -- that is, H(Y) - H(Y|X)
info_gain <- function(y, x) {
  stopifnot(length(x) == length(y))
  entropy(y) - cond_entropy(y, x) 
}

# Set seed for reproducibility
set.seed(0)

# Build a data table with each row being the results of one sample tested,
# with 50 samples for each threshold 0:9 * .01
find_ig_thresh <- rbindlist(lapply(7:9 * .01, function(ig_thresh) {
  message('\nig_thresh: ', ig_thresh, ' begin at ', Sys.time(), '\n')
  rbindlist(lapply(1:50, function(iteration) {
    message('iteration: ', iteration)
    # Prior exploration indicate we have at least 120 in each of high-growth and non-high-growth,
    # so we can create a balanced sample using up to that amount in each subset, with each 
    # being at least 20 so as to be "sufficiently large", in the statistical sense.
    half_sample_size <- sample(20:120, 1)
    message('half_sample_size: ', half_sample_size)
    ksaw_by_soc_sample <- rbind(
      ksaw_by_soc[(high_growth)][sample(.N, half_sample_size)],
      ksaw_by_soc[!(high_growth)][sample(.N, half_sample_size)]
    )
    
    # Measure the information gain of each variable according to this sample
    information_gain <- sapply(ksaw_by_soc_sample[, -c('soc_code', 'high_growth')], function(this_value) {
      info_gain(ksaw_by_soc_sample$high_growth, this_value)
    })
    
    information_gain <- data.table(elem_id = names(information_gain),
                                   elem_ig = information_gain)
    
    high_info_vars <- information_gain[elem_ig > ig_thresh]$elem_id
    nvars <- length(high_info_vars)
    message(nvars)
    ksaw_by_soc_sample_limited <- subset(ksaw_by_soc_sample, select = c('high_growth', high_info_vars))
    
    train <- sample(c(TRUE, FALSE), nrow(ksaw_by_soc_sample_limited), replace = TRUE, prob = c(.9, .1))
    test <- !train
    
    ksaw_train <- ksaw_by_soc_sample_limited[train]
    ksaw_test <- ksaw_by_soc_sample_limited[test]
    
    ktree <- randomForest(as.factor(high_growth) ~ ., ksaw_train, importance = TRUE, ntree = 1000) 
    test_pred <- predict(ktree, ksaw_test)
    data.table(ig_thresh,
               iteration,
               half_sample_size,
               qom = sum(test_pred == ksaw_test$high_growth) / nrow(ksaw_test), 
               nvars,
               crossval = ifelse(nvars > 2,
                                 min(rfcv(ksaw_by_soc_sample_limited[, -c('high_growth')],
                                          as.factor(ksaw_by_soc_sample_limited$high_growth))[['error.cv']]),
                                 NA))
  }))
}))