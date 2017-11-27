library(data.table)
library(randomForest)

load('data/processed/ohiolmi_occ.rds')
load('data/processed/onet_ksaw.rds')
load('data/processed/onet_content_model.rds')

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


# Info gained about boolean x by boolean y - that is, H(Y) - H(Y|X)
info_gain <- function(y, x) {
  stopifnot(length(x) == length(y))
  entropy(y) - cond_entropy(y, x) 
}


# Define high growth
ohiolmi_occ[, high_growth := occ_empl_change_pct_lmi > .1]


# Known issue of not all SOCs match up, ignoring -- can't do much about it
ksaw_by_soc <- merge(onet_ksaw, 
                     subset(ohiolmi_occ, select = c('soc_code_lmi', 'high_growth')), 
                     by.x = 'soc_code', 
                     by.y = 'soc_code_lmi')


table(ksaw_by_soc$high_growth)


# Run simulations to identify a good information gain threshold
set.seed(0)

find_ig_thresh <- rbindlist(lapply(1:50, function(x) {
  message(x)
  rbindlist(lapply(seq(.0, .09, .01), function(ig_thresh) {
    #message('------------')
    #message(ig_thresh)

    
    ksaw_by_soc_sample <- rbind(
      ksaw_by_soc[(high_growth)][sample(.N, 120)],
      ksaw_by_soc[!(high_growth)][sample(.N, 120)]
    )[, -c('soc_code')]
    names(ksaw_by_soc_sample) <- make.names(names(ksaw_by_soc_sample))
    
    
    # soc_code is the first variable now, high_growth the last
    ksaw_values <- ksaw_by_soc_sample[, -c('high_growth')]

    
    information_gain <- sapply(ksaw_values, function(this_value) {
      info_gain(ksaw_by_soc_sample$high_growth, this_value)
    })
    
    information_gain <- data.table(elem_id = names(information_gain),
                                   elem_ig = information_gain)
    
    
    
    high_info_vars <- information_gain[elem_ig > ig_thresh]$elem_id
    ksaw_by_soc_sample_limited <- subset(ksaw_by_soc_sample, select = c('high_growth', high_info_vars))
    
    train <- sample(c(TRUE, FALSE), nrow(ksaw_by_soc_sample_limited), replace = TRUE, prob = c(.9, .1))
    test <- !train

    ksaw_train <- ksaw_by_soc_sample_limited[train]#, c(information_gain[elem_ig > ig_thresh]$elem_id, 'high_growth'), with = FALSE] 
    ksaw_test <- ksaw_by_soc_sample_limited[test]#, c(information_gain[elem_ig > ig_thresh]$elem_id, 'high_growth'), with = FALSE]
    ktree <- randomForest(as.factor(high_growth) ~ ., ksaw_train, importance = TRUE, ntree = 1000) 
    emp_pred <- predict(ktree, ksaw_train)
    table(emp_pred, ksaw_train$high_growth) / nrow(ksaw_train)
    #message(sum(emp_pred == ksaw_train$high_growth) / nrow(ksaw_train))
    test_pred <- predict(ktree, ksaw_test)
    table(test_pred, ksaw_test$high_growth) / nrow(ksaw_test)
    #message(sum(test_pred == ksaw_test$high_growth) / nrow(ksaw_test))
    data.table(ig_thresh, qom = sum(test_pred == ksaw_test$high_growth) / nrow(ksaw_test), nvars = length(high_info_vars))
  }))
  
}))


fit_sum <- rbindlist(lapply(seq(0, .09, .01), function(ig) {
  data.table(ig_thresh = ig,
             median = mean(find_ig_thresh[ig_thresh == ig]$qom),
             mean = mean(find_ig_thresh[ig_thresh == ig]$qom),
             sd = sd(find_ig_thresh[ig_thresh == ig]$qom),
             nvars = mean(find_ig_thresh[ig_thresh == ig]$nvars))
}))

boxplot(find_ig_thresh$qom ~ find_ig_thresh$ig_thresh)
fit_sum 

