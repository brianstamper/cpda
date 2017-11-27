library(data.table)
library(randomForest)

load('data/processed/ohiolmi_occ.rds')
load('data/processed/onet_ksaw.rds')
load('data/processed/onet_content_model.rds')

# Entropy function for a single boolean variable
entropy <- function(x) {
  if(length(x) == 0) return(1)
  p1x <- sum(x) / length(x)
  p2x <- sum(!x) / length(x)
  if(p1x == 0 | p2x == 0) return(0)
  - p1x * log2(p1x) - p2x * log2(p2x)
}

# Info gained about boolean x by boolean y - that is, (x|y)
info_gain <- function(x, y) {
  stopifnot(length(x) == length(y))
  p1y <- sum(y) / length(y)
  p2y <- sum(!y) / length(y)
  entropy(x) - p1y * entropy(x[y]) - p2y * entropy(x[!y])
}


# Define high growth
ohiolmi_occ[, high_growth := occ_empl_change_pct_lmi > .1]


# Known issue of not all SOCs match up, ignoring -- can't do much about it
ksaw_by_soc <- merge(onet_ksaw, 
                     subset(ohiolmi_occ, select = c('soc_code_lmi', 'high_growth')), 
                     by.x = 'soc_code', 
                     by.y = 'soc_code_lmi')


table(ksaw_by_soc$high_growth)


# Sample a balanced set for measuring information gain
set.seed(17)
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

ig_explorer <- merge(information_gain, onet_content_model)

high_info_vars <- information_gain[elem_ig > .01]$elem_id
#ksaw_by_soc_sample <- subset(ksaw_by_soc_sample, select = c('high_growth', high_info_vars))

train <- sample(c(TRUE, FALSE), nrow(ksaw_by_soc_sample), replace = TRUE, prob = c(.8, .2))
test <- !train

#ksaw_by_soc_sample$high_growth <- as.factor(ksaw_by_soc_sample$high_growth)

#{
#  lapply(seq(500, 4000, 500), function(nt) {
#    message('------------')
#    message(ig_thresh)
#    message(nt)
    ksaw_train <- ksaw_by_soc_sample[train]#, c(information_gain[elem_ig > ig_thresh]$elem_id, 'high_growth'), with = FALSE] 
    ksaw_test <- ksaw_by_soc_sample[test]#, c(information_gain[elem_ig > ig_thresh]$elem_id, 'high_growth'), with = FALSE]
    ktree <- randomForest(as.factor(high_growth) ~ ., ksaw_train, importance = TRUE, ntree = 2000) 
    emp_pred <- predict(ktree, ksaw_train)
    table(emp_pred, ksaw_train$high_growth) / nrow(ksaw_train)
    #message(sum(emp_pred == ksaw_train$high_growth) / nrow(ksaw_train))
    test_pred <- predict(ktree, ksaw_test)
    table(test_pred, ksaw_test$high_growth) / nrow(ksaw_test)
    message(sum(test_pred == ksaw_test$high_growth) / nrow(ksaw_test))
#  })
#  NULL
#}
