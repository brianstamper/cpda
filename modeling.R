
library(data.table)


load('data/processed/ohiolmi_occ.rds')
load('data/processed/onet_knowledge.rds')

ohiolmi_occ[, high_growth := occ_empl_change_pct_lmi > .1]

# Known issue of not all SOCs match up, ignoring -- can't do much about it
knowledge_by_soc <- merge(onet_knowledge, 
                          subset(ohiolmi_occ, select = c('soc_code_lmi', 'high_growth')), 
                          by.x = 'soc_code', 
                          by.y = 'soc_code_lmi')


library(data.table)

# Entropy function for a single variable
entropy <- function(x) {
  p1x <- sum(x) / length(x)
  p2x <- sum(!x) / length(x)
  - p1x * log2(p1x) - p2x * log2(p2x)
}

# Info gained about x by y - that is, (x|y)
info_gain <- function(x, y) {
  p1y <- sum(y) / length(y)
  p2y <- sum(!y) / length(y)
  entropy(x) - p1y * entropy(x[y]) - p2y * entropy(x[!y])
}

# Values based on the slide
k1 <- data.table(row = 1:59, murder = TRUE, gun = TRUE)
k2 <- data.table(row = 1:57, murder = TRUE, gun = FALSE)
k3 <- data.table(row = 1:59, murder = FALSE, gun = TRUE)
k4 <- data.table(row = 1:75, murder = FALSE, gun = FALSE)

mg <- rbindlist(list(k1, k2, k3, k4))

entropy(mg$murder)
info_gain(mg$murder, mg$gun)
