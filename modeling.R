library(data.table)

load('data/processed/ohiolmi_occ.rds')
load('data/processed/onet_ksaw.rds')

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

# Define high growth as > 10%
ohiolmi_occ[, high_growth := occ_empl_change_pct_lmi > .1]


for(im_break in 4:8/10) {
  for(lv_break in 4:8/10) {
    
    
    onet_ksaw <- rbindlist(list(onet_knowledge, onet_skills, onet_abilities, onet_work_activities))
    
    
    # Separate the six-digit SOC and filter for cases where the suffix == 00 (the aggregate)
    onet_ksaw[, soc_code := substr(`O*NET-SOC Code`, 1, 7)]
    onet_ksaw[, soc_suff := substr(`O*NET-SOC Code`, 9, 10)]
    onet_ksaw <- onet_ksaw[soc_suff == '00']
    
    # rename and select relevant variables
    setnames(onet_ksaw,
             old = c('Element ID',
                     'Element Name',
                     'Scale ID',
                     'Data Value'),
             new = c('elem_id',
                     'elem_name',
                     'scale_id',
                     'value'))
    
    onet_ksaw <- subset(onet_ksaw,
                        select = c('soc_code',
                                   'elem_id',
                                   'elem_name',
                                   'scale_id',
                                   'value'))
    
    # Cast the 'importance' next to the 'level' for each soc/knowledge
    onet_ksaw <- dcast(onet_ksaw, soc_code + elem_id + elem_name ~ scale_id, value.var = 'value')
    
    # For each occupation, we want to know if a given knowledge area is
    # in the upper quantile for importance (Scale ID = 'IM') *AND* 
    # competency required (Scale ID = 'LV')
    # Compute with the upper quantile break point:
    # (using quantile because experimenting with different break points)
    onet_ksaw[, high_IM := quantile(IM, im_break), by = 'elem_id']
    onet_ksaw[, high_LV := quantile(LV, lv_break), by = 'elem_id']
    
    # Determine if both importance and level are above both for the occupation:
    onet_ksaw[, high_IMLV := IM > high_IM & LV > high_LV]
    
    # Cast the score wide over the knowledge areas:
    onet_ksaw <- dcast(onet_ksaw, soc_code ~ elem_id, value.var = 'high_IMLV')
    
    
    
    # Known issue of not all SOCs match up, ignoring -- can't do much about it
    ksaw_by_soc <- merge(onet_ksaw, 
                         subset(ohiolmi_occ, select = c('soc_code_lmi', 'high_growth')), 
                         by.x = 'soc_code', 
                         by.y = 'soc_code_lmi')
    
    # soc_code is the first variable now, high_growth the last
    ksaw_values <- ksaw_by_soc[, -c('soc_code', 'high_growth')]
    
    information_gain <- sapply(ksaw_values, function(this_value) {
      info_gain(ksaw_by_soc$high_growth, this_value)
    })
    
    information_gain <- data.table(elem_id = names(information_gain),
                                   elem_ig = information_gain)
    
    ig_explorer <- merge(information_gain, onet_content_model)
    top_ig <- ig_explorer[order(-frank(elem_ig))]
    message('im_break: ', im_break)
    message('lv_break: ', lv_break)
    message(round(top_ig$elem_ig[3], 2)) 
    
  }
}

#
#onet_ksaw <- rbindlist(list(onet_knowledge, onet_skills, onet_abilities, onet_work_activities))
#
#
## Separate the six-digit SOC and filter for cases where the suffix == 00 (the aggregate)
#onet_ksaw[, soc_code := substr(`O*NET-SOC Code`, 1, 7)]
#onet_ksaw[, soc_suff := substr(`O*NET-SOC Code`, 9, 10)]
#onet_ksaw <- onet_ksaw[soc_suff == '00']
#
## rename and select relevant variables
#setnames(onet_ksaw,
#         old = c('Element ID',
#                 'Element Name',
#                 'Scale ID',
#                 'Data Value'),
#         new = c('elem_id',
#                 'elem_name',
#                 'scale_id',
#                 'value'))
#
#onet_ksaw <- subset(onet_ksaw,
#                    select = c('soc_code',
#                               'elem_id',
#                               'elem_name',
#                               'scale_id',
#                               'value'))
#
## Cast the 'importance' next to the 'level' for each soc/knowledge
#onet_ksaw <- dcast(onet_ksaw, soc_code + elem_id + elem_name ~ scale_id, value.var = 'value')
#
## For each occupation, we want to know if a given knowledge area is
## in the upper quantile for importance (Scale ID = 'IM') *AND* 
## competency required (Scale ID = 'LV')
## Compute with the upper quantile break point:
## (using quantile because experimenting with different break points)
#onet_ksaw[, high_IM := quantile(IM, im_break), by = 'elem_id']
#onet_ksaw[, high_LV := quantile(LV, lv_break), by = 'elem_id']
#
## Determine if both importance and level are above both for the occupation:
#onet_ksaw[, high_IMLV := IM > high_IM & LV > high_LV]
#
## Cast the score wide over the knowledge areas:
#onet_ksaw <- dcast(onet_ksaw, soc_code ~ elem_id, value.var = 'high_IMLV')
#
#
#
## Known issue of not all SOCs match up, ignoring -- can't do much about it
#ksaw_by_soc <- merge(onet_ksaw, 
#                     subset(ohiolmi_occ, select = c('soc_code_lmi', 'high_growth')), 
#                     by.x = 'soc_code', 
#                     by.y = 'soc_code_lmi')
#
## soc_code is the first variable now, high_growth the last
#ksaw_values <- ksaw_by_soc[, -c('soc_code', 'high_growth')]
#
#information_gain <- sapply(ksaw_values, function(this_value) {
#  info_gain(ksaw_by_soc$high_growth, this_value)
#})
#
#information_gain <- data.table(elem_id = names(information_gain),
#                               elem_ig = information_gain)
#
#ig_explorer <- merge(information_gain, onet_content_model)
#