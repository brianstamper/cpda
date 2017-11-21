## O*Net data preparation
library(readxl)
library(data.table)

if(!dir.exists('data/onet')) dir.create('data/onet')
if(!dir.exists('data/processed')) dir.create('data/processed')

# Downloader function to save repetitive code and file transfers
onet_loader <- function(fn, ...) {
  if(!file.exists(paste0('data/onet/', fn))) {
    download.file(url = paste0('https://www.onetcenter.org/dl_files/database/db_22_1_text/', fn),
                  destfile = paste0('data/onet/', fn),
                  mode = 'wb')
  }
  fread(paste0('data/onet/', fn), na.strings = 'n/a', ...)
}

onet_content_model <- onet_loader('Content%20Model%20Reference.txt')
names(onet_content_model) <- c('elem_id', 'elem_name', 'elem_desc')
save(onet_content_model, file = 'data/processed/onet_content_model.rds')



# data with IM/LV
onet_knowledge <- onet_loader('Knowledge.txt')
onet_skills <- onet_loader('Skills.txt')
onet_abilities <- onet_loader('Abilities.txt')
onet_work_activities <- onet_loader('Work%20Activities.txt')

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

#save(onet_ksaw, file = 'data/processed/onet_ksaw.rds')




