

#Knowledge, Skills, Abilities
onet_knowledge <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Knowledge.txt', na.strings = 'n/a')
onet_skills <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Skills.txt', na.strings = 'n/a')
onet_abilities <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Abilities.txt', na.strings = 'n/a')

#Education, Experience, Training
onet_educ <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Education%2C%20Training%2C%20and%20Experience.txt', na.strings = 'n/a')
onet_educ_cats <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Education%2C%20Training%2C%20and%20Experience%20Categories.txt', na.strings = 'n/a')
onet_job_zones <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Job%20Zones.txt', na.strings = 'n/a')
onet_job_zone_ref <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Job%20Zone%20Reference.txt', na.strings = 'n/a')

#Interests, Work Values, Work Styles
onet_interests <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Interests.txt', na.strings = 'n/a')
onet_work_values <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Work%20Values.txt', na.strings = 'n/a')
onet_work_styles <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Work%20Styles.txt', na.strings = 'n/a')

#Work Context
onet_work_context <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Work%20Context.txt', na.strings = 'n/a')
onet_work_context_cats <- fread('https://www.onetcenter.org/dl_files/database/db_22_1_text/Work%20Context%20Categories.txt', na.strings = 'n/a')
