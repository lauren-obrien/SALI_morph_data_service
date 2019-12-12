# lab data stats 20191128

### setup
options(stringsAsFactors = FALSE, scipen = 999)
library(sf)
library(DBI)
library(ROracle)
library(getPass)
library(tidyverse)

Sys.setenv(
  'TNS_ADMIN'   = file.path('C:', 'Users', Sys.getenv('USERNAME'), 'Oracle'),
  'ORACLE_HOME' = 'C:/Program Files/Oracle Instant Client'
)
SALI <- dbConnect(dbDriver('Oracle'),
                  username = 'obrienle',
                  password = getPass(),
                  dbname   = 'sispda',
                  host     = 'ex02client01',
                  port     = 1521)

# sit lab methods table
sit_lab_methods <- dbGetQuery(SALI, "select * from sit_lab_methods") %>%
  arrange(LAB_METH_CODE)

# lab results and external availability
labdat <-
  dbGetQuery(SALI, "select project_code, site_id, obs_no, lab_meth_code,
                      numeric_value, result_status,
                      case when site_qc_status = 'P'
                        and proj_qc_status = 'P'
                        and avail_Status = 'E'
                        then 'External' else 'Internal' end as od_status
                    from reg_projects
                    join sit_lab_results using (project_code)
                    join sit_locations using (project_code, site_id, obs_no)
                    where datum = 3 and longitude > 0")

# which projects have internal-only lab sites and how many
labsites_int <- labdat %>%
  dplyr::filter(OD_STATUS == 'Internal') %>%
  dplyr::select(PROJECT_CODE, SITE_ID, OBS_NO) %>%
  distinct() %>%
  group_by(PROJECT_CODE) %>%
  summarise(N_LAB_OBS_INT = n()) %>%
  arrange(PROJECT_CODE)

# analyte counts by lab method, split by availability
int_ext <- group_by(labdat, LAB_METH_CODE, OD_STATUS) %>%
  summarise(N_RESULTS = n()) %>%
  pivot_wider(names_from  = OD_STATUS, values_from = N_RESULTS) %>%
  ungroup()

# analyte counts by lab method, split by lab result status
rstat <- labdat %>%
  mutate(RESULT_STATUS= ifelse(is.na(RESULT_STATUS), 'UNK', RESULT_STATUS)) %>%
  group_by(LAB_METH_CODE, RESULT_STATUS) %>%
  summarise(N_RESULTS = n()) %>%
  pivot_wider(names_from  = RESULT_STATUS, values_from = N_RESULTS) %>%
  ungroup()

# analyte counts by lab method, split by lab result status and external availability
both <-  labdat %>%
  mutate(RESULT_STATUS= ifelse(is.na(RESULT_STATUS), 'UNK', RESULT_STATUS)) %>%
  group_by(LAB_METH_CODE, RESULT_STATUS, OD_STATUS) %>%
  summarise(N_RESULTS = n()) %>%
  unite('STATUS', RESULT_STATUS, OD_STATUS, sep = '-', na.rm = TRUE) %>%
  pivot_wider(names_from  = STATUS, values_from = N_RESULTS) %>%
  ungroup()

# sites with valid locations by project and availability
projects <- dbGetQuery(SALI,
  "with t1 as (select project_code,
     case when site_qc_status = 'P'
       and proj_qc_status = 'P'
       and avail_Status = 'E'
       then 'External' else 'Internal' end as od_status
   from reg_projects
   left join sit_observations using (project_code)
   join (select * from sit_locations where datum = 3) locs
     using (project_code, site_id, obs_no)
   where longitude > 0
   order by project_code)
  select project_code, od_status, count(*) as N_OBS
  from t1
  group by project_code, od_status
  order by od_status, project_code")

list("SIT_LAB_METHODS" = sit_lab_methods,
     "LABDAT_INTEXT" = int_ext,
     "LABDAT_RSTATUS" = rstat,
     "LABDAT_BOTH" = both,
     "LABSITES_INT" = labsites_int,
     "PROJECT_SITES" = projects) %>%
  saveRDS(., file.path(getwd(), 'DERIVED_DATA',
                       'SALI_availability_summaries_20191129.rds'))

#test <-readRDS(file.path(getwd(), 'DERIVED_DATA',
#                         'SALI_availability_summaries_20191129.rds'))

###


imo <- dbGetQuery(SALI,
  "select project_code, user_id as imo
   from reg_proj_users
   where project_role = 'IMO' and end_date is null and IMO != 'SALI'
  order by project_code") %>%
  group_by(PROJECT_CODE) %>%
  summarise(IMO = )



labsites_int <- left_join(labsites_int, imo, by = 'PROJECT_CODE')
