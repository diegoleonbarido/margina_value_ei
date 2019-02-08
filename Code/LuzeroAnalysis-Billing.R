## LUZERO ANALYSIS -- DATA PREP
## DEREK WOLFSON & DIEGO PONCE DE LEON BARIDO 
## LAST MODIFIED: JAN-16-2019 
library(readr)
library(dplyr)
library(devtools)
library(lubridate)
library(lfe)

# load functions 
unique_id <- function(x, ...) {
  id_set <- x %>% select(...)
  id_set_dist <- id_set %>% distinct
  if (nrow(id_set) == nrow(id_set_dist)) {
    TRUE
  } else {
    non_unique_ids <- id_set %>% 
      filter(id_set %>% duplicated()) %>% 
      distinct()
    suppressMessages(
      inner_join(non_unique_ids, x) %>% arrange(...)
    )
  }
}

## SET WORKING DIRECTORY
getwd()
setwd(file.path('G:','marginal_value_ei'))


## INSPECT DATA 
aDataBilling <- read_csv(file.path('DATA','treatment_control_endline.csv'))
aDataUniqueHH <- select(aDataBilling, 'num_medidor', 'Current_Group') %>% unique()


## START ANALYSIS ON BILLING DATA
treatmentGroups <- unique(aDataBilling$Current_Group) %>%
  setdiff('Control')
luzeroGroup <- c('treatment_luzero_NO_SMS', 'treatment_luzero_SMS')
smsGroup <- c('treatment_SMS', 'treatment_luzero_SMS')

aDataBilling <- aDataBilling %>%
  mutate(anyTreatment = 
           ifelse(Current_Group %in% treatmentGroups & timeline == 'Ongoing Experiment' & !is.na(timeline), 1, 0),
        luzeroTreatment = 
          ifelse(Current_Group %in% luzeroGroup & timeline == 'Ongoing Experiment' & !is.na(timeline), 1, 0),
        smsTreatment = 
          ifelse(Current_Group %in% smsGroup & timeline == 'Ongoing Experiment' & !is.na(timeline), 1, 0),
        smsOnly = 
          ifelse(Current_Group %in% 'treatment_SMS' & timeline == 'Ongoing Experiment' & !is.na(timeline), 1, 0),
        luzeroSMSTreatment =
          ifelse(Current_Group %in% 'treatment_luzero_SMS' & timeline == 'Ongoing Experiment' & !is.na(timeline), 1, 0),
        luzeroNoSMSTreatment =
          ifelse(Current_Group %in% 'treatment_luzero_NO_SMS' & timeline == 'Ongoing Experiment' & !is.na(timeline), 1, 0),
        paperTreatment = 
          ifelse(Current_Group %in% 'treatment_PAPER' & timeline == 'Ongoing Experiment' & !is.na(timeline), 1, 0))
        
                            
# use some assertion commands to make sure these things are properly defined...
assertthat::are_equal(aDataBilling$luzeroTreatment * aDataBilling$smsTreatment, aDataBilling$luzeroSMSTreatment)
assertthat::are_equal(aDataBilling$luzeroTreatment * (1-aDataBilling$smsTreatment), aDataBilling$luzeroNoSMSTreatment)

# select some data to make this check easier
test <- aDataBilling %>% select(encuesta_id, timeline, Current_Group, anyTreatment, luzeroTreatment, smsTreatment, luzeroSMSTreatment, luzeroNoSMSTreatment, paperTreatment) %>%
  unique()
test %>% unique_id(encuesta_id, timeline, Current_Group) # these are unique values -- good for analysis now.

# ok now run some regressions with the billing data: 
aDataBilling <- aDataBilling %>% unique()

# run a regression on kwh used
model1 <- felm(csmo_energia ~ 
                 luzeroTreatment + smsTreatment + paperTreatment +
                 jan_prop + feb_prop + mar_prop + apr_prop + 
                 may_prop + jun_prop + jul_prop + aug_prop + 
                 sep_prop + oct_prop + nov_prop + dec_prop | 
                 encuesta_id | 0 | 0, data = aDataBilling)

model2 <- felm(csmo_energia ~ 
                 luzeroTreatment + smsTreatment + paperTreatment +
                 jan_prop + feb_prop + mar_prop + apr_prop + 
                 may_prop + jun_prop + jul_prop + aug_prop + 
                 sep_prop + oct_prop + nov_prop + dec_prop | 
                 encuesta_id | 0 | encuesta_id, data = aDataBilling)


modelAllTreatment <- felm(csmo_energia ~ 
                            luzeroSMSTreatment + luzeroNoSMSTreatment + smsOnly + paperTreatment +
                            jan_prop + feb_prop + mar_prop + apr_prop + 
                            may_prop + jun_prop + jul_prop + aug_prop + 
                            sep_prop + oct_prop + nov_prop + dec_prop | 
                            encuesta_id | 0 | encuesta_id, data = aDataBilling)
