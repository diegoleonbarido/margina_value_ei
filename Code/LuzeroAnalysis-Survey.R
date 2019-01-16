## LUZERO ANALYSIS -- SURVEY DATA
## DEREK WOLFSON & DIEGO PONCE DE LEON BARIDO 
## LAST MODIFIED: JAN-16-2019 
library(readr)
library(dplyr)
library(devtools)
library(lubridate)

## SET WORKING DIRECTORY
getwd()
setwd(file.path('G:','marginal_value_ei'))

# load survey data 
surveyData <- read_csv('Data/baseline_endline_surveys.csv')


# create treatment indicators
treatmentGroups <- unique(aDataBilling$Current_Group) %>%
  setdiff('Control')
luzeroGroup <- c('treatment_luzero_no_SMS', 'treatment_luzero_SMS')
smsGroup <- c('treatment_SMS', 'treatment_luzero_SMS')

surveyData <- surveyData %>%
  mutate(anyTreatment = 
           ifelse(Current_Group %in% treatmentGroups & survey_time == 'endline', 1, 0),
         luzeroTreatment = 
           ifelse(Current_Group %in% luzeroGroup & survey_time == 'endline', 1, 0),
         smsTreatment = 
           ifelse(Current_Group %in% smsGroup & survey_time == 'endline', 1, 0),
         luzeroSMSTreatment =
           ifelse(Current_Group %in% 'treatment_luzero_SMS' & survey_time == 'endline', 1, 0),
         luzeroNoSMSTreatment =
           ifelse(Current_Group %in% 'treatment_luzero_no_SMS' & survey_time == 'endline', 1, 0),
         paperTreatment = 
           ifelse(Current_Group %in% 'treatment_PAPER' & survey_time == 'endline', 1, 0))
