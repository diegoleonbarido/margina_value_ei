## LUZERO ANALYSIS -- DATA PREP
## DEREK WOLFSON & DIEGO PONCE DE LEON BARIDO 
## LAST MODIFIED: JAN-16-2019 
library(readr)

## SET WORKING DIRECTORY
getwd()
setwd(file.path('G:','marginal_value_ei'))


## INSPECT DATA 
aData <- read_csv('data','treatment_control_endline.csv')
