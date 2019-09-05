## LUZERO ANALYSIS -- SURVEY DATA
## DEREK WOLFSON & DIEGO PONCE DE LEON BARIDO 
## LAST MODIFIED: JAN-16-2019 
library(readr)
library(dplyr)
library(devtools)
library(lubridate)
library(haven)

## SET WORKING DIRECTORY
getwd()
setwd(file.path('G:','marginal_value_ei'))

# load survey data 
surveyData <- read_csv('Data/baseline_endline_surveys.csv') 
  
# fix Current_Group
surveyData <- surveyData %>% 
  mutate(Current_Group = ifelse(Current_Group %in% c('No', 'NO'), 'Control', Current_Group)) %>%
  mutate(Current_Group = as.factor(Current_Group), 
         Assigned_Group = as.factor(Assigned_Group))

# create treatment indicators
treatmentGroups <- unique(surveyData$Current_Group) %>%
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

assertthat::are_equal(surveyData$luzeroTreatment * surveyData$smsTreatment, surveyData$luzeroSMSTreatment)

## clean up all the data you need...

# do some macro-cleaning "NA" cleaning first
  surveyData[7:NCOL(surveyData)] <- lapply(surveyData[7:NCOL(surveyData)], function(x){
    x <- ifelse(x == "n/a", NA, x)
    x <- ifelse(x == "NA", NA, x)
    x <- ifelse(x == "N/A", NA, x)
  })

# destring variables that should be numeric
  destring <- function(x){
    x %>% as.character() %>% as.numeric()
  }
  
  # set destring variables
  destringVars <- c(
    'num_personas'
  )
  surveyData[destringVars] <- lapply(surveyData[destringVars], destring)

#house type
surveyData <- surveyData %>% mutate(
  hhType_HH = ifelse(tipo_encuesta == 'casa', 1, 0),
  hhType_ME = ifelse(tipo_encuesta == 'micro_empresa', 1, 0)) %>%
  select(-tipo_encuesta)

# tariff code
surveyData <- surveyData %>% 
  mutate(tarrifType_t0 = ifelse(tariff_code == 't0', 1, 0), 
  tarrifType_t1 = ifelse(tariff_code == 't1', 1, 0), 
  tarrifType_tjubil = ifelse(tariff_code == 'tjubilados', 1, 0)) %>%
  select(-tariff_code) %>%

# meter
surveyData <- surveyData %>%
  mutate(meter = ifelse(medidor == 'si', 1, 0)) %>%
  select(-medidor)

# gender
surveyData <- surveyData %>%
  mutate(hhh_female = ifelse(sexo == 'mujer', 1, 0)) %>%
  select(-sexo)

# education 
surveyData <- surveyData %>%
  mutate(education = as.factor(nivel_educacion)) %>%
  mutate(edu_basico = ifelse(nivel_educacion == "ciclo_basico", 1, 0),
        edu_diversificado = ifelse(nivel_educacion == "ciclo_diversificado", 1, 0),
        edu_primaria = ifelse(nivel_educacion == 'primaria', 1, 0), 
        edu_sin = ifelse(nivel_educacion == 'sin', 1, 0), 
        edu_univ = ifelse(nivel_educacion == 'universidad', 1, 0)) %>%
  select(-nivel_educacion)

# store type
surveyData <- surveyData %>%
  mutate(store_type = as.factor(store_type))

# consume most variables
surveyData <- surveyData %>% mutate(
  energyMore_morning = ifelse(mas_gasta_tiempo == 'manana', 1, 0), 
  energyMore_midday = ifelse(mas_gasta_tiempo == 'medio_dia', 1, 0), 
  energyMore_afternoon = ifelse(mas_gasta_tiempo == 'tarde', 1, 0), 
  energyMore_night = ifelse(mas_gasta_tiempo == 'noche', 1, 0), 
  energyMore_noSe = ifelse(mas_gasta_tiempo == 'no_se', 1, 0),
  consumeMostDay = as.factor(mas_gasta_tiempo))
  select(-mas_gasta_tiempo)
  
# consume most hour
surveyData <- surveyData %>%
  mutate(consumeMostHour = as.character(hora_pico_energia) %>% as.numeric())

# financing for energy efficient applinaces
surveyData <- surveyData %>% 
  mutate(iFinancing = ifelse(financiamiento_eficiencia == 'si', 1, 0))

mod1 <- lm(financiamiento_eficiencia ~ Assigned_Group, data = surveyData)
  

## clean up the appliance ownership data...
baselineData <- surveyData %>% filter(survey_time == 'endline')
colnames(baselineData)
idVars <- c('encuesta_id', 'Assigned_Group', 'Current_Group', 'anyTreatment', 'luzeroTreatment', 'smsTreatment', 'luzeroSMSTreatment', 'luzeroNoSMSTreatment', 'paperTreatment')


baselineData <- baselineData %>% select(idVars, balanceVariables)

# mutate baseline data
colnames(baselineData)
baselineData <- baselineData %>%
  mutate(
    
    hora_pico_energia = as.numeric(as.character(hora_pico_energia)), 
    efficiencyInt_veryInterested = ifelse(baselineData$interest_efficiency == 'estoy_muy_interesado', 1, 0),
    efficiencyInt_interested = ifelse(baselineData$interest_efficiency == 'me_interesa', 1, 0),
    efficiencyInt_indifferent = ifelse(baselineData$interest_efficiency == 'mas_o_menos', 1, 0),
    efficiencyInt_notInterested = ifelse(baselineData$interest_efficiency == 'no_me_interesa', 1, 0),
    billPaymentDiff_easy = ifelse(baselineData$dificultad_pago == 'facil', 1, 0),
    billPaymentDiff_somewhatEasy = ifelse(baselineData$dificultad_pago == 'relativamente_facil', 1, 0),
    billPaymentDiff_hard = ifelse(baselineData$dificultad_pago == 'dificil', 1, 0),
    billPaymentDiff_veryHard = ifelse(baselineData$dificultad_pago == 'muy_dificil', 1, 0),
    saveByMonth_yes = ifelse(baselineData$ahorros_si_no == 'si', 1, 0),
    moreMoney_save = ifelse(ahorros_gastos == 'ahorraria', 1, 0), 
    trackEnergy_yes = ifelse(control_de_consumo == 'si', 1, 0),
    energySaveInfo_very = ifelse(informacion_util == 'muy_util', 1, 0),
    energySaveInfo_somewhat = ifelse(informacion_util == 'util', 1, 0),
    energySaveInfo_indiff = ifelse(informacion_util == 'mas_o_menos', 1, 0),
    energySaveInfo_notUseful = ifelse(informacion_util == 'no_me_sirve', 1, 0),
    energyShareInfo_yes = ifelse(comparte_papel_info == 'si', 1, 0),
    consumo_refrigerador = ifelse(consumo_refrigerador == '4t', 40, consumo_refrigerador))
    
    
    
    
    
    

# destring some variables 
destringVars <- c('consumo_abanico', 'consumo_celular', 'consumo_luces', 
                  'consumo_television', 'consumo_radio', 
                  'consumo_refrigerador',
                  'gasto_electrico',
                  'gasto_electrico_cordobas',
                  'gasto_tarifa_electrica',
                  'gasto_agua','gasto_agua_cordobas',
                   # 'gas_casa',
                  'gasto_gas',
                  'ingresos_mensuales', 'gastos_mensuales',
                  'porcentaje_electricidad',
                  'gasto_servicios',
                  'gasto_cosas_basicas',
                  'gastos_otros_no_diversion',
                  'gastos_diversion',
                  'cuanto_tiempo_dia_control',
                  'cuantos_dias_control',
                  'tiempo_pago')

sapply(baselineData[destringVars], table)
 

baselineData[destringVars] <- lapply(baselineData[destringVars], destring)

# still need to do the how much variables at the bottom
varList <- c('focos_eficiencia_ahorro',
'ventana_eficiencia_ahorro',
'botella_eficiencia_ahorro',
'cortina_eficiencia_ahorro',
'refri_eficiencia_ahorro',
'abanico_eficiencia_ahorro',
'tv_eficiencia_ahorro')

varList2 <- 
  c('focos_eficiencia_cuanto',
    'ventana_eficiencia_cuanto',
    'botella_eficiencia_cuanto',
    'cortina_eficiencia_cuanto',
    'refri_eficiencia_cuanto',
    'abanico_eficiencia_cuanto',
    'tv_eficiencia_cuanto')

lapply(baselineData[varList], table)
types <- unique(baselineData$ventana_eficiencia_ahorro) %>% na.omit() %>% as.character()

bottomVars1 <- function(x, df, levels){
  varname <- paste0(x,'_', 'none')
  mutate(df, !!varname := ifelse(x == 'nada', 1, 0))
  
  varname <- paste0(x,'_', 'low')
  mutate(df, !!varname := ifelse(x == 'poco', 1, 0))

  varname <- paste0(x,'_', 'med')
  mutate(df, !!varname := ifelse(x == 'mas o menos', 1, 0))
  
  varname <- paste0(x,'_', 'high')
  mutate(df, !!varname := ifelse(x == 'mucho', 1, 0))
  
  varname <- paste0(x,'_', 'nose')
  mutate(df, !!varname := ifelse(x == 'no se', 1, 0))
}

# set string answers that indicate zero WTP to zero
answerZeroList <- 
  c('no me interesa', 
    'no pagaria', 
    'se pueden meter a robar por ahi', 
    'no pagaria, se pueden meter a robar por ahi',
    'no se', 
    'no haria', 
    'no me interesa', 
    'no pagaria', 
    'ni idea', 
    'no le interesa', 
    'no lo haria')

baselineData[varList2] <- lapply(baselineData[varList2], function(x){
  x <- ifelse(x %in% answerZeroList, 0, x) %>% destring()
})

# export to stata for some analysis
baselineDataID <- baselineData %>% select(idVars)
baselineData1 <- baselineData %>% select(c(1, 107:143))
baselineData2 <- baselineData %>% select('encuesta_id',varList2, destringVars)
baselineData <- merge(baselineDataID, baselineData1, id = 'encuesta_id') %>%
  merge(baselineData2, id = 'encuesta_id')
write_dta(baselineData, 'data/baselineData.dta', version = 13)




