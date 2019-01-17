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


baselineData <- surveyData %>% filter(survey_time == 'baseline')
colnames(baselineData)
idVars <- c('encuesta_id', 'Assigned_Group', 'Current_Group', 'anyTreatment', 'luzeroTreatment', 'smsTreatment', 'luzeroSMSTreatment', 'luzeroNoSMSTreatment', 'paperTreatment')
balanceVariables <- c(
  'tipo_encuesta',
  'tariff_code',
  'num_personas',
  'medidor',
  'sexo',
  'nivel_educacion',
  'store_type',
  'mas_gasta_tiempo',
  'hora_pico_energia',
  'dia_pico_energia_choices',
  'ano_pico_energia_choices',
  'epoca_consumo',
  'bombillas',
  'bombillas_horas',
  'celular',
  'celular_horas',
  'internet',
  'internet_horas',
  'radio',
  'radio_horas',
  'television',
  'television_horas',
  'computadora',
  'computadora_horas',
  'refrigerador',
  'refrigerador_horas',
  'abanico',
  'abanico_horas',
  'aire_acondicionado',
  'aire_acondicionado_horas',
  'microondas',
  'microondas_horas',
  'licuadora',
  'licuadora_horas',
  'equipo',
  'equipo_horas',
  'lavadora',
  'lavadora_horas',
  'secadora',
  'secadora_horas',
  'plancha',
  'plancha_horas',
  'plancha_pelo',
  'plancha_pelo_horas',
  'consumo_abanico',
  'consumo_luces',
  'consumo_television',
  'consumo_celular',
  'consumo_radio',
  'consumo_refrigerador',
  'interest_efficiency',
  'estrategias_pasadas_efficiency',
  'barreras_efficiency',
  'razon_eficiencia_energeticaI',
  'razon_eficiencia_energeticaII',
  'razon_eficiencia_energeticaIII',
  'gasto_electrico',
  'gasto_electrico_cordobas',
  'gasto_tarifa_electrica',
  'gasto_agua',
  'gasto_agua_cordobas',
  'gas_casa',
  'gasto_gas',
  'ingresos_mensuales',
  'gastos_mensuales',
  'porcentaje_electricidad',
  'dificultad_pago',
  'ahorros_si_no',
  'gasto_servicios',
  'gasto_cosas_basicas',
  'gastos_otros_no_diversion',
  'gastos_diversion',
  'ahorros_gastos',
  'control_de_consumo',
  'como_contro_de_consumo',
  'cuanto_tiempo_dia_control',
  'cuantos_dias_control',
  'informacion_util',
  'uso_informacion_disnorte',
  'comparte_papel_info',
  'type_pago',
  'tiempo_pago',
  'type_pago_quality',
  'focos_eficiencia_ahorro',
  'focos_eficiencia_cuanto',
  'ventana_eficiencia_ahorro',
  'ventana_eficiencia_cuanto',
  'botella_eficiencia_ahorro',
  'botella_eficiencia_cuanto',
  'cortina_eficiencia_ahorro',
  'cortina_eficiencia_cuanto',
  'refri_eficiencia_ahorro',
  'refri_eficiencia_cuanto',
  'abanico_eficiencia_ahorro',
  'abanico_eficiencia_cuanto',
  'tv_eficiencia_ahorro',
  'tv_eficiencia_cuanto')

baselineData <- baselineData %>% select(idVars, balanceVariables)
write_dta(baselineData, 'data/baselineData.dta', version = 13)

# mutate baseline data
colnames(baselineData)
baselineData <- baselineData %>%
  mutate(
    hhType_HH = ifelse(tipo_encuesta == 'casa', 1, 0),
    hhType_ME = ifelse(tipo_encuesta == 'micro_empresa', 1, 0),
    tarrifType_t0 = ifelse(tariff_code == 't0', 1, 0), 
    tarrifType_t1 = ifelse(tariff_code == 't1', 1, 0), 
    tarrifType_tjubil = ifelse(tariff_code == 'tjubilados', 1, 0),
    num_personas = as.numeric(as.character(num_personas)),
    meter = ifelse(medidor == 'si', 1, 0), 
    hhh_female = ifelse(sexo == 'mujer', 1, 0),
    edu_basico = ifelse(nivel_educacion == "ciclo_basico", 1, 0),
    edu_diversificado = ifelse(nivel_educacion == "ciclo_diversificado", 1, 0),
    edu_primaria = ifelse(nivel_educacion == 'primaria', 1, 0), 
    edu_sin = ifelse(nivel_educacion == 'sin', 1, 0), 
    edu_univ = ifelse(nivel_educacion == 'universidad', 1, 0),
    energyMore_morning = ifelse(mas_gasta_tiempo == 'manana', 1, 0), 
    energyMore_midday = ifelse(mas_gasta_tiempo == 'medio_dia', 1, 0), 
    energyMore_afternoon = ifelse(mas_gasta_tiempo == 'tarde', 1, 0), 
    energyMore_night = ifelse(mas_gasta_tiempo == 'noche', 1, 0), 
    energyMore_noSe = ifelse(mas_gasta_tiempo == 'no_se', 1, 0), 
    hora_pico_energia = as.numeric(as.character(hora_pico_energia)), 
    energyMore_month_December = ifelse(ano_pico_energia_choices == 'diciembre', 1,0), 
    energyMore_season_same = ifelse(baselineData$epoca_consumo == 'igual', 1, 0), 
    energyMore_season_winter = ifelse(baselineData$epoca_consumo == 'invierno', 1, 0),
    energyMore_season_summer = ifelse(baselineData$epoca_consumo == 'verano', 1, 0),
    efficiencyInterest_veryInterested = ifelse(baselineData$interest_efficiency == 'estoy_muy_interesado', 1, 0),
    efficiencyInterest_interested = ifelse(baselineData$interest_efficiency == 'me_interesa', 1, 0),
    efficiencyInterest_indifferent = ifelse(baselineData$interest_efficiency == 'mas_o_menos', 1, 0),
    efficiencyInterest_notInterested = ifelse(baselineData$interest_efficiency == 'no_me_interesa', 1, 0),
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
 
destring <- function(x){
  x %>% as.character() %>% as.numeric()
}

baselineData[destringVars] <- lapply(baselineData[destringVars], destring)

# still need to do the how much variables at the bottom
varList <- c('focos_eficiencia_ahorro',
'ventana_eficiencia_ahorro',
'botella_eficiencia_ahorro',
'cortina_eficiencia_ahorro',
'refri_eficiencia_ahorro',
'abanico_eficiencia_ahorro',
'tv_eficiencia_ahorro')

varList2 <- c('focos_eficiencia_cuanto',
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

colnames(baselineData)
