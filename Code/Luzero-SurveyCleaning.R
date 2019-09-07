###########################################
# LUZERO DATA BUILD TAKE II 
# AUTHOR: DEREK WOLFSON 
# CREATION DATE: 09/05/2019
###########################################


###########################################
## SECTION 0 
##  PREAMBLE 
###########################################
## INSTALL 
require('pacman')
pacman::p_load('readr', 'dplyr', 'devtools', 'lubridate', 'haven', 'tidyr')

## SET WORKING DIRECTORY 
username <- Sys.info()['user']
if(username == 'Derek' | username == 'derekwolfson'){
  gitDir <- file.path('G:/','marginal_value_ei')
  dataDir <- file.path('Z:', 'My Drive', 'marginal_value_ei', 'Data')
}
if(username == 'diego'){
  setwd('/Users/diego/Desktop/Projects_Code/marginal_value_ei/')
}


## LOAD DATASETS FOR USE LATER
endline <- read.csv(file.path(dataDir, '01-raw', 'surveyData', 'Metadata_endline_survey_data copy.csv'))
baseline <- read.csv(file.path(dataDir, '01-raw', 'surveyData', 'Metadata_baseline_survey_data.csv'))
df_data <- read.csv(file.path(dataDir, '01-raw', 'billingData', 'df_data.csv'))
detalles_casas <- read.csv(file.path(dataDir, '01-raw', 'adminData', 'detalles_casas.csv'))
encuesta_id_nis_control <- read.csv(file.path(dataDir, '01-raw', 'adminData', 'encuesta_id_nis_control.csv'))
implementation_timeline <- read.csv(file.path(dataDir, '01-raw', 'adminData', 'implementation_timeline.csv'))

###########################################
## END SECTION 0 
###########################################

###########################################
## SECTION 1 -
##  CLEAN BASELINE
###########################################
# turn all blank cells into NA 
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}
baseline <- baseline %>% mutate_all(funs(empty_as_na))


# treatment indicators/study indicators
baseline <- baseline %>%
  mutate(Current_Group = ifelse(Current_Group %in% c('No', 'NO'), 'Control', Current_Group)) %>%
  mutate(Current_Group = as.factor(Current_Group)) %>%
  mutate(Assigned_Group = ifelse(Assigned_Group %in% c('No', 'NO'), 'Control', Assigned_Group)) %>%
  mutate(Assigned_Group = as.factor(Assigned_Group))

# household variables
baseline <- baseline %>% mutate(
  # hh type
  hhType_HH = if_else(tipo_encuesta == 'casa', 1, 0, missing = NULL),
  hhType_ME = if_else(tipo_encuesta == 'micro_empresa', 1, 0, missing = NULL),
  # hh size
  num_personas = as.numeric(as.character(num_personas)), 
  # hh head gender
  hhh_female = if_else(sexo == 'mujer', 1, 0, missing = NULL),
  # hh head education
  edu_basico = if_else(nivel_educacion == "ciclo_basico", 1, 0),
  edu_diversificado = if_else(nivel_educacion == "ciclo_diversificado", 1, 0),
  edu_primaria = if_else(nivel_educacion == 'primaria', 1, 0), 
  edu_sin = if_else(nivel_educacion == 'sin', 1, 0), 
  edu_univ = if_else(nivel_educacion == 'universidad', 1, 0))

# tariff type/electricity stuff
baseline <- baseline %>% mutate(
  tarrifType_t0 = ifelse(tariff_code == 't0', 1, 0),
  tarrifType_t0 = ifelse(is.na(tariff_code), NA, tarrifType_t0), 
  tarrifType_t1 = ifelse(tariff_code == 't1', 1, 0), 
  tarrifType_t1 = ifelse(is.na(tariff_code), NA, tarrifType_t1), 
  tarrifType_tjubil = ifelse(tariff_code == 'tjubilados', 1, 0),
  tarrifType_tjubil = ifelse(is.na(tariff_code), NA, tarrifType_tjubil), 
  meter = ifelse(medidor == 'si', 1, 0),
  meter = ifelse(is.na(medidor), NA, meter),
  # when do they use energy the most
  energyMore_morning = if_else(mas_gasta_tiempo == 'manana', 1, 0), 
  energyMore_midday = if_else(mas_gasta_tiempo == 'medio_dia', 1, 0), 
  energyMore_afternoon = if_else(mas_gasta_tiempo == 'tarde', 1, 0), 
  energyMore_night = if_else(mas_gasta_tiempo == 'noche', 1, 0), 
  energyMore_noSe = if_else(mas_gasta_tiempo == 'no_se', 1, 0),
  # highest hour of usage during day 
  hora_pico_energia = as.numeric(as.character(hora_pico_energia)),
  # highest month of usage
  energyMore_month_December = if_else(ano_pico_energia_choices == 'diciembre', 1,0), 
  # highest season of usage
  energyMore_season_same = if_else(epoca_consumo == 'igual', 1, 0), 
  energyMore_season_winter = if_else(epoca_consumo == 'invierno', 1, 0),
  energyMore_season_summer = if_else(epoca_consumo == 'verano', 1, 0))

# appliance ownership
baseline <- baseline %>% mutate(
  ## lights
  bombillas = if_else(bombillas == '3 bombillas y 16 Candela', '3', bombillas), 
  bombillas = if_else(bombillas == 'tres', '3', bombillas),
  bombillas = as.numeric(as.character(bombillas)),
  ## cellular phones
  celular = if_else(celular == "Una hora", "1", celular),
  celular = as.numeric(as.character(celular)),
  ## internet 
  internet = if_else(internet == "Si", "1", internet),
  internet = as.numeric(as.character(internet)),
  ## radio (OK)
  ## television (OK)
  ## computer (OK)
  ## fridge 
  refrigerador = if_else(refrigerador == "Est’‘’\u0081 da’‘Î±ado", "", refrigerador),
  refrigerador = as.numeric(as.character(refrigerador)),
  ## fan (OK)
  ## AC (OK) 
  ## Microwave (OK)
  ## Blender (OK)
  ## equipo (??)
  ## washing machine (OK)
  ## dryer (OK)
  ## Griddle (plancha) (OK)
  ## Hair Straightener 
  plancha_pelo = if_else(plancha_pelo == "150", "", plancha_pelo), ## 150?
  plancha_pelo = if_else(plancha_pelo == "1 hora a la semana", "1", plancha_pelo),
  plancha_pelo = as.numeric(as.character(plancha_pelo)))
  #
  
# energy efficiency interest
baseline <- baseline %>% mutate(
efficiencyInterest_veryInterested = if_else(interest_efficiency == 'estoy_muy_interesado', 1, 0),
efficiencyInterest_interested = if_else(interest_efficiency == 'me_interesa', 1, 0),
efficiencyInterest_indifferent = if_else(interest_efficiency == 'mas_o_menos', 1, 0),
efficiencyInterest_notInterested = if_else(interest_efficiency == 'no_me_interesa', 1, 0))

# difficulty paying bills
baseline <- baseline %>% mutate(
  billPaymentDiff_easy = if_else(dificultad_pago == 'facil', 1, 0),
  billPaymentDiff_somewhatEasy = if_else(dificultad_pago == 'relativamente_facil', 1, 0),
  billPaymentDiff_hard = if_else(dificultad_pago == 'dificil', 1, 0),
  billPaymentDiff_veryHard = if_else(dificultad_pago == 'muy_dificil', 1, 0))
  
  
# saving by end of month
baseline <- baseline %>% mutate(
  saveByMonth_yes = if_else(ahorros_si_no == 'si', 1, 0))

# if given more money, would you save or spend?
baseline <- baseline %>% mutate(
  moreMoney_save = if_else(ahorros_gastos == 'ahorraria', 1, 0))

# do ou track energy consumption?
baseline <- baseline %>% mutate(
  trackEnergy_yes = if_else(control_de_consumo == 'si', 1, 0))

# How useful is information for energy saving?
baseline <- baseline %>% mutate(
energySaveInfo_very = if_else(informacion_util == 'muy_util', 1, 0),
  energySaveInfo_somewhat = if_else(informacion_util == 'util', 1, 0),
  energySaveInfo_indiff = if_else(informacion_util == 'mas_o_menos', 1, 0),
  energySaveInfo_notUseful = if_else(informacion_util == 'no_me_sirve', 1, 0),
  energyShareInfo_yes = if_else(comparte_papel_info == 'si', 1, 0))

# do you share you energy info with anyone else?
baseline <- baseline %>% mutate( 
  shareEnergyInfo = if_else(comparte_papel_info == 'si', 1, 0))

# clean 'how much willing to pay for efficient appliance...'
## change things like "not willing" to zero, respecting missing values
baseline <- baseline %>%
  mutate_at(vars(matches("eficiencia_cuanto")), destring) %>%
  mutate_at(vars(matches("eficiencia_cuanto")), as.numeric)


# Creating likert scale for interest variables
baseline <- baseline %>% mutate(
interest_efficiency_likert = 
  ifelse(interest_efficiency == "estoy_muy_interesado",4,
  ifelse(interest_efficiency == "me_interesa",3,
  ifelse(interest_efficiency == "mas_o_menos",2,
  ifelse(interest_efficiency == "no_me_interesa",1,NA)))),
dificultad_pago_likert = 
  ifelse(dificultad_pago == "facil",4,
  ifelse(dificultad_pago == "relativamente_facil",3,
  ifelse(dificultad_pago == "dificil",2,
  ifelse(dificultad_pago == "muy_dificil",1,NA)))),
informacion_util_likert = 
  ifelse(informacion_util == "muy_util",4,
  ifelse(informacion_util == "util",3,
  ifelse(informacion_util == "mas_o_menos",2,
  ifelse(informacion_util == "no_me_sirve",1,NA)))),
focos_eficiencia_ahorro_likert = 
  ifelse(focos_eficiencia_ahorro == "mucho",3,
  ifelse(focos_eficiencia_ahorro == "mas o menos",2,
  ifelse(focos_eficiencia_ahorro == "poco",1,NA))),
ventana_eficiencia_ahorro_likert = 
  ifelse(ventana_eficiencia_ahorro == "mucho",3,
  ifelse(ventana_eficiencia_ahorro == "mas o menos",2,
  ifelse(ventana_eficiencia_ahorro == "poco",1,NA))),
botella_eficiencia_ahorro_likert = 
  ifelse(botella_eficiencia_ahorro == "mucho",3,
  ifelse(botella_eficiencia_ahorro == "mas o menos",2,
  ifelse(botella_eficiencia_ahorro == "poco",1,NA))),
cortina_eficiencia_ahorro_likert = 
  ifelse(cortina_eficiencia_ahorro == "mucho",3,
  ifelse(cortina_eficiencia_ahorro == "mas o menos",2,
  ifelse(cortina_eficiencia_ahorro == "poco",1,NA))),
refri_eficiencia_ahorro_likert = 
  ifelse(refri_eficiencia_ahorro == "mucho",3,
  ifelse(refri_eficiencia_ahorro == "mas o menos",2,
  ifelse(refri_eficiencia_ahorro == "poco",1,NA))),
abanico_eficiencia_ahorro_likert =  
  ifelse(abanico_eficiencia_ahorro == "mucho",3,
  ifelse(abanico_eficiencia_ahorro == "mas o menos",2,
  ifelse(abanico_eficiencia_ahorro == "poco",1,NA))),
tv_eficiencia_ahorro_likert = 
  ifelse(tv_eficiencia_ahorro == "mucho",3,
  ifelse(tv_eficiencia_ahorro == "mas o menos",2,
  ifelse(tv_eficiencia_ahorro == "poco",1,NA))))

# how much willing to pay for new efficient appliances...
destring <- function(x){
  gsub('.*[a-zA-Z].*', '0', x)
  }

saveBaseline <- file.path(dataDir, 'clean', )
write_csv(baseline, dataDir)

###########################################
# END SECTION 1 
###########################################

###########################################
## SECTION 2  
##  CLEAN ENDLINE DATA 
###########################################


###########################################
## SECTION 1  
##  CREATE TREATMENT INDICATORS 
###########################################
# load survey data
surveyData <- read_csv('Data/Raw/baseline_endline_surveys.csv')

# extract assignment information from baseline period only
assignment <- surveyData %>% filter(survey_time == 'baseline') %>%
  select(encuesta_id, Current_Group, Assigned_Group) %>%
  mutate(Current_Group = ifelse(Current_Group %in% c('No', 'NO'), 'Control', Current_Group)) %>%
  mutate(Current_Group = as.factor(Current_Group)) %>%
  mutate(Assigned_Group = ifelse(Assigned_Group %in% c('No', 'NO'), 'Control', Assigned_Group)) %>%
  mutate(Assigned_Group = as.factor(Assigned_Group))


# clean and merge the assignment variables 
surveyData <- surveyData %>% 
  select(-Current_Group, -Assigned_Group) %>%
  merge(assignment, by = 'encuesta_id')
  

# create treatment indicators only for endline time period
treatmentGroups <- unique(surveyData$Current_Group) %>%
  setdiff('Control')
luzeroGroup <- c('treatment_luzero_NO_SMS', 'treatment_luzero_SMS')
smsGroup <- c('treatment_SMS', 'treatment_luzero_SMS')


# something is missing here...
surveyData <- surveyData %>%
  mutate(anyTreatment = 
           ifelse(Current_Group %in% treatmentGroups & survey_time == 'endline', 1, 0),
        luzeroTreatment = 
           ifelse(Current_Group %in% luzeroGroup & survey_time == 'endline', 1, 0), 
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


###########################################
## END SECTION 1  
###########################################

###########################################
## SECTION 2 -
## CREATE BASELINE ONLY FILE
###########################################
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

baselineData <- baselineData %>%
  mutate(
    


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
View(baselineData)

baselineData <- baselineData %>% select(idVars, balanceVariables)
write_dta(baselineData, 'data/baselineData.dta', version = 13)


###########################################
## END SECTION 2
###########################################