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

# SETUP FUNCTIONS FOR REUSE
# turn all blank cells into NA 
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

empty_as_zero <- function(x){
  x <- if_else(is.na(x), as.numeric(0), as.numeric(x))
}

destring <- function(x){
  gsub('.*[a-zA-Z].*', '0', x)
}

###########################################
## END SECTION 0 
###########################################


###########################################
## SECTION 1 - CLEAN ALL ADMIN DATA 
###########################################
# detalles_cases 
houseDetails <- detalles_casas %>%
  select(encuesta_id, Current_Group, tariff_code, num_medidor)

# implementation time (OK)

# encuesta_id_nis_control (OK)


###########################################
## EMD SECTION 1
###########################################

###########################################
## SECTION 2 -
##  CLEAN BASELINE
###########################################
# add survey round variable
baseline <- baseline %>% 
  mutate(surveyRound = "baseline")

# turn blank cells into NA
baseline <- baseline %>% mutate_all(funs(empty_as_na))

# treatment indicators/study indicators
baseline <- baseline %>%
  mutate(Current_Group = ifelse(Current_Group %in% c('No', 'NO'), 'Control', Current_Group)) %>%
  mutate(Current_Group = as.factor(Current_Group)) %>%
  mutate(Assigned_Group = ifelse(Assigned_Group %in% c('No', 'NO'), 'Control', Assigned_Group)) %>%
  mutate(Assigned_Group = as.factor(Assigned_Group))

# add more treatment group variables
treatmentGroups <- unique(baseline$Current_Group) %>%
  setdiff('Control') %>%
  setdiff(NA)
luzeroGroup <- c('treatment_luzero_NO_SMS', 'treatment_luzero_SMS')
smsGroup <- c('treatment_SMS', 'treatment_luzero_SMS')

baseline <- baseline %>%
  mutate(anyTreatment = 
           if_else(Current_Group %in% treatmentGroups, 1, 0),
         luzeroTreatment = 
           if_else(Current_Group %in% luzeroGroup, 1, 0), 
         luzeroTreatment = 
           if_else(Current_Group %in% luzeroGroup, 1, 0),
         smsTreatment = 
           if_else(Current_Group %in% smsGroup, 1, 0),
         luzeroSMSTreatment =
           if_else(Current_Group %in% 'treatment_luzero_SMS', 1, 0),
         luzeroNoSMSTreatment =
           if_else(Current_Group %in% 'treatment_luzero_no_SMS', 1, 0),
         paperTreatment = 
           if_else(Current_Group %in% 'treatment_PAPER', 1, 0))

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

# fill missings with zero
appliances <- c(
  'bombillas',
  'celular',
  'internet',
  'radio',
  'television',
  'computadora',
  'refrigerador',
  'abanico',
  'aire_acondicionado',
  'microondas',
  'licuadora',
  'lavadora',
  'secadora',
  'plancha',
  'plancha_pelo')

baseline <- baseline %>%
  mutate_at(appliances, empty_as_zero)
  
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
  energySaveInfo_notUseful = if_else(informacion_util == 'no_me_sirve', 1, 0))

# do you share you energy info with anyone else?
baseline <- baseline %>% mutate( 
  shareEnergyInfo = if_else(comparte_papel_info == 'si', 1, 0))

# clean 'how much willing to pay for efficient appliance...'
## change things like "not willing" to zero, respecting missing values
# how much willing to pay for new efficient appliances...

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

saveBaseline <- file.path(dataDir, '02-clean', 'baselineSurveyClean.csv')
write_csv(baseline, saveBaseline)

###########################################
# END SECTION 2 
###########################################

###########################################
## SECTION 3  
##  CLEAN ENDLINE DATA 
###########################################
# add survey round variable
endline <- endline %>% 
  mutate(surveyRound = "endline")


# turn all blank cells into NA 
endline <- endline %>% mutate_all(funs(empty_as_na))


# merge in treatment groups from baseline 
treatmentGroups <- baseline %>% select(Assigned_Group, Current_Group, encuesta_id)
endline <- endline %>% select(-Current_Group, -Assigned_Group)
endline <- merge(endline, treatmentGroups, by = 'encuesta_id')

# add more treatment group variables
treatmentGroups <- unique(endline$Current_Group) %>%
  setdiff('Control') %>%
  setdiff(NA)
luzeroGroup <- c('treatment_luzero_NO_SMS', 'treatment_luzero_SMS')
smsGroup <- c('treatment_SMS', 'treatment_luzero_SMS')

endline <- endline %>%
  mutate(anyTreatment = 
           if_else(Current_Group %in% treatmentGroups, 1, 0),
         luzeroTreatment = 
           if_else(Current_Group %in% luzeroGroup, 1, 0), 
         luzeroTreatment = 
           if_else(Current_Group %in% luzeroGroup, 1, 0),
         smsTreatment = 
           if_else(Current_Group %in% smsGroup, 1, 0),
         luzeroSMSTreatment =
           if_else(Current_Group %in% 'treatment_luzero_SMS', 1, 0),
         luzeroNoSMSTreatment =
           if_else(Current_Group %in% 'treatment_luzero_no_SMS', 1, 0),
         paperTreatment = 
           if_else(Current_Group %in% 'treatment_PAPER', 1, 0))


# household variables
endline <- endline %>% mutate(
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
endline <- endline %>% mutate(
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
endline <- endline %>% mutate(
  ## lights
  bombillas = as.numeric(as.character(bombillas)),
  ## cellular phones (OK)
  ## internet (OK)
  internet = as.numeric(as.character(internet)),
  ## radio (OK)
  ## television (OK)
  ## computer (OK)
  ## fridge (OK)
  ## fan (OK)
  ## AC (OK) 
  ## Microwave (OK)
  ## Blender (OK)
  ## equipo (leaving for now)
  ## washing machine (OK)
  ## dryer (OK)
  secadora = if_else(secadora == '1 de pelo', '0', secadora),
  secadora = as.numeric(as.character(secadora))
  ## Griddle (plancha) (OK)
  ## Hair Straightener 
) # end mutate
#

# fill missings with zero
appliances <- c(
'bombillas',
'celular',
'internet',
'radio',
'television',
'computadora',
'refrigerador',
'abanico',
'aire_acondicionado',
'microondas',
'licuadora',
'lavadora',
'secadora',
'plancha',
'plancha_pelo')

endline <- endline %>%
  mutate_at(appliances, empty_as_zero)

# energy efficiency interest
endline <- endline %>% mutate(
  efficiencyInterest_veryInterested = if_else(interest_efficiency == 'estoy_muy_interesado', 1, 0),
  efficiencyInterest_interested = if_else(interest_efficiency == 'me_interesa', 1, 0),
  efficiencyInterest_indifferent = if_else(interest_efficiency == 'mas_o_menos', 1, 0),
  efficiencyInterest_notInterested = if_else(interest_efficiency == 'no_me_interesa', 1, 0))

# difficulty paying bills
endline <- endline %>% mutate(
  billPaymentDiff_easy = if_else(dificultad_pago == 'facil', 1, 0),
  billPaymentDiff_somewhatEasy = if_else(dificultad_pago == 'relativamente_facil', 1, 0),
  billPaymentDiff_hard = if_else(dificultad_pago == 'dificil', 1, 0),
  billPaymentDiff_veryHard = if_else(dificultad_pago == 'muy_dificil', 1, 0))


# saving by end of month
endline <- endline %>% mutate(
  saveByMonth_yes = if_else(ahorros_si_no == 'si', 1, 0))

# if given more money, would you save or spend?
endline <- endline %>% mutate(
  moreMoney_save = if_else(ahorros_gastos == 'ahorraria', 1, 0))

# do ou track energy consumption?
endline <- endline %>% mutate(
  trackEnergy_yes = if_else(control_de_consumo == 'si', 1, 0))

# How useful is information for energy saving?
endline <- endline %>% mutate(
  energySaveInfo_very = if_else(informacion_util == 'muy_util', 1, 0),
  energySaveInfo_somewhat = if_else(informacion_util == 'util', 1, 0),
  energySaveInfo_indiff = if_else(informacion_util == 'mas_o_menos', 1, 0),
  energySaveInfo_notUseful = if_else(informacion_util == 'no_me_sirve', 1, 0))

# do you share you energy info with anyone else?
endline <- endline %>% mutate( 
  shareEnergyInfo = if_else(comparte_papel_info == 'si', 1, 0))

# clean 'how much willing to pay for efficient appliance...'
## change things like "not willing" to zero, respecting missing values
# how much willing to pay for new efficient appliances...

endline <- endline %>%
  mutate_at(vars(matches("eficiencia_cuanto")), destring) %>%
  mutate_at(vars(matches("eficiencia_cuanto")), as.numeric)
  # will get warning from last line, worth double checking again if rebuilding
  # but should be OK, just making null results NA

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

saveEndline <- file.path(dataDir, '02-clean', 'endlineSurveyClean.csv')
write_csv(endline, saveEndline)

###########################################
# END SECTION 3
###########################################

###########################################
## SECTION 4  
##  CREATE SURVEY PANEL DATA
###########################################
# append endline data to baseline data, while removing variables that are problematic
removeVars <- c(
  'consumo_abanico', 
  'consumo_refrigerador', 
  'telefonos', 
   'X_coordenadasIII_precision')
endline <- endline %>% select(-removeVars)
baseline <- baseline %>% select(-removeVars)
surveyPanel <- bind_rows(baseline, endline)

# now change treatment indicator to respect the panel nature of the implementation 
surveyPanel <- surveyPanel %>%
  mutate(anyTreatment = 
           if_else(Current_Group %in% treatmentGroups & surveyRound == 'endline', 1, 0),
         luzeroTreatment = 
           if_else(Current_Group %in% luzeroGroup & surveyRound == 'endline', 1, 0), 
         luzeroTreatment = 
           if_else(Current_Group %in% luzeroGroup & surveyRound == 'endline', 1, 0),
         smsTreatment = 
           if_else(Current_Group %in% smsGroup & surveyRound == 'endline', 1, 0),
         luzeroSMSTreatment =
           if_else(Current_Group %in% 'treatment_luzero_SMS' & surveyRound == 'endline', 1, 0),
         luzeroNoSMSTreatment =
           if_else(Current_Group %in% 'treatment_luzero_no_SMS' & surveyRound == 'endline', 1, 0),
         paperTreatment = 
           if_else(Current_Group %in% 'treatment_PAPER' & surveyRound == 'endline', 1, 0))

savePanel <- file.path(dataDir, '02-clean', 'panelSurveyClean.csv')
write_csv(surveyPanel, savePanel)

###########################################
# END SECTION 4
###########################################

###########################################
## SECTION 4  
##  CLEAN ELECTRICITY USE DATA
###########################################
df_data <- df_data %>% 




# Dropping some meter data when the metering was off by many days and only keeping variables that were within the
# billing cycle. For example, if you do table(treatment_control_endline$days) you'll see that there are many days with
# a billing cycle of -337 or 337 days. We are only keeping billing cycles that are within 28 to 69 days




keep_days <- keep_days <- c(28,29,30,31,32,33,34,35,36,37,38,39,41,45,46,48,66,67,68,69)


