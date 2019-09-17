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
endline <- read.csv(file.path(dataDir, '01-raw', 'surveyData', 'Metadata_endline_survey_data.csv'))
baseline <- read.csv(file.path(dataDir, '01-raw', 'surveyData', 'Metadata_baseline_survey_data.csv'))
df_data <- read.csv(file.path(dataDir, '01-raw', 'billingData', 'df_data.csv'))
detalles_casas <- read.csv(file.path(dataDir, '01-raw', 'adminData', 'detalles_casas.csv'))
encuesta_id_nis_control <- read.csv(file.path(dataDir, '01-raw', 'adminData', 'encuesta_id_nis_control.csv'))
implementation_timeline <- read.csv(file.path(dataDir, '01-raw', 'adminData', 'implementation_timeline.csv'))

# SETUP FUNCTIONS FOR REUSE
# turn all blank cells into NA
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since if_else wont work with factors
  if_else(as.character(x)!="", as.character(x), NA_character_)
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
meterID <- houseDetails %>%
  select(encuesta_id, num_medidor)
treatmentAssignment <- detalles_casas %>%
  select(encuesta_id, Current_Group)

# implementation time (OK)

# encuesta_id_nis_control
encuesta_id_nis_control <- encuesta_id_nis_control %>%
  rename(num_medidor = nis)


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

# merge in meterID
baseline <- merge(baseline, meterID, by = 'encuesta_id', all.x = T, all.y = FALSE)

# turn blank cells into NA
baseline <- baseline %>% mutate_all(funs(empty_as_na))

# treatment indicators/study indicators
baseline <- baseline %>%
  mutate(Current_Group = if_else(Current_Group %in% c('No', 'NO'), 'Control', Current_Group)) %>%
  mutate(Current_Group = as.factor(Current_Group)) %>%
  mutate(Assigned_Group = if_else(Assigned_Group %in% c('No', 'NO'), 'Control', Assigned_Group)) %>%
  mutate(Assigned_Group = as.factor(Assigned_Group))

# merge in other Current_Group from Detalles
baseline <- merge(baseline, treatmentAssignment, by = 'encuesta_id', all.x = TRUE, all.y = FALSE)

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
  meter = if_else(medidor == 'si', 1, 0),
  meter = if_else(is.na(medidor), NA_real_, meter),
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
  refrigerador = if_else(refrigerador == "Est���\u0081 da����ado", "", refrigerador),
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

# percent of income spent on electricity
baseline <- baseline %>% mutate(
  porcentaje_electricidad = if_else(porcentaje_electricidad > 100, NA_real_, as.numeric(porcentaje_electricidad)))

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
  if_else(interest_efficiency == "estoy_muy_interesado",4,
  if_else(interest_efficiency == "me_interesa",3,
  if_else(interest_efficiency == "mas_o_menos",2,
  if_else(interest_efficiency == "no_me_interesa",1,NA_real_)))),
dificultad_pago_likert =
  if_else(dificultad_pago == "facil",4,
  if_else(dificultad_pago == "relativamente_facil",3,
  if_else(dificultad_pago == "dificil",2,
  if_else(dificultad_pago == "muy_dificil",1,NA_real_)))),
informacion_util_likert =
  if_else(informacion_util == "muy_util",4,
  if_else(informacion_util == "util",3,
  if_else(informacion_util == "mas_o_menos",2,
  if_else(informacion_util == "no_me_sirve",1,NA_real_)))),
focos_eficiencia_ahorro_likert =
  if_else(focos_eficiencia_ahorro == "mucho",3,
  if_else(focos_eficiencia_ahorro == "mas o menos",2,
  if_else(focos_eficiencia_ahorro == "poco",1,NA_real_))),
ventana_eficiencia_ahorro_likert =
  if_else(ventana_eficiencia_ahorro == "mucho",3,
  if_else(ventana_eficiencia_ahorro == "mas o menos",2,
  if_else(ventana_eficiencia_ahorro == "poco",1,NA_real_))),
botella_eficiencia_ahorro_likert =
  if_else(botella_eficiencia_ahorro == "mucho",3,
  if_else(botella_eficiencia_ahorro == "mas o menos",2,
  if_else(botella_eficiencia_ahorro == "poco",1,NA_real_))),
cortina_eficiencia_ahorro_likert =
  if_else(cortina_eficiencia_ahorro == "mucho",3,
  if_else(cortina_eficiencia_ahorro == "mas o menos",2,
  if_else(cortina_eficiencia_ahorro == "poco",1,NA_real_))),
refri_eficiencia_ahorro_likert =
  if_else(refri_eficiencia_ahorro == "mucho",3,
  if_else(refri_eficiencia_ahorro == "mas o menos",2,
  if_else(refri_eficiencia_ahorro == "poco",1,NA_real_))),
abanico_eficiencia_ahorro_likert =
  if_else(abanico_eficiencia_ahorro == "mucho",3,
  if_else(abanico_eficiencia_ahorro == "mas o menos",2,
  if_else(abanico_eficiencia_ahorro == "poco",1,NA_real_))),
tv_eficiencia_ahorro_likert =
  if_else(tv_eficiencia_ahorro == "mucho",3,
  if_else(tv_eficiencia_ahorro == "mas o menos",2,
  if_else(tv_eficiencia_ahorro == "poco",1,NA_real_))))

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

# merge in treatment groups from baseline
treatmentGroups <- baseline %>% select(Assigned_Group, Current_Group.x, encuesta_id)
endline <- endline %>% select(-Current_Group, -Assigned_Group)
endline <- merge(endline, treatmentGroups, by = 'encuesta_id', all.x = TRUE)

# merge in meterID
endline <- merge(endline, meterID, by = 'encuesta_id', all.x = TRUE, all.y = FALSE)

# merge in other Current_Group from Detalles
endline <- merge(endline, treatmentAssignment, by = 'encuesta_id', all.x = TRUE, all.y = FALSE)

# turn all blank cells into NA
endline <- endline %>% mutate_all(funs(empty_as_na))

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
  hhType_HH = if_else(tipo_encuesta == 'casa', 1, 0),
  hhType_ME = if_else(tipo_encuesta == 'micro_empresa', 1, 0),
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
  tarrifType_t0 = if_else(tariff_code == 't0', 1, 0),
  tarrifType_t1 = if_else(tariff_code == 't1', 1, 0),
  tarrifType_tjubil = if_else(tariff_code == 'tjubilados', 1, 0),
  meter = if_else(medidor == 'si', 1, 0),
  meter = if_else(is.na(medidor), NA_real_, meter),
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

# percent of income spent on electricity
endline <- endline %>% mutate(
  porcentaje_electricidad = if_else(porcentaje_electricidad > 100, NA_real_, as.numeric(porcentaje_electricidad)))

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
    if_else(interest_efficiency == "estoy_muy_interesado",4,
    if_else(interest_efficiency == "me_interesa",3,
    if_else(interest_efficiency == "mas_o_menos",2,
    if_else(interest_efficiency == "no_me_interesa",1,NA_real_)))),
  dificultad_pago_likert =
    if_else(dificultad_pago == "facil",4,
    if_else(dificultad_pago == "relativamente_facil",3,
    if_else(dificultad_pago == "dificil",2,
    if_else(dificultad_pago == "muy_dificil",1,NA_real_)))),
  informacion_util_likert =
    if_else(informacion_util == "muy_util",4,
    if_else(informacion_util == "util",3,
    if_else(informacion_util == "mas_o_menos",2,
    if_else(informacion_util == "no_me_sirve",1,NA_real_)))),
  focos_eficiencia_ahorro_likert =
    if_else(focos_eficiencia_ahorro == "mucho",3,
    if_else(focos_eficiencia_ahorro == "mas o menos",2,
    if_else(focos_eficiencia_ahorro == "poco",1,NA_real_))),
  ventana_eficiencia_ahorro_likert =
    if_else(ventana_eficiencia_ahorro == "mucho",3,
    if_else(ventana_eficiencia_ahorro == "mas o menos",2,
    if_else(ventana_eficiencia_ahorro == "poco",1,NA_real_))),
  botella_eficiencia_ahorro_likert =
    if_else(botella_eficiencia_ahorro == "mucho",3,
    if_else(botella_eficiencia_ahorro == "mas o menos",2,
    if_else(botella_eficiencia_ahorro == "poco",1,NA_real_))),
  cortina_eficiencia_ahorro_likert =
    if_else(cortina_eficiencia_ahorro == "mucho",3,
    if_else(cortina_eficiencia_ahorro == "mas o menos",2,
    if_else(cortina_eficiencia_ahorro == "poco",1,NA_real_))),
  refri_eficiencia_ahorro_likert =
    if_else(refri_eficiencia_ahorro == "mucho",3,
    if_else(refri_eficiencia_ahorro == "mas o menos",2,
    if_else(refri_eficiencia_ahorro == "poco",1,NA_real_))),
  abanico_eficiencia_ahorro_likert =
    if_else(abanico_eficiencia_ahorro == "mucho",3,
    if_else(abanico_eficiencia_ahorro == "mas o menos",2,
    if_else(abanico_eficiencia_ahorro == "poco",1,NA_real_))),
  tv_eficiencia_ahorro_likert =
    if_else(tv_eficiencia_ahorro == "mucho",3,
    if_else(tv_eficiencia_ahorro == "mas o menos",2,
    if_else(tv_eficiencia_ahorro == "poco",1,NA_real_))))

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
## SECTION 5
##  CLEAN BILLING DATA
###########################################
# add time periods to the DF data
df_data <- df_data %>%
  mutate(date_previous_reading = dmy(df_data$fecha.factura.ant..),
         date_current_reading = dmy(fecha.factura.)) %>%
  select(-fecha.factura.ant.., -fecha.factura.) %>%
  mutate(days = as.numeric(date_current_reading - date_previous_reading))

# Dropping some meter data when the metering was off by many days and only keeping variables that were within the
# billing cycle. For example, if you do table(treatment_control_endline$days) you'll see that there are many days with
# a billing cycle of -337 or 337 days. We are only keeping billing cycles that are within 28 to 69 days
df_data <- df_data %>%
  filter(between(days, 28, 69))

# rename medidor
df_data <- df_data %>%
  rename(num_medidor = nis)

# merge in household IDs for control households
df_data <- merge(df_data, encuesta_id_nis_control, by = 'num_medidor', all = T)

# merge in treatment data
df_data <-
  merge(df_data, houseDetails, by='num_medidor',all=T)

# fill in treatment as control if current group is missing
df_data <- df_data %>%
  mutate(Current_Group = if_else(is.na(Current_Group), 'Control', as.character(Current_Group)))

# add in timelin variable to determine if treatment is happening yet
begin = '01/09/17' # Date when the Luzeros were all online and we were sending SMS, and paper energy reports
begin_date = as.Date(begin, format = "%d/%m/%y")
df_data <- df_data %>% mutate(
  treatmentPeriod = if_else(date_current_reading >= begin_date, 1, 0))

# now start preparing to create proportions of data in each month
df_data <- df_data %>%
  mutate(prop1 = (days - day(date_previous_reading))/days,
         prop2 = day(date_current_reading)/days)
###########################################
# END SECTION 5
###########################################
