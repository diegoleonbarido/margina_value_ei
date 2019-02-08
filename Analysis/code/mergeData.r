## MERGE DATA FOR LUZERO PROJECT 
library('taRifx')
library('forcats')
library('dplyr')
library('lazyeval')
library('magrittr')
library('readxl')

# set directories for Derek
DATA <- file.path('P:', 'Data')
DATAIN <- file.path('P:', 'Data', 'Raw')
DATAOUT <- file.path('P:', 'Data', 'Cleaned')

## load datasets
baseline <- 
  read.csv(file.path(DATAIN,'baseline_survey.csv'), stringsAsFactors = F, na.strings = c('','n/a','N/A'))
endline <- 
  read.csv(file.path(DATAIN,'endline_survey.csv'), stringsAsFactors = F, na.strings = c('', 'n/a', 'N/A'))
casaDetails <- 
  read.csv(file.path(DATAIN,'detalles_casas.csv'), stringsAsFactor = F)
meterData <- 
  read.csv(file.path(DATAIN,'df_data.csv'), stringsAsFactors = F)

## load metadata for rename
sheetNames <- excel_sheets(file.path(DATA, 'Metadata.xlsx'))





## clean the baseline data 

### first grab english names from the metadata file
baselineName <- read_xlsx(file.path(DATA, 'Metadata.xlsx'), sheet = "baseline_survey") %>%
  select(1:2)

### some don't match so only keep the ones that match the metadata for now
selectVars <- match(colnames(baseline), baselineName[[1]]) %>% 
  na.omit() %>% as.numeric()
colnames <- baselineName[[2]][selectVars]
baseline <- baseline %>%
  select(selectVars) %>%
  arrange(selectVars)

baseline2 <- baseline
colnames(baseline2) <- colnames
baseline <- baseline %>%
  rename(control_estudio = treatmentAssignment) 

## select baseline characteristics to keep for analysis dataset
blVars <- 
  c('medidor',
    'sexo',
    'nivel_educacion',
    'store_type',
    'mas_gasta_tiempo',
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
    'interest_efficiency',
    'estrategias_pasadas_efficiency',
    'ingresos_mensuales',
    'gastos_mensuales',
    'porcentaje_electricidad',
    'dificultad_pago',
    'ahorros_si_no')

commonVars <- 
  c('encuesta_id',
    'tipo_encuesta',
    'tariff_code',
    'num_personas',
    'hora_pico_energia', 
    'dia_pico_energia_choices',
    'ano_pico_energia_choices',
    'epoca_consumo',
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
    'control_de_consumo',
    'como_contro_de_consumo',
    'cuanto_tiempo_dia_control',
    'cuantos_dias_control',
    'informacion_util',
    'uso_informacion_disnorte',
    'comparte_papel_info',
    'comparte_papel_info',
    'comparte_papel_info_si',
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

elVars <- 
  c('financiamiento_eficiencia',
    'financiamiento_eficiencia_tamano',
    'financiamiento_eficiencia_meses',
    'financiamiento_solar',
    'financiamiento_solar_tamano',
    'financiamiento_solar_meses',
    'cuenta_de_ahorros')
                
## now select variables and then add variable indicating baseline or endline
baseline <- baseline %>% select(blVars, commonVars) %>%
  mutate(surveyWave = 1)
endline <- endline %>% select(blVars, commonVars, elVars) %>%
  mutate(surveyWave = 2)

## do some cleaning before appending
baseline <- baseline %>% 
  mutate(bombillas = ifelse(bombillas == 'tres', '3', bombillas)) %>%
  mutate(bombillas = ifelse(bombillas == '3 bombillas y 16 Candela', '3', bombillas)) %>%
  mutate(bombillas = as.numeric(bombillas))

baseline <- baseline %>%
  mutate(celular = ifelse(celular == 'Una hora', '1', celular)) %>%
  mutate(celular = as.numeric(celular))

baseline <- baseline %>%
  mutate(internet = ifelse(internet == 'Si', '1', internet)) %>%
  mutate(internet = as.numeric(internet))

baseline <- baseline %>%
  mutate(refrigerador = ifelse(refrigerador == "Está dañado", "NA", refrigerador)) %>%
  mutate(refrigerador = as.numeric(refrigerador))

baseline <- baseline %>%
  mutate(plancha_pelo = ifelse(plancha_pelo == "1 hora a la semana", NA, plancha_pelo)) %>%
  mutate(plancha_pelo = ifelse(plancha_pelo == 150, NA, plancha_pelo)) %>%
  mutate(plancha_pelo = as.numeric(plancha_pelo))
  
baseline <- baseline %>%
  mutate(consumo_refrigerador = ifelse(consumo_refrigerador == "4t", 40, consumo_refrigerador)) %>%
  mutate(consumo_refrigerador = as.numeric(consumo_refrigerador))

baseline <- baseline %>%
  mutate(focos_eficiencia_cuanto = 
           ifelse(focos_eficiencia_cuanto == "no pagaria, es lo mismo", NA, focos_eficiencia_cuanto)) %>%
  mutate(focos_eficiencia_cuanto = 
           ifelse(focos_eficiencia_cuanto == "no pagaria", NA, focos_eficiencia_cuanto)) %>%
  mutate(focos_eficiencia_cuanto = 
           ifelse(focos_eficiencia_cuanto == "no me interesa", NA, focos_eficiencia_cuanto)) %>%
  mutate(focos_eficiencia_cuanto = as.numeric(focos_eficiencia_cuanto))
           
endline <- endline %>%
  mutate(secadora = ifelse(secadora == "1 de pelo", NA, secadora)) %>%
  mutate(secadora = as.numeric(secadora)) 

endline <- endline %>%
  mutate(consumo_abanico = ifelse(consumo_abanico == "30 incluyendo el aire", 30, consumo_abanico)) %>%
  mutate(consumo_abanico = ifelse(consumo_abanico == "5 abanicos, 20 aire acondicionado", 25, consumo_abanico)) %>%
  mutate(consumo_abanico = as.numeric(consumo_abanico))


## append baseline and endline
surveyData <- bind_rows(baseline, endline)

## now that it is merged clean the data + translate variable names to english
surveyData <- surveyData %>% 
  dplyr::rename(hasMeter = medidor) %>%
  mutate(hasMeter = fct_recode(hasMeter, yes = 'si', no = 'no'))

surveyData <- surveyData %>%
  dplyr::rename(gender = sexo) %>%
  mutate(gender = fct_recode(gender, male = 'hombre', female = 'mujer'))

surveyData <- surveyData
  dplyr::rename(educAttainment = nivel_educacion) %>%
  mutate(educAttainment = 
           fct_recode(educAttainment, 
                      universidad = 'University', 
                      ciclo_basico = ''))



surveyData <- surveyData %>%
  mutate(interest_efficiency = as.factor(interest_efficiency))


## now merge with household characteristics
analysisData <- merge(surveyData, casaDetails, by = 'encuesta_id', all = TRUE)

