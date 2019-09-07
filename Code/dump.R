
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