endline <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/Metadata_endline_survey_data copy.csv")
baseline <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/Metadata_baseline_survey_data.csv")
df_data <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data//df_data.csv")
detalles_casas <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/detalles_casas.csv")
encuesta_id_nis_control <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/encuesta_id_nis_control.csv")
implementation_timeline <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/implementation_timeline.csv")


# Merging Baseline and Endline
baseline <- baseline[,-181]
baseline <- baseline[,-180]

baseline$survey_time <- "baseline"
endline$survey_time <- "endline"

baseline_endline <- rbind(baseline,endline)
control_estudio <- baseline_endline[c('control_estudio','encuesta_id','tipo_encuesta')]

# Detalles Casas
detalles_casas_v1 <- detalles_casas[c('encuesta_id','Current_Group','tariff_code','num_medidor')]


# Time Periods
df_data$date_previous_reading <- as.Date(as.character(df_data$fecha.factura.ant..), format = "%d/%m/%y")
df_data$date_current_reading <- as.Date(as.character(df_data$fecha.factura.), format = "%d/%m/%y")
df_data$num_medidor <- df_data$nis


# Implementation Time Periods
implementation_timeline$Date.Start_date <- as.Date(as.character(implementation_timeline$Date.Start), format = "%d/%m/%y")
implementation_timeline$Date.End_date <- as.Date(as.character(implementation_timeline$Date.End), format = "%d/%m/%y")


# Merging Meter Data, Treatment Group, Implementation Time Periods
# Check numbers
# length(unique(df_data$nis)) - 190
# length(unique(detalles_casas_v1$num_medidor)) - 108 (this is essentially the treatment group)
# length(unique(baseline$encuesta_id)) - 183
# length(unique(endline$encuesta_id)) - 190

# Making sure the labels are good
df_data_control_estudio <- merge(df_data,detalles_casas_v1, by='num_medidor',all=T)

# Creating the Treatment and Control Groups
treatment <-  df_data_control_estudio[!is.na(df_data_control_estudio$Current_Group),]
control <- df_data_control_estudio[is.na(df_data_control_estudio$Current_Group),]
control$Current_Group <- "Control"

        length(unique(control$num_medidor)) #83
        length(unique(control$encuesta_id)) #83

        #Adding Encuesta Id to this Group
        control_susa <- merge(control,encuesta_id_nis_control,by="num_medidor")
        names(control_susa)[23] <- "encuesta_id"
        control_var_list <- c("num_medidor", "alumbrado.publico","cargos.varios","comercializacion","csmo_energia",
        "energia_kwh", "fecha.factura.ant..","fecha.factura.", "iva","regulacion.ine","sub_alum_pub_menor_150kwh",
        "subsidio.comercializacion.menor.150.kwh", "subsidio.consumo.menor.150.kwh","subsidio.a.jubilados.con.consumo.menor.150kwh",
        "tarifa.","total","nis","date_previous_reading","date_current_reading", "encuesta_id" , "Current_Group","tariff_code")
        control <- control_susa[,control_var_list]
        
        length(unique(control$num_medidor)) #79
        length(unique(control$encuesta_id)) #79
        #NOTE: We lose 4 people because we don't have their match of encuesta_id and num_medidor
         
treatment_control <- rbind(treatment,control)

# Incorporating the Implementation Timeline
begin = '01/09/17' # Date when the Luzeros were all online and we were sending SMS, and paper energy reports
begin_date = as.Date(begin, format = "%d/%m/%y")

treatment_control$timeline <- ifelse(treatment_control$date_current_reading>begin_date,'Ongoing Experiment','No Experiment')

      length(unique(subset(treatment_control,treatment_control$Current_Group != 'Control')$num_medidor)) #108
      length(unique(subset(treatment_control,treatment_control$Current_Group != 'Control')$encuesta_id)) #116
      length(unique(subset(treatment_control,treatment_control$Current_Group == 'Control')$num_medidor)) #79
      length(unique(subset(treatment_control,treatment_control$Current_Group == 'Control')$encuesta_id)) #79


###################
###################
# Merging with Control Variables
#Keeping endline variables for merge
endline_vars <- endline[c('encuesta_id','control_estudio','tipo_encuesta','medidor','nivel_educacion','store_type','num_personas','sexo','plancha_pelo',
                          'bombillas','celular','internet','radio','television','computadora','refrigerador','abanico','aire_acondicionado',
                          'microondas','licuadora','equipo','lavadora','secadora','plancha','plancha_pelo','interest_efficiency','razon_eficiencia_energeticaI',
                          'razon_eficiencia_energeticaII','razon_eficiencia_energeticaIII','financiamiento_eficiencia','financiamiento_eficiencia_tamano','financiamiento_eficiencia_meses',
                          'financiamiento_solar','financiamiento_solar_meses','financiamiento_solar_tamano','cuenta_de_ahorros','gasto_electrico','gasto_electrico_cordobas','gasto_tarifa_electrica',
                          'gasto_agua','gasto_agua_cordobas','gas_casa','gasto_gas','ingresos_mensuales','gastos_mensuales','porcentaje_electricidad','dificultad_pago','ahorros_si_no','ahorros_cantidad',
                          'gasto_servicios','gasto_cosas_basicas','gastos_otros_no_diversion','gastos_diversion','control_de_consumo','informacion_util','control_de_consumo','informacion_util','comparte_papel_info',
                          'type_pago')]

# Making sure that value vars are good
endline_vars$num_personas <- as.numeric(as.character(endline_vars$num_personas))
endline_vars$bombillas <- as.numeric(as.character(endline_vars$bombillas))
endline_vars$celular <- as.numeric(as.character(endline_vars$celular))
endline_vars$internet <- as.numeric(as.character(endline_vars$internet))
endline_vars$radio <- as.numeric(as.character(endline_vars$radio))
endline_vars$television <- as.numeric(as.character(endline_vars$television))
endline_vars$abanico <- as.numeric(as.character(endline_vars$abanico))
endline_vars$aire_acondicionado <- as.numeric(as.character(endline_vars$aire_acondicionado))
endline_vars$microondas <- as.numeric(as.character(endline_vars$microondas))
endline_vars$licuadora <- as.numeric(as.character(endline_vars$licuadora))
endline_vars$equipo <- as.numeric(as.character(endline_vars$equipo))
endline_vars$lavadora <- as.numeric(as.character(endline_vars$lavadora))
endline_vars$secadora <- as.numeric(as.character(endline_vars$secadora))
endline_vars$plancha <- as.numeric(as.character(endline_vars$plancha))
endline_vars$plancha_pelo.1 <- as.numeric(as.character(endline_vars$plancha_pelo.1))

endline_vars$financiamiento_eficiencia_tamano <- as.numeric(as.character(endline_vars$financiamiento_eficiencia_tamano))
endline_vars$financiamiento_eficiencia_meses <- as.numeric(as.character(endline_vars$financiamiento_eficiencia_meses))
endline_vars$financiamiento_solar_meses <- as.numeric(as.character(endline_vars$financiamiento_solar_meses))
endline_vars$financiamiento_solar_tamano <- as.numeric(as.character(endline_vars$financiamiento_solar_tamano))
endline_vars$gasto_electrico_cordobas <- as.numeric(as.character(endline_vars$gasto_electrico_cordobas))
endline_vars$gasto_tarifa_electrica <- as.numeric(as.character(endline_vars$gasto_tarifa_electrica))
endline_vars$gasto_agua <- as.numeric(as.character(endline_vars$gasto_agua))
endline_vars$gasto_agua_cordobas <- as.numeric(as.character(endline_vars$gasto_agua_cordobas))
endline_vars$gasto_gas <- as.numeric(as.character(endline_vars$gasto_gas))
endline_vars$ingresos_mensuales <- as.numeric(as.character(endline_vars$ingresos_mensuales))
endline_vars$gastos_mensuales <- as.numeric(as.character(endline_vars$gastos_mensuales))
endline_vars$porcentaje_electricidad <- as.numeric(as.character(endline_vars$porcentaje_electricidad))
    endline_vars$porcentaje_electricidad[endline_vars$porcentaje_electricidad == 20000 ] <- 20
endline_vars$ahorros_cantidad <- as.numeric(as.character(endline_vars$ahorros_cantidad))
endline_vars$gasto_servicios <- as.numeric(as.character(endline_vars$gasto_servicios))
endline_vars$gasto_cosas_basicas <- as.numeric(as.character(endline_vars$gasto_cosas_basicas))
endline_vars$gastos_otros_no_diversion <- as.numeric(as.character(endline_vars$gastos_otros_no_diversion))
endline_vars$gastos_diversion <- as.numeric(as.character(endline_vars$gastos_diversion))

        length(unique(subset(endline_vars,endline_vars$control_estudio == "estudio")$encuesta_id)) #104
        length(unique(subset(endline_vars,endline_vars$control_estudio == "control")$encuesta_id)) #86

# Merge, we begin with 108 treatment with encuesta_id & num_medidor, and 79 control encuesta_id & num_medidor
treatment_control_endline <- merge(treatment_control,endline_vars,by='encuesta_id')

        length(unique(subset(treatment_control_endline,treatment_control_endline$Current_Group != "Control")$encuesta_id)) #102
        length(unique(subset(treatment_control_endline,treatment_control_endline$Current_Group != "Control")$num_medidor)) #102
        length(unique(subset(treatment_control_endline,treatment_control_endline$Current_Group == "Control")$encuesta_id)) #66
        length(unique(subset(treatment_control_endline,treatment_control_endline$Current_Group == "Control")$num_medidor)) #66


# Adding a couple of vars before finishing up
        
treatment_control_endline$plancha_pelo <- as.numeric(as.character(treatment_control_endline$plancha_pelo))
treatment_control_endline$bombillas  <- as.numeric(as.character(treatment_control_endline$bombillas))
treatment_control_endline$celular <-  as.numeric(as.character(treatment_control_endline$celular))
treatment_control_endline$internet <-  as.numeric(as.character(treatment_control_endline$internet))
treatment_control_endline$radio <-  as.numeric(as.character(treatment_control_endline$radio))
treatment_control_endline$aire_acondicionado <-  as.numeric(as.character(treatment_control_endline$aire_acondicionado))
treatment_control_endline$secadora<-  as.numeric(as.character(treatment_control_endline$secadora))
treatment_control_endline$radio <-  as.numeric(as.character(treatment_control_endline$radio ))
treatment_control_endline$television<-  as.numeric(as.character(treatment_control_endline$television))
treatment_control_endline$aire_acondicionado <-  as.numeric(as.character( treatment_control_endline$aire_acondicionado))
treatment_control_endline$computadora  <-  as.numeric(as.character(treatment_control_endline$computadora))
treatment_control_endline$refrigerador  <-  as.numeric(as.character( treatment_control_endline$refrigerador))
treatment_control_endline$abanico  <-  as.numeric(as.character(treatment_control_endline$abanico))
treatment_control_endline$microondas  <-  as.numeric(as.character(treatment_control_endline$microondas))
treatment_control_endline$licuadora <-  as.numeric(as.character( treatment_control_endline$licuadora))
treatment_control_endline$equipo <-  as.numeric(as.character(treatment_control_endline$equipo))
treatment_control_endline$lavadora <-  as.numeric(as.character(treatment_control_endline$lavadora))
treatment_control_endline$lavadora <-  as.numeric(as.character(treatment_control_endline$lavadora))
treatment_control_endline$plancha <-  as.numeric(as.character(treatment_control_endline$plancha))
treatment_control_endline$plancha_pelo.1 <-  as.numeric(as.character(treatment_control_endline$plancha_pelo.1))

        
treatment_control_endline$num_appliances <- rowSums(treatment_control_endline[31:47],na.rm=TRUE)
  

        
# Write CSVs
#write.csv(treatment_control, file = "/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/treatment_control.csv")
write.csv(treatment_control_endline, file = "/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/treatment_control_endline.csv")
        
        










