# set working directory dynamically
username <- Sys.info()['user']
if(username == 'Derek'){
  setwd('G:/marginal_value_ei')
}
if(username == 'diego'){
  setwd('/Users/diego/Desktop/Projects_Code/marginal_value_ei/')
  
}
# load libraries
library(lubridate)
library(tidyr)
library(dplyr)

endline <- read.csv("Data/Metadata_endline_survey_data copy.csv")
baseline <- read.csv("Data/Metadata_baseline_survey_data.csv")
df_data <- read.csv("Data//df_data.csv")
detalles_casas <- read.csv("Data/detalles_casas.csv")
encuesta_id_nis_control <- read.csv("Data/encuesta_id_nis_control.csv")
implementation_timeline <- read.csv("Data/implementation_timeline.csv")

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
#df_data$date_previous_reading <- as.Date(as.character(df_data$fecha.factura.ant..), format = "%d/%m/%y")
#df_data$date_current_reading <- as.Date(as.character(df_data$fecha.factura.), format = "%d/%m/%y")
df_data$date_previous_reading <- dmy(df_data$fecha.factura.)
df_data$date_current_reading <- dmy(df_data$fecha.factura.ant..)


df_data$num_medidor <- df_data$nis


# Implementation Time Periods
#implementation_timeline$Date.Start_date <- as.Date(as.character(implementation_timeline$Date.Start), format = "%d/%m/%y")
#implementation_timeline$Date.End_date <- as.Date(as.character(implementation_timeline$Date.End), format = "%d/%m/%y")
implementation_timeline$Date.Start_date <- dmy(implementation_timeline$Date.Start)
implementation_timeline$Date.End_date <- dmy(implementation_timeline$Date.End)

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
        length(unique(control$encuesta_id)) #1, 83

        #Adding Encuesta Id to this Group
        control_susa <- merge(control,encuesta_id_nis_control,by="nis")
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
#begin_date <- as.Date(begin, format = "%d/%m/%y")
begin_date <- dmy(begin)

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

treatment_control_endline$date_current_reading <- dmy(treatment_control_endline$fecha.factura.)
treatment_control_endline$date_previous_reading <- dmy(treatment_control_endline$fecha.factura.ant..)
treatment_control_endline$days <- treatment_control_endline$date_current_reading - treatment_control_endline$date_previous_reading


# Dropping some meter data when the metering was off by many days and only keeping variables that were within the
# billing cycle. For example, if you do table(treatment_control_endline$days) you'll see that there are many days with
# a billing cycle of -337 or 337 days. We are only keeping billing cycles that are within 28 to 69 days

keep_days <- c(28,29,30,31,32,33,34,35,36,37,38,39,41,45,46,48,66,67,68,69)
treatment_control_endline_keep <- treatment_control_endline[treatment_control_endline$days %in% keep_days,]


# Proportion of Bills
# 12 Columns with bill proportions
treatment_control_endline_keep_prop <- treatment_control_endline_keep
treatment_control_endline_keep_prop$prop1 <- as.numeric(as.numeric(treatment_control_endline_keep_prop$days) - day(treatment_control_endline_keep_prop$date_previous_reading))/as.numeric(treatment_control_endline_keep_prop$days)
treatment_control_endline_keep_prop$prop2 <- as.numeric(day(treatment_control_endline_keep_prop$date_current_reading))/as.numeric(treatment_control_endline_keep_prop$days)

treatment_control_endline_keep_prop$jan_prop <- NA
treatment_control_endline_keep_prop$feb_prop <- NA
treatment_control_endline_keep_prop$mar_prop <- NA
treatment_control_endline_keep_prop$apr_prop <- NA
treatment_control_endline_keep_prop$may_prop <- NA
treatment_control_endline_keep_prop$jun_prop <- NA
treatment_control_endline_keep_prop$jul_prop <- NA
treatment_control_endline_keep_prop$aug_prop <- NA
treatment_control_endline_keep_prop$sep_prop <- NA
treatment_control_endline_keep_prop$oct_prop <- NA
treatment_control_endline_keep_prop$nov_prop <- NA
treatment_control_endline_keep_prop$dec_prop <- NA

treatment_control_endline_keep_prop$jan_prop1 <- NA
treatment_control_endline_keep_prop$feb_prop1 <- NA
treatment_control_endline_keep_prop$mar_prop1 <- NA
treatment_control_endline_keep_prop$apr_prop1 <- NA
treatment_control_endline_keep_prop$may_prop1 <- NA
treatment_control_endline_keep_prop$jun_prop1 <- NA
treatment_control_endline_keep_prop$jul_prop1 <- NA
treatment_control_endline_keep_prop$aug_prop1 <- NA
treatment_control_endline_keep_prop$sep_prop1 <- NA
treatment_control_endline_keep_prop$oct_prop1 <- NA
treatment_control_endline_keep_prop$nov_prop1 <- NA
treatment_control_endline_keep_prop$dec_prop1 <- NA

#Making Column Months
treatment_control_endline_keep_prop$jan_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 1,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$feb_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 2,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$mar_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 3,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$apr_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 4,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$may_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 5,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$jun_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 6,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$jul_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 7,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$aug_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 8,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$sep_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 9,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$oct_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 10,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$nov_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 11,treatment_control_endline_keep_prop$prop1,NA))
treatment_control_endline_keep_prop$dec_prop <- lapply(month(treatment_control_endline_keep_prop$date_previous_reading), function(x) ifelse(x == 12,treatment_control_endline_keep_prop$prop1,NA))

treatment_control_endline_keep_prop$jan_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 1, treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$feb_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 2,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$mar_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 3,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$apr_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 4,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$may_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 5,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$jun_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 6,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$jul_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 7,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$aug_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 8,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$sep_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 9,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$oct_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 10,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$nov_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 11,treatment_control_endline_keep_prop$prop2,NA))
treatment_control_endline_keep_prop$dec_prop1 <- lapply(month(treatment_control_endline_keep_prop$date_current_reading), function(x) ifelse(x == 12,treatment_control_endline_keep_prop$prop2,NA))

treatment_control_endline_keep_prop$jan_prop[is.na(treatment_control_endline_keep_prop$jan_prop )] <- treatment_control_endline_keep_prop$jan_prop1[is.na(treatment_control_endline_keep_prop$jan_prop)]
treatment_control_endline_keep_prop$feb_prop[is.na(treatment_control_endline_keep_prop$feb_prop )] <- treatment_control_endline_keep_prop$feb_prop1[is.na(treatment_control_endline_keep_prop$feb_prop)]
treatment_control_endline_keep_prop$mar_prop[is.na(treatment_control_endline_keep_prop$mar_prop )] <- treatment_control_endline_keep_prop$mar_prop1[is.na(treatment_control_endline_keep_prop$mar_prop)]
treatment_control_endline_keep_prop$apr_prop[is.na(treatment_control_endline_keep_prop$apr_prop )] <- treatment_control_endline_keep_prop$apr_prop1[is.na(treatment_control_endline_keep_prop$apr_prop)]
treatment_control_endline_keep_prop$may_prop[is.na(treatment_control_endline_keep_prop$may_prop )] <- treatment_control_endline_keep_prop$may_prop1[is.na(treatment_control_endline_keep_prop$may_prop)]
treatment_control_endline_keep_prop$jun_prop[is.na(treatment_control_endline_keep_prop$jun_prop )] <- treatment_control_endline_keep_prop$jun_prop1[is.na(treatment_control_endline_keep_prop$jun_prop)]
treatment_control_endline_keep_prop$jul_prop[is.na(treatment_control_endline_keep_prop$jul_prop )] <- treatment_control_endline_keep_prop$jul_prop1[is.na(treatment_control_endline_keep_prop$jul_prop)]
treatment_control_endline_keep_prop$aug_prop[is.na(treatment_control_endline_keep_prop$aug_prop )] <- treatment_control_endline_keep_prop$aug_prop1[is.na(treatment_control_endline_keep_prop$aug_prop)]
treatment_control_endline_keep_prop$sep_prop[is.na(treatment_control_endline_keep_prop$sep_prop )] <- treatment_control_endline_keep_prop$sep_prop1[is.na(treatment_control_endline_keep_prop$sep_prop)]
treatment_control_endline_keep_prop$oct_prop[is.na(treatment_control_endline_keep_prop$oct_prop )] <- treatment_control_endline_keep_prop$oct_prop1[is.na(treatment_control_endline_keep_prop$oct_prop)]
treatment_control_endline_keep_prop$nov_prop[is.na(treatment_control_endline_keep_prop$nov_prop )] <- treatment_control_endline_keep_prop$nov_prop1[is.na(treatment_control_endline_keep_prop$nov_prop)]
treatment_control_endline_keep_prop$dec_prop[is.na(treatment_control_endline_keep_prop$dec_prop )] <- treatment_control_endline_keep_prop$dec_prop1[is.na(treatment_control_endline_keep_prop$dec_prop)]


treatment_control_endline_keep_prop_ex <- treatment_control_endline_keep_prop[,1:104]

treatment_control_endline_keep_prop_ex$jan_prop  <- unlist(treatment_control_endline_keep_prop_ex$jan_prop)
treatment_control_endline_keep_prop_ex$feb_prop  <- unlist(treatment_control_endline_keep_prop_ex$feb_prop)
treatment_control_endline_keep_prop_ex$mar_prop  <- unlist(treatment_control_endline_keep_prop_ex$mar_prop)
treatment_control_endline_keep_prop_ex$apr_prop  <- unlist(treatment_control_endline_keep_prop_ex$apr_prop)
treatment_control_endline_keep_prop_ex$may_prop  <- unlist(treatment_control_endline_keep_prop_ex$may_prop)
treatment_control_endline_keep_prop_ex$jun_prop  <- unlist(treatment_control_endline_keep_prop_ex$jun_prop)
treatment_control_endline_keep_prop_ex$jul_prop  <- unlist(treatment_control_endline_keep_prop_ex$jul_prop)
treatment_control_endline_keep_prop_ex$aug_prop  <- unlist(treatment_control_endline_keep_prop_ex$aug_prop)
treatment_control_endline_keep_prop_ex$sep_prop  <- unlist(treatment_control_endline_keep_prop_ex$sep_prop)
treatment_control_endline_keep_prop_ex$oct_prop  <- unlist(treatment_control_endline_keep_prop_ex$oct_prop)
treatment_control_endline_keep_prop_ex$nov_prop  <- unlist(treatment_control_endline_keep_prop_ex$nov_prop)
treatment_control_endline_keep_prop_ex$dec_prop  <- unlist(treatment_control_endline_keep_prop_ex$dec_prop)


# select what is needed
treatment_control_endline_keep_prop_ex <- treatment_control_endline_keep_prop_ex %>% 
  select(-jan_prop1, -feb_prop1, -mar_prop1, -apr_prop1, - may_prop1, -jun_prop1, -jul_prop1)

# Write CSVs

#write.csv(treatment_control, file = "Data/treatment_control.csv")
write.csv(treatment_control_endline_keep_prop_ex, file = "Data/treatment_control_endline.csv")
