##### Analysis on the treatment and control endline

library(Hmisc)
library(lubridate)

endline <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/Metadata_endline_survey_data copy.csv")
          endline_treatment <- subset(endline,endline$control_estudio=='estudio')
          endline_control <- subset(endline,endline$control_estudio=='control')
baseline <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/Metadata_baseline_survey_data.csv")
          baseline_treatment <- subset(baseline,baseline$control_estudio=='estudio')
          baseline_control <- subset(baseline,baseline$control_estudio=='control')
df_data <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data//df_data.csv")
detalles_casas <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/detalles_casas.csv")
encuesta_id_nis_control <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/encuesta_id_nis_control.csv")
implementation_timeline <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/implementation_timeline.csv")
treatment_control_endline <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/treatment_control_endline.csv")





# Baseline Characteristics

#Number of Treatment and Control and Groups
table(baseline$control_estudio) #control 106, estudio 77
table(baseline$Assigned_Group) #treatment_luzero_NO_SMS: 22, treatment_luzero_SMS: 23, treatment_PAPER: 13, treatment_SMS: 15
table(baseline$tipo_encuesta) #casa: 96, micro-epresa: 76
table(baseline$tariff_code) #t0: 105 , t1: 30, tjubilados: 30
table(baseline$sexo) #hombre: 52, #mujer: 112
table(baseline$nivel_educacion) #ciclo_basico: 20,  ciclo_diversificado: 19, primaria: 28, sin_educacion : 1, universidad: 57
table(baseline$store_type) #carniceria: 1, other: 3, pulperia: 69
table(baseline$mas_gasta_tiempo) #manana: 26, medio_dia, 17, no_se: 11, noche: 85, tarde: 23
table(baseline$porque_mas_gasta_tiempo) #manana: 26, medio_dia, 17, no_se: 11, noche: 85, tarde: 23
#Categorize:    "porque_mas_gasta_tiempo"   
table(baseline$hora_pico_energia)
#1  2  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 
#2  5  1  1  4 10 12 17 22  3 14  2  4  1  2  1 16 18  4  4  1 # People say they consume most at night, but when we ask them the times, they show that they consume more energy in the beginning of the day
table(baseline$dia_pico_energia_choices)
#domingo     lunes    martes miercoles    sabado   viernes 
# 52        13         1         3        47        12  # Sunday, consume the most - is it true?
table(baseline$ano_pico_energia_choices)
#abril    agosto diciembre   febrero     julio     junio     marzo      mayo noviembre   octubre 
#14         2       108         1         1         2        11         4         1         1 
table(baseline$epoca_consumo)
#igual invierno   verano 
#41       12      110 


# Averages
mean(baseline$num_personas, na.rm=TRUE) # 5.6 ~ 6 people per household

#Number and Hours of Consumption of Appliances
baseline$bombillas <- as.numeric(as.character(baseline$bombillas))
mean(baseline$bombillas, na.rm=TRUE) # 6. ~ 7 per household
    baseline$bombillas_horas <- as.numeric(as.character(baseline$bombillas_horas))
    mean(baseline$bombillas_horas, na.rm=TRUE) # 5.17 hours per-household
baseline$celular <- as.numeric(as.character(baseline$celular))
mean(baseline$celular, na.rm=TRUE) # 3.8 ~ 4 per household
    baseline$celular_horas <- as.numeric(as.character(baseline$celular_horas))
    mean(baseline$celular_horas, na.rm=TRUE) # 4.1 hours per-household
baseline$internet <- as.numeric(as.character(baseline$internet))
mean(baseline$internet, na.rm=TRUE) # 1 per household 
    baseline$internet_horas <- as.numeric(as.character(baseline$internet_horas))
    mean(baseline$internet_horas, na.rm=TRUE) # 16 hours per-household
baseline$radio <- as.numeric(as.character(baseline$radio))
mean(baseline$radio, na.rm=TRUE) # 1 per household 
    baseline$radio_horas <- as.numeric(as.character(baseline$radio_horas))
    mean(baseline$radio_horas, na.rm=TRUE) # 4 hours per-household
baseline$television <- as.numeric(as.character(baseline$television))
mean(baseline$television, na.rm=TRUE) # 2.3 per household 
    baseline$television_horas <- as.numeric(as.character(baseline$television_horas))
    mean(baseline$television_horas, na.rm=TRUE) # 5.5 hours per-household
baseline$computadora <- as.numeric(as.character(baseline$computadora))
mean(baseline$computadora, na.rm=TRUE) # 1.4 per household 
    baseline$computadora_horas <- as.numeric(as.character(baseline$computadora_horas))
    mean(baseline$computadora_horas, na.rm=TRUE) # 3.2 hours per-household
baseline$refrigerador <- as.numeric(as.character(baseline$refrigerador))
mean(baseline$refrigerador, na.rm=TRUE) # 1.4 per household 
    baseline$refrigerador_horas <- as.numeric(as.character(baseline$refrigerador_horas))
    mean(baseline$refrigerador_horas, na.rm=TRUE) #17 hours per-household
baseline$abanico <- as.numeric(as.character(baseline$abanico))
mean(baseline$abanico, na.rm=TRUE) # 2.8 per household 
    baseline$abanico_horas <- as.numeric(as.character(baseline$abanico_horas))
    mean(baseline$abanico_horas, na.rm=TRUE) #8.7 hours per-household
baseline$aire_acondicionado <- as.numeric(as.character(baseline$aire_acondicionado))
mean(baseline$aire_acondicionado, na.rm=TRUE) # 3 per household 
    baseline$aire_acondicionado_horas <- as.numeric(as.character(baseline$aire_acondicionado_horas))
    mean(baseline$aire_acondicionado_horas, na.rm=TRUE) #4.4 hours per-household
baseline$microondas <- as.numeric(as.character(baseline$microondas))
mean(baseline$microondas, na.rm=TRUE) # 1.04 per household 
    baseline$microondas_horas <- as.numeric(as.character(baseline$microondas_horas))
    mean(baseline$microondas_horas, na.rm=TRUE) # 0.3 hours per-household
baseline$licuadora <- as.numeric(as.character(baseline$licuadora))
mean(baseline$licuadora, na.rm=TRUE) # 1.05 per household 
    baseline$licuadora_horas <- as.numeric(as.character(baseline$licuadora_horas))
    mean(baseline$licuadora_horas, na.rm=TRUE) # 0.66 hours per-household
baseline$equipo <- as.numeric(as.character(baseline$equipo))
mean(baseline$equipo, na.rm=TRUE) # 1.33 per household 
    baseline$equipo_horas <- as.numeric(as.character(baseline$equipo_horas))
    mean(baseline$equipo_horas, na.rm=TRUE) # 6.22 hours per-household
baseline$lavadora <- as.numeric(as.character(baseline$lavadora))
mean(baseline$lavadora, na.rm=TRUE) # 1.2 per household 
    baseline$lavadora_horas <- as.numeric(as.character(baseline$lavadora_horas))
    mean(baseline$lavadora_horas, na.rm=TRUE) # 2.24 hours per-household
baseline$secadora <- as.numeric(as.character(baseline$secadora))
mean(baseline$secadora, na.rm=TRUE) # 1 per household 
    baseline$secadora_horas <- as.numeric(as.character(baseline$secadora_horas))
    mean(baseline$secadora_horas, na.rm=TRUE) # 0.5 hours per-household
baseline$plancha <- as.numeric(as.character(baseline$plancha))
mean(baseline$plancha, na.rm=TRUE) # 1.3 per household 
    baseline$plancha_horas <- as.numeric(as.character(baseline$plancha_horas))
    mean(baseline$plancha_horas, na.rm=TRUE) # 1.96 hours per-household
baseline$plancha_pelo <- as.numeric(as.character(baseline$plancha_pelo))
mean(baseline$plancha_pelo, na.rm=TRUE) # 4.48 per household 
    baseline$plancha_pelo_horas <- as.numeric(as.character(baseline$plancha_pelo_horas))
    mean(baseline$plancha_pelo_horas, na.rm=TRUE) # 0.88 hours per-household
    
# Number of people with appliances
bombillas <- length(unique(subset(baseline,baseline$bombillas!=0)$encuesta_id)) #163
celular <- length(unique(subset(baseline,baseline$celular!=0)$encuesta_id)) #163
internet <- length(unique(subset(baseline,baseline$internet!=0)$encuesta_id)) #79
radio <- length(unique(subset(baseline,baseline$radio!=0)$encuesta_id)) #83
television <- length(unique(subset(baseline,baseline$television!=0)$encuesta_id)) #160
computadora <- length(unique(subset(baseline,baseline$computadora!=0)$encuesta_id)) #76
refrigerador <- length(unique(subset(baseline,baseline$refrigerador!=0)$encuesta_id)) #141
abanico <- length(unique(subset(baseline,baseline$abanico!=0)$encuesta_id)) #155
microondas <- length(unique(subset(baseline,baseline$microondas!=0)$encuesta_id)) #42
licuadora <- length(unique(subset(baseline,baseline$licuadora!=0)$encuesta_id)) #110
equipo <- length(unique(subset(baseline,baseline$equipo!=0)$encuesta_id)) #9
lavadora <- length(unique(subset(baseline,baseline$lavadora!=0)$encuesta_id)) #54
secadora <- length(unique(subset(baseline,baseline$secadora!=0)$encuesta_id)) #9
plancha <- length(unique(subset(baseline,baseline$plancha!=0)$encuesta_id)) #128
plancha_pelo <- length(unique(subset(baseline,baseline$plancha_pelo!=0)$encuesta_id)) #43
ACs <- length(unique(subset(baseline,baseline$aire_acondicionado!=0)$encuesta_id)) #10
    
# Energy use of appliances
baseline$consumo_abanico <- as.numeric(as.character(baseline$consumo_abanico))
    mean(baseline$consumo_abanico, na.rm=TRUE) # 20%
baseline$consumo_luces <- as.numeric(as.character(baseline$consumo_luces))
    mean(baseline$consumo_luces, na.rm=TRUE) # 16%
baseline$consumo_television <- as.numeric(as.character(baseline$consumo_television))
    mean(baseline$consumo_television, na.rm=TRUE) # 17.8
baseline$consumo_celular <- as.numeric(as.character(baseline$consumo_celular))
    mean(baseline$consumo_celular, na.rm=TRUE) # 10.7
baseline$consumo_radio <- as.numeric(as.character(baseline$consumo_radio))
    mean(baseline$consumo_radio, na.rm=TRUE) # 8.14
baseline$consumo_refrigerador <- as.numeric(as.character(baseline$consumo_refrigerador))
    mean(baseline$consumo_refrigerador, na.rm=TRUE) # 40

# Turn into data baseline$buy_fridge
    
### Interest in Energy Efficiency
as.data.frame(table(baseline$interest_efficiency))
as.data.frame(table(baseline_treatment$interest_efficiency))
as.data.frame(table(baseline_control$interest_efficiency))

as.data.frame(table(endline$interest_efficiency))
as.data.frame(table(endline_treatment$interest_efficiency))
as.data.frame(table(endline_control$interest_efficiency))


# Efficiency Strategies
as.data.frame(table(baseline$estrategias_pasadas_efficiency_resp1))
as.data.frame(table(baseline_treatment$estrategias_pasadas_efficiency_resp1))
as.data.frame(table(baseline_control$estrategias_pasadas_efficiency_resp1))

as.data.frame(table(endline$estrategias_pasadas_efficiency_resp1))
as.data.frame(table(endline_treatment$estrategias_pasadas_efficiency_resp1))
as.data.frame(table(endline_control$estrategias_pasadas_efficiency_resp1))

as.data.frame(table(baseline$estrategias_pasadas_efficiency_resp2))
as.data.frame(table(baseline_treatment$estrategias_pasadas_efficiency_resp2))
as.data.frame(table(baseline_control$estrategias_pasadas_efficiency_resp2))

as.data.frame(table(endline$estrategias_pasadas_efficiency_resp2))
as.data.frame(table(endline_treatment$estrategias_pasadas_efficiency_resp2))
as.data.frame(table(endline_control$estrategias_pasadas_efficiency_resp2))

as.data.frame(table(baseline$estrategias_pasadas_efficiency_resp3))
as.data.frame(table(baseline_treatment$estrategias_pasadas_efficiency_resp3))
as.data.frame(table(baseline_control$estrategias_pasadas_efficiency_resp3))

as.data.frame(table(endline$estrategias_pasadas_efficiency_resp3))
as.data.frame(table(endline_treatment$estrategias_pasadas_efficiency_resp3))
as.data.frame(table(endline_control$estrategias_pasadas_efficiency_resp3))


# Barriers for Greater Energy Efficiency
as.data.frame(table(baseline$barreras_efficiency_resp1))
as.data.frame(table(baseline_treatment$barreras_efficiency_resp1))
as.data.frame(table(baseline_control$barreras_efficiency_resp1))

as.data.frame(table(endline$barreras_efficiency_resp1))
as.data.frame(table(endline_treatment$barreras_efficiency_resp1))
as.data.frame(table(endline_control$barreras_efficiency_resp1))
    

# Reasons for Greater Energy Efficiency
as.data.frame(table(baseline$razon_eficiencia_energeticaI))
as.data.frame(table(baseline_treatment$razon_eficiencia_energeticaI))
as.data.frame(table(baseline_control$razon_eficiencia_energeticaI))

as.data.frame(table(endline$razon_eficiencia_energeticaI))
as.data.frame(table(endline_treatment$razon_eficiencia_energeticaI))
as.data.frame(table(endline_control$razon_eficiencia_energeticaI))


# Would you want access to financing for energy efficiency
as.data.frame(table(endline$financiamiento_eficiencia))
as.data.frame(table(endline_treatment$financiamiento_eficiencia))
as.data.frame(table(endline_control$financiamiento_eficiencia))

# How much would you take out as a loan for energy efficiency?
endline$financiamiento_eficiencia_tamano <- as.numeric(as.character(endline$financiamiento_eficiencia_tamano))
mean(endline$financiamiento_eficiencia_tamano,na.rm=TRUE)/29
sd(endline$financiamiento_eficiencia_tamano,na.rm=TRUE)/29

endline_treatment$financiamiento_eficiencia_tamano <- as.numeric(as.character(endline_treatment$financiamiento_eficiencia_tamano))
mean(endline_treatment$financiamiento_eficiencia_tamano,na.rm=TRUE)/29
sd(endline_treatment$financiamiento_eficiencia_tamano,na.rm=TRUE)/29

endline_control$financiamiento_eficiencia_tamano <- as.numeric(as.character(endline_control$financiamiento_eficiencia_tamano))
mean(endline_control$financiamiento_eficiencia_tamano,na.rm=TRUE)/29
sd(endline_control$financiamiento_eficiencia_tamano,na.rm=TRUE)/29

# In how many months would you like to pay this?
mean(as.numeric(as.character(endline$financiamiento_eficiencia_meses)),na.rm=TRUE)
sd(as.numeric(as.character(endline$financiamiento_eficiencia_meses)),na.rm=TRUE)

mean(as.numeric(as.character(endline_treatment$financiamiento_eficiencia_meses)),na.rm=TRUE)
sd(as.numeric(as.character(endline_treatment$financiamiento_eficiencia_meses)),na.rm=TRUE)

mean(as.numeric(as.character(endline_control$financiamiento_eficiencia_meses)),na.rm=TRUE)
sd(as.numeric(as.character(endline_control$financiamiento_eficiencia_meses)),na.rm=TRUE)


# Would you like to have access to solar?
as.data.frame(table(endline$financiamiento_solar))
as.data.frame(table(endline_treatment$financiamiento_solar))
as.data.frame(table(endline_control$financiamiento_solar))

# How big would the solar loan be?
endline$financiamiento_solar_tamano <- as.numeric(as.character(endline$financiamiento_solar_tamano))
mean(endline$financiamiento_solar_tamano,na.rm=TRUE)/29
sd(endline$financiamiento_solar_tamano,na.rm=TRUE)/29

endline_treatment$financiamiento_solar_tamano <- as.numeric(as.character(endline_treatment$financiamiento_solar_tamano))
mean(endline_treatment$financiamiento_solar_tamano,na.rm=TRUE)/29
sd(endline_treatment$financiamiento_solar_tamano,na.rm=TRUE)/29

endline_control$financiamiento_solar_tamano <- as.numeric(as.character(endline_control$financiamiento_solar_tamano))
mean(endline_control$financiamiento_solar_tamano,na.rm=TRUE)/29
sd(endline_control$financiamiento_solar_tamano,na.rm=TRUE)/29


# In how many months would you like to pay it?
mean(as.numeric(as.character(endline$financiamiento_solar_meses)),na.rm=TRUE)
sd(as.numeric(as.character(endline$financiamiento_solar_meses)),na.rm=TRUE)

mean(as.numeric(as.character(endline_treatment$financiamiento_solar_meses)),na.rm=TRUE)
sd(as.numeric(as.character(endline_treatment$financiamiento_solar_meses)),na.rm=TRUE)

mean(as.numeric(as.character(endline_control$financiamiento_solar_meses)),na.rm=TRUE)
sd(as.numeric(as.character(endline_control$financiamiento_solar_meses)),na.rm=TRUE)

# Would you like to link your energy bill to your savings account?
as.data.frame(table(endline$cuenta_de_ahorros))
as.data.frame(table(endline_treatment$cuenta_de_ahorros))
as.data.frame(table(endline_control$cuenta_de_ahorros))

# How much energy do you consume per month?
baseline$gasto_electrico <- as.numeric(as.character(baseline$gasto_electrico))
mean(baseline$gasto_electrico,na.rm=TRUE)
sd(baseline$gasto_electrico,na.rm=TRUE)

baseline_treatment$gasto_electrico <- as.numeric(as.character(baseline_treatment$gasto_electrico))
mean(baseline_treatment$gasto_electrico,na.rm=TRUE)
sd(baseline_treatment$gasto_electrico,na.rm=TRUE)

baseline_control$gasto_electrico <- as.numeric(as.character(baseline_control$gasto_electrico))
mean(baseline_control$gasto_electrico,na.rm=TRUE)
sd(baseline_control$gasto_electrico,na.rm=TRUE)

endline$gasto_electrico <- as.numeric(as.character(endline$gasto_electrico))
mean(endline$gasto_electrico,na.rm=TRUE)
sd(endline$gasto_electrico,na.rm=TRUE)

endline_treatment$gasto_electrico <- as.numeric(as.character(endline_treatment$gasto_electrico))
mean(endline_treatment$gasto_electrico,na.rm=TRUE)
sd(endline_treatment$gasto_electrico,na.rm=TRUE)

endline_control$gasto_electrico <- as.numeric(as.character(endline_control$gasto_electrico))
mean(endline_control$gasto_electrico,na.rm=TRUE)
sd(endline_control$gasto_electrico,na.rm=TRUE)


#How many cordobas do you spend on energy?
baseline$gasto_electrico_cordobas <- as.numeric(as.character(baseline$gasto_electrico_cordobas))
mean(baseline$gasto_electrico_cordobas,na.rm=TRUE)/29
sd(baseline$gasto_electrico_cordobas,na.rm=TRUE)/29

baseline_treatment$gasto_electrico_cordobas <- as.numeric(as.character(baseline_treatment$gasto_electrico_cordobas))
mean(baseline_treatment$gasto_electrico_cordobas,na.rm=TRUE)/29
sd(baseline_treatment$gasto_electrico_cordobas,na.rm=TRUE)/29

baseline_control$gasto_electrico_cordobas <- as.numeric(as.character(baseline_control$gasto_electrico_cordobas))
mean(baseline_control$gasto_electrico_cordobas,na.rm=TRUE)/29
sd(baseline_control$gasto_electrico_cordobas,na.rm=TRUE)/29

endline$gasto_electrico_cordobas <- as.numeric(as.character(endline$gasto_electrico_cordobas))
mean(endline$gasto_electrico_cordobas,na.rm=TRUE)/29
sd(endline$gasto_electrico_cordobas,na.rm=TRUE)/29

endline_treatment$gasto_electrico_cordobas <- as.numeric(as.character(endline_treatment$gasto_electrico_cordobas))
mean(endline_treatment$gasto_electrico_cordobas,na.rm=TRUE)/29
sd(endline_treatment$gasto_electrico_cordobas,na.rm=TRUE)/29

endline_control$gasto_electrico_cordobas <- as.numeric(as.character(endline_control$gasto_electrico_cordobas))
mean(endline_control$gasto_electrico_cordobas,na.rm=TRUE)/29
sd(endline_control$gasto_electrico_cordobas,na.rm=TRUE)/29

# Do you you know much money you spend per kwh?
baseline$gasto_tarifa_electrica <- as.numeric(as.character(baseline$gasto_tarifa_electrica))
mean(baseline$gasto_tarifa_electrica,na.rm=TRUE)/29
sd(baseline$gasto_tarifa_electrica,na.rm=TRUE)/29

baseline_treatment$gasto_tarifa_electrica <- as.numeric(as.character(baseline_treatment$gasto_tarifa_electrica))
mean(baseline_treatment$gasto_tarifa_electrica,na.rm=TRUE)/29
sd(baseline_treatment$gasto_tarifa_electrica,na.rm=TRUE)/29

baseline_control$gasto_tarifa_electrica <- as.numeric(as.character(baseline_control$gasto_tarifa_electrica))
mean(baseline_control$gasto_tarifa_electrica,na.rm=TRUE)/29
sd(baseline_control$gasto_tarifa_electrica,na.rm=TRUE)/29

endline$gasto_tarifa_electrica <- as.numeric(as.character(endline$gasto_tarifa_electrica))
mean(endline$gasto_tarifa_electrica,na.rm=TRUE)/29
sd(endline$gasto_tarifa_electrica,na.rm=TRUE)/29

endline_treatment$gasto_tarifa_electrica <- as.numeric(as.character(endline_treatment$gasto_tarifa_electrica))
mean(endline_treatment$gasto_tarifa_electrica,na.rm=TRUE)/29
sd(endline_treatment$gasto_tarifa_electrica,na.rm=TRUE)/29

endline_control$gasto_tarifa_electrica <- as.numeric(as.character(endline_control$gasto_tarifa_electrica))
mean(endline_control$gasto_tarifa_electrica,na.rm=TRUE)/29
sd(endline_control$gasto_tarifa_electrica,na.rm=TRUE)/29


# How much water do you consume?
baseline$gasto_agua <- as.numeric(as.character(baseline$gasto_agua))
mean(baseline$gasto_agua,na.rm=TRUE)
sd(baseline$gasto_agua,na.rm=TRUE)

baseline_treatment$gasto_agua <- as.numeric(as.character(baseline_treatment$gasto_agua))
mean(baseline_treatment$gasto_agua,na.rm=TRUE)
sd(baseline_treatment$gasto_agua,na.rm=TRUE)

baseline_control$gasto_agua <- as.numeric(as.character(baseline_control$gasto_agua))
mean(baseline_control$gasto_agua,na.rm=TRUE)
sd(baseline_control$gasto_agua,na.rm=TRUE)

endline$gasto_agua <- as.numeric(as.character(endline$gasto_agua))
mean(endline$gasto_agua,na.rm=TRUE)
sd(endline$gasto_agua,na.rm=TRUE)

endline_treatment$gasto_agua <- as.numeric(as.character(endline_treatment$gasto_agua))
mean(endline_treatment$gasto_agua,na.rm=TRUE)
sd(endline_treatment$gasto_agua,na.rm=TRUE)

endline_control$gasto_agua <- as.numeric(as.character(endline_control$gasto_agua))
mean(endline_control$gasto_agua,na.rm=TRUE)
sd(endline_control$gasto_agua,na.rm=TRUE)

# Gasto Agua Cordobas
baseline$gasto_agua_cordobas <- as.numeric(as.character(baseline$gasto_agua_cordobas))
mean(baseline$gasto_agua_cordobas,na.rm=TRUE)/29
sd(baseline$gasto_agua_cordobas,na.rm=TRUE)/29

baseline_treatment$gasto_agua_cordobas <- as.numeric(as.character(baseline_treatment$gasto_agua_cordobas))
mean(baseline_treatment$gasto_agua_cordobas,na.rm=TRUE)/29
sd(baseline_treatment$gasto_agua_cordobas,na.rm=TRUE)/29

baseline_control$gasto_agua_cordobas <- as.numeric(as.character(baseline_control$gasto_agua_cordobas))
mean(baseline_control$gasto_agua_cordobas,na.rm=TRUE)/29
sd(baseline_control$gasto_agua_cordobas,na.rm=TRUE)/29

endline$gasto_agua_cordobas <- as.numeric(as.character(endline$gasto_agua_cordobas))
mean(endline$gasto_agua_cordobas,na.rm=TRUE)/29
sd(endline$gasto_agua_cordobas,na.rm=TRUE)/29

endline_treatment$gasto_agua_cordobas <- as.numeric(as.character(endline_treatment$gasto_agua_cordobas))
mean(endline_treatment$gasto_agua_cordobas,na.rm=TRUE)/29
sd(endline_treatment$gasto_agua_cordobas,na.rm=TRUE)/29

endline_control$gasto_agua_cordobas <- as.numeric(as.character(endline_control$gasto_agua_cordobas))
mean(endline_control$gasto_agua_cordobas,na.rm=TRUE)/29
sd(endline_control$gasto_agua_cordobas,na.rm=TRUE)/29

# What is your monthly income?
baseline$ingresos_mensuales <- as.numeric(as.character(baseline$ingresos_mensuales))
mean(baseline$ingresos_mensuales,na.rm=TRUE)/29
sd(baseline$ingresos_mensuales,na.rm=TRUE)/29

baseline_treatment$ingresos_mensuales <- as.numeric(as.character(baseline_treatment$ingresos_mensuales))
mean(baseline_treatment$ingresos_mensuales,na.rm=TRUE)/29
sd(baseline_treatment$ingresos_mensuales,na.rm=TRUE)/29

baseline_control$ingresos_mensuales <- as.numeric(as.character(baseline_control$ingresos_mensuales))
mean(baseline_control$ingresos_mensuales,na.rm=TRUE)/29
sd(baseline_control$ingresos_mensuales,na.rm=TRUE)/29

endline$ingresos_mensuales <- as.numeric(as.character(endline$ingresos_mensuales))
mean(endline$ingresos_mensuales,na.rm=TRUE)/29
sd(endline$ingresos_mensuales,na.rm=TRUE)/29

endline_treatment$ingresos_mensuales <- as.numeric(as.character(endline_treatment$ingresos_mensuales))
mean(endline_treatment$ingresos_mensuales,na.rm=TRUE)/29
sd(endline_treatment$ingresos_mensuales,na.rm=TRUE)/29

endline_control$ingresos_mensuales <- as.numeric(as.character(endline_control$ingresos_mensuales))
mean(endline_control$ingresos_mensuales,na.rm=TRUE)/29
sd(endline_control$ingresos_mensuales,na.rm=TRUE)/29

#What are your monthly expenditures?
baseline$gastos_mensuales <- as.numeric(as.character(baseline$gastos_mensuales))
mean(baseline$gastos_mensuales,na.rm=TRUE)/29
sd(baseline$gastos_mensuales,na.rm=TRUE)/29

baseline_treatment$gastos_mensuales <- as.numeric(as.character(baseline_treatment$gastos_mensuales))
mean(baseline_treatment$gastos_mensuales,na.rm=TRUE)/29
sd(baseline_treatment$gastos_mensuales,na.rm=TRUE)/29

baseline_control$gastos_mensuales <- as.numeric(as.character(baseline_control$gastos_mensuales))
mean(baseline_control$gastos_mensuales,na.rm=TRUE)/29
sd(baseline_control$gastos_mensuales,na.rm=TRUE)/29

endline$gastos_mensuales <- as.numeric(as.character(endline$gastos_mensuales))
mean(endline$gastos_mensuales,na.rm=TRUE)/29
sd(endline$gastos_mensuales,na.rm=TRUE)/29

endline_treatment$gastos_mensuales <- as.numeric(as.character(endline_treatment$gastos_mensuales))
mean(endline_treatment$gastos_mensuales,na.rm=TRUE)/29
sd(endline_treatment$gastos_mensuales,na.rm=TRUE)/29

endline_control$gastos_mensuales <- as.numeric(as.character(endline_control$gastos_mensuales))
mean(endline_control$gastos_mensuales,na.rm=TRUE)/29
sd(endline_control$gastos_mensuales,na.rm=TRUE)/29

# How hard is it for you to pay your electricity bill?
as.data.frame(table(baseline$dificultad_pago))
as.data.frame(table(baseline_treatment$dificultad_pago))
as.data.frame(table(baseline_control$dificultad_pago))

as.data.frame(table(endline$dificultad_pago))
as.data.frame(table(endline_treatment$dificultad_pago))
as.data.frame(table(endline_control$dificultad_pago))

# Why is it hard for you to pay your electricity bill?
as.data.frame(table(baseline$dificil_porque_pago))
as.data.frame(table(baseline_treatment$dificil_porque_pago))
as.data.frame(table(baseline_control$dificil_porque_pago))

as.data.frame(table(endline$dificil_porque_pago))
as.data.frame(table(endline_treatment$dificil_porque_pago))
as.data.frame(table(endline_control$dificil_porque_pago))

# Are you able to save any money by the end of the month?
as.data.frame(table(baseline$dificil_porque_pago))
as.data.frame(table(baseline_treatment$dificil_porque_pago))
as.data.frame(table(baseline_control$dificil_porque_pago))

as.data.frame(table(endline$dificil_porque_pago))
as.data.frame(table(endline_treatment$dificil_porque_pago))
as.data.frame(table(endline_control$dificil_porque_pago))

# Comparison
as.data.frame(table(baseline$ahorros_si_no))
as.data.frame(table(baseline_treatment$ahorros_si_no))
as.data.frame(table(baseline_control$ahorros_si_no))

as.data.frame(table(endline$ahorros_si_no))
as.data.frame(table(endline_treatment$ahorros_si_no))
as.data.frame(table(endline_control$ahorros_si_no))


# Do you keep track of your energy consumption?
as.data.frame(table(baseline$control_de_consumo))
as.data.frame(table(baseline_treatment$control_de_consumo))
as.data.frame(table(baseline_control$control_de_consumo))

as.data.frame(table(endline$control_de_consumo))
as.data.frame(table(endline_treatment$control_de_consumo))
as.data.frame(table(endline_control$control_de_consumo))

# How do you keep track of your consumption?
as.data.frame(table(baseline$como_contro_de_consumo_resp1))
as.data.frame(table(baseline_treatment$como_contro_de_consumo_resp1))
as.data.frame(table(baseline_control$como_contro_de_consumo_resp1))

as.data.frame(table(endline$como_contro_de_consumo_resp1))
as.data.frame(table(endline_treatment$como_contro_de_consumo_resp1))
as.data.frame(table(endline_control$como_contro_de_consumo_resp1))


# Is information useful to you?
as.data.frame(table(baseline$informacion_util))
as.data.frame(table(baseline_treatment$informacion_util))
as.data.frame(table(baseline_control$informacion_util))

as.data.frame(table(endline$informacion_util))
as.data.frame(table(endline_treatment$informacion_util))
as.data.frame(table(endline_control$informacion_util))

# Why is information not useful to you?
as.data.frame(table(baseline$para_que_util_nada_resp))
as.data.frame(table(baseline_treatment$para_que_util_nada_resp))
as.data.frame(table(baseline_control$para_que_util_nada_resp))

as.data.frame(table(endline$para_que_util_nada_resp))
as.data.frame(table(endline_treatment$para_que_util_nada_resp))
as.data.frame(table(endline_control$para_que_util_nada_resp))

# Why is it more or less useful to you?
as.data.frame(table(baseline$para_que_util_masmenos_resp))
as.data.frame(table(baseline_treatment$para_que_util_masmenos_resp))
as.data.frame(table(baseline_control$para_que_util_masmenos_resp))

as.data.frame(table(endline$para_que_util_masmenos_resp))
as.data.frame(table(endline_treatment$para_que_util_masmenos_resp))
as.data.frame(table(endline_control$para_que_util_masmenos_resp))

# Why is it useful?

as.data.frame(table(baseline$para_que_util_uil_resp))
as.data.frame(table(baseline_treatment$para_que_util_uil_resp))
as.data.frame(table(baseline_control$para_que_util_uil_resp))

as.data.frame(table(endline$para_que_util_uil_resp))
as.data.frame(table(endline_treatment$para_que_util_uil_resp))
as.data.frame(table(endline_control$para_que_util_uil_resp))

# Why is it very useful?
as.data.frame(table(baseline$para_que_util_muyutil_resp))
as.data.frame(table(baseline_treatment$para_que_util_muyutil_resp))
as.data.frame(table(baseline_control$para_que_util_muyutil_resp))

as.data.frame(table(endline$para_que_util_muyutil_resp))
as.data.frame(table(endline_treatment$para_que_util_muyutil_resp))
as.data.frame(table(endline_control$para_que_util_muyutil_resp))

# How do you use the information from Disnorte/Disur?
as.data.frame(table(baseline$uso_informacion_disnorte_resp1))
as.data.frame(table(baseline_treatment$uso_informacion_disnorte_resp1))
as.data.frame(table(baseline_control$uso_informacion_disnorte_resp1))

as.data.frame(table(endline$uso_informacion_disnorte_resp1))
as.data.frame(table(endline_treatment$uso_informacion_disnorte_resp1))
as.data.frame(table(endline_control$uso_informacion_disnorte_resp1))

# Do you share your paper energy bill with anyone else?
as.data.frame(table(baseline$comparte_papel_info))
as.data.frame(table(baseline_treatment$comparte_papel_info))
as.data.frame(table(baseline_control$comparte_papel_info))

as.data.frame(table(endline$comparte_papel_info))
as.data.frame(table(endline_treatment$comparte_papel_info))
as.data.frame(table(endline_control$comparte_papel_info))

# How do you pay your electricity bill?
as.data.frame(table(baseline$type_pago))
as.data.frame(table(baseline_treatment$type_pago))
as.data.frame(table(baseline_control$type_pago))

as.data.frame(table(endline$type_pago))
as.data.frame(table(endline_treatment$type_pago))
as.data.frame(table(endline_control$type_pago))

# How much time do you spend paying your electricity bill?
baseline$tiempo_pago <- as.numeric(as.character(baseline$tiempo_pago))
mean(baseline$tiempo_pago,na.rm=TRUE)
sd(baseline$tiempo_pago,na.rm=TRUE)

baseline_treatment$tiempo_pago <- as.numeric(as.character(baseline_treatment$tiempo_pago))
mean(baseline_treatment$tiempo_pago,na.rm=TRUE)
sd(baseline_treatment$tiempo_pago,na.rm=TRUE)

baseline_control$tiempo_pago <- as.numeric(as.character(baseline_control$tiempo_pago))
mean(baseline_control$tiempo_pago,na.rm=TRUE)
sd(baseline_control$tiempo_pago,na.rm=TRUE)

endline$tiempo_pago <- as.numeric(as.character(endline$tiempo_pago))
mean(endline$tiempo_pago,na.rm=TRUE)
sd(endline$tiempo_pago,na.rm=TRUE)

endline_treatment$tiempo_pago <- as.numeric(as.character(endline_treatment$tiempo_pago))
mean(endline_treatment$tiempo_pago,na.rm=TRUE)
sd(endline_treatment$tiempo_pago,na.rm=TRUE)

endline_control$tiempo_pago <- as.numeric(as.character(endline_control$tiempo_pago))
mean(endline_control$tiempo_pago,na.rm=TRUE)
sd(endline_control$tiempo_pago,na.rm=TRUE)

# How much do you think you would save with more energy efficient bulbs?
as.data.frame(table(baseline$focos_eficiencia_ahorro))
as.data.frame(table(baseline_treatment$focos_eficiencia_ahorro))
as.data.frame(table(baseline_control$focos_eficiencia_ahorro))

as.data.frame(table(endline$focos_eficiencia_ahorro))
as.data.frame(table(endline_treatment$focos_eficiencia_ahorro))
as.data.frame(table(endline_control$focos_eficiencia_ahorro))

# How much would you pay for more energy efficient bulbs?
baseline$focos_eficiencia_cuanto <- as.numeric(as.character(baseline$focos_eficiencia_cuanto))
mean(baseline$focos_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline$focos_eficiencia_cuanto,na.rm=TRUE)/20

baseline_treatment$focos_eficiencia_cuanto <- as.numeric(as.character(baseline_treatment$focos_eficiencia_cuanto))
mean(baseline_treatment$focos_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_treatment$focos_eficiencia_cuanto,na.rm=TRUE)/29

baseline_control$focos_eficiencia_cuanto <- as.numeric(as.character(baseline_control$focos_eficiencia_cuanto))
mean(baseline_control$focos_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_control$focos_eficiencia_cuanto,na.rm=TRUE)/29

endline$focos_eficiencia_cuanto <- as.numeric(as.character(endline$focos_eficiencia_cuanto))
mean(endline$focos_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline$focos_eficiencia_cuanto,na.rm=TRUE)/29

endline_treatment$focos_eficiencia_cuanto <- as.numeric(as.character(endline_treatment$focos_eficiencia_cuanto))
mean(endline_treatment$focos_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_treatment$focos_eficiencia_cuanto,na.rm=TRUE)/29

endline_control$focos_eficiencia_cuanto <- as.numeric(as.character(endline_control$focos_eficiencia_cuanto))
mean(endline_control$focos_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_control$focos_eficiencia_cuanto,na.rm=TRUE)/29

#How much would you pay for having more energy efficient windows?
as.data.frame(table(baseline$ventana_eficiencia_ahorro))
as.data.frame(table(baseline_treatment$ventana_eficiencia_ahorro))
as.data.frame(table(baseline_control$ventana_eficiencia_ahorro))

as.data.frame(table(endline$ventana_eficiencia_ahorro))
as.data.frame(table(endline_treatment$ventana_eficiencia_ahorro))
as.data.frame(table(endline_control$ventana_eficiencia_ahorro))

#How much would you be willing to pay for more efficient windows?
baseline$ventana_eficiencia_cuanto <- as.numeric(as.character(baseline$ventana_eficiencia_cuanto))
mean(baseline$ventana_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline$ventana_eficiencia_cuanto,na.rm=TRUE)/20

baseline_treatment$ventana_eficiencia_cuanto <- as.numeric(as.character(baseline_treatment$ventana_eficiencia_cuanto))
mean(baseline_treatment$ventana_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_treatment$ventana_eficiencia_cuanto,na.rm=TRUE)/29

baseline_control$ventana_eficiencia_cuanto <- as.numeric(as.character(baseline_control$ventana_eficiencia_cuanto))
mean(baseline_control$ventana_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_control$ventana_eficiencia_cuanto,na.rm=TRUE)/29

endline$ventana_eficiencia_cuanto <- as.numeric(as.character(endline$ventana_eficiencia_cuanto))
mean(endline$ventana_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline$ventana_eficiencia_cuanto,na.rm=TRUE)/29

endline_treatment$ventana_eficiencia_cuanto <- as.numeric(as.character(endline_treatment$ventana_eficiencia_cuanto))
mean(endline_treatment$ventana_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_treatment$ventana_eficiencia_cuanto,na.rm=TRUE)/29

endline_control$ventana_eficiencia_cuanto <- as.numeric(as.character(endline_control$ventana_eficiencia_cuanto))
mean(endline_control$ventana_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_control$ventana_eficiencia_cuanto,na.rm=TRUE)/29

# How much would the solar water bottle save you?
as.data.frame(table(baseline$botella_eficiencia_ahorro))
as.data.frame(table(baseline_treatment$botella_eficiencia_ahorro))
as.data.frame(table(baseline_control$botella_eficiencia_ahorro))

as.data.frame(table(endline$botella_eficiencia_ahorro))
as.data.frame(table(endline_treatment$botella_eficiencia_ahorro))
as.data.frame(table(endline_control$botella_eficiencia_ahorro))

# How much would you pay for a solar water bottle?
baseline$botella_eficiencia_cuanto <- as.numeric(as.character(baseline$botella_eficiencia_cuanto))
mean(baseline$botella_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline$botella_eficiencia_cuanto,na.rm=TRUE)/20

baseline_treatment$botella_eficiencia_cuanto <- as.numeric(as.character(baseline_treatment$botella_eficiencia_cuanto))
mean(baseline_treatment$botella_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_treatment$botella_eficiencia_cuanto,na.rm=TRUE)/29

baseline_control$botella_eficiencia_cuanto <- as.numeric(as.character(baseline_control$botella_eficiencia_cuanto))
mean(baseline_control$botella_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_control$botella_eficiencia_cuanto,na.rm=TRUE)/29

endline$botella_eficiencia_cuanto <- as.numeric(as.character(endline$botella_eficiencia_cuanto))
mean(endline$botella_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline$botella_eficiencia_cuanto,na.rm=TRUE)/29

endline_treatment$botella_eficiencia_cuanto <- as.numeric(as.character(endline_treatment$botella_eficiencia_cuanto))
mean(endline_treatment$botella_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_treatment$botella_eficiencia_cuanto,na.rm=TRUE)/29

endline_control$botella_eficiencia_cuanto <- as.numeric(as.character(endline_control$botella_eficiencia_cuanto))
mean(endline_control$botella_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_control$botella_eficiencia_cuanto,na.rm=TRUE)/29

#How much would a more efficient fridge save you?
as.data.frame(table(baseline$refri_eficiencia_ahorro))
as.data.frame(table(baseline_treatment$refri_eficiencia_ahorro))
as.data.frame(table(baseline_control$refri_eficiencia_ahorro))

as.data.frame(table(endline$refri_eficiencia_ahorro))
as.data.frame(table(endline_treatment$refri_eficiencia_ahorro))
as.data.frame(table(endline_control$refri_eficiencia_ahorro))

#How much would you pay for a new fridge?
baseline$refri_eficiencia_cuanto <- as.numeric(as.character(baseline$refri_eficiencia_cuanto))
mean(baseline$refri_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline$refri_eficiencia_cuanto,na.rm=TRUE)/20

baseline_treatment$refri_eficiencia_cuanto <- as.numeric(as.character(baseline_treatment$refri_eficiencia_cuanto))
mean(baseline_treatment$refri_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_treatment$refri_eficiencia_cuanto,na.rm=TRUE)/29

baseline_control$refri_eficiencia_cuanto <- as.numeric(as.character(baseline_control$refri_eficiencia_cuanto))
mean(baseline_control$refri_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_control$refri_eficiencia_cuanto,na.rm=TRUE)/29

endline$refri_eficiencia_cuanto <- as.numeric(as.character(endline$refri_eficiencia_cuanto))
mean(endline$refri_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline$refri_eficiencia_cuanto,na.rm=TRUE)/29

endline_treatment$refri_eficiencia_cuanto <- as.numeric(as.character(endline_treatment$refri_eficiencia_cuanto))
mean(endline_treatment$refri_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_treatment$refri_eficiencia_cuanto,na.rm=TRUE)/29

endline_control$refri_eficiencia_cuanto <- as.numeric(as.character(endline_control$refri_eficiencia_cuanto))
mean(endline_control$refri_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_control$refri_eficiencia_cuanto,na.rm=TRUE)/29


#How much would a new TV save you?
as.data.frame(table(baseline$tv_eficiencia_ahorro))
as.data.frame(table(baseline_treatment$tv_eficiencia_ahorro))
as.data.frame(table(baseline_control$tv_eficiencia_ahorro))

as.data.frame(table(endline$tv_eficiencia_ahorro))
as.data.frame(table(endline_treatment$tv_eficiencia_ahorro))
as.data.frame(table(endline_control$tv_eficiencia_ahorro))

#how much would you pay for a new tv?
baseline$tv_eficiencia_cuanto <- as.numeric(as.character(baseline$tv_eficiencia_cuanto))
mean(baseline$tv_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline$tv_eficiencia_cuanto,na.rm=TRUE)/20

baseline_treatment$tv_eficiencia_cuanto <- as.numeric(as.character(baseline_treatment$tv_eficiencia_cuanto))
mean(baseline_treatment$tv_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_treatment$tv_eficiencia_cuanto,na.rm=TRUE)/29

baseline_control$tv_eficiencia_cuanto <- as.numeric(as.character(baseline_control$tv_eficiencia_cuanto))
mean(baseline_control$tv_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_control$tv_eficiencia_cuanto,na.rm=TRUE)/29

endline$tv_eficiencia_cuanto <- as.numeric(as.character(endline$tv_eficiencia_cuanto))
mean(endline$tv_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline$tv_eficiencia_cuanto,na.rm=TRUE)/29

endline_treatment$tv_eficiencia_cuanto <- as.numeric(as.character(endline_treatment$tv_eficiencia_cuanto))
mean(endline_treatment$tv_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_treatment$tv_eficiencia_cuanto,na.rm=TRUE)/29

endline_control$tv_eficiencia_cuanto <- as.numeric(as.character(endline_control$tv_eficiencia_cuanto))
mean(endline_control$tv_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_control$tv_eficiencia_cuanto,na.rm=TRUE)/29

#How much more would a new fan save you?
as.data.frame(table(baseline$tv_eficiencia_ahorro))
as.data.frame(table(baseline_treatment$tv_eficiencia_ahorro))
as.data.frame(table(baseline_control$tv_eficiencia_ahorro))

as.data.frame(table(endline$tv_eficiencia_ahorro))
as.data.frame(table(endline_treatment$tv_eficiencia_ahorro))
as.data.frame(table(endline_control$tv_eficiencia_ahorro))

#How much would you pay for a new fan?
baseline$abanico_eficiencia_cuanto <- as.numeric(as.character(baseline$abanico_eficiencia_cuanto))
mean(baseline$abanico_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline$abanico_eficiencia_cuanto,na.rm=TRUE)/20

baseline_treatment$abanico_eficiencia_cuanto <- as.numeric(as.character(baseline_treatment$abanico_eficiencia_cuanto))
mean(baseline_treatment$abanico_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_treatment$abanico_eficiencia_cuanto,na.rm=TRUE)/29

baseline_control$abanico_eficiencia_cuanto <- as.numeric(as.character(baseline_control$abanico_eficiencia_cuanto))
mean(baseline_control$abanico_eficiencia_cuanto,na.rm=TRUE)/29
sd(baseline_control$abanico_eficiencia_cuanto,na.rm=TRUE)/29

endline$abanico_eficiencia_cuanto <- as.numeric(as.character(endline$abanico_eficiencia_cuanto))
mean(endline$abanico_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline$abanico_eficiencia_cuanto,na.rm=TRUE)/29

endline_treatment$abanico_eficiencia_cuanto <- as.numeric(as.character(endline_treatment$abanico_eficiencia_cuanto))
mean(endline_treatment$abanico_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_treatment$abanico_eficiencia_cuanto,na.rm=TRUE)/29

endline_control$abanico_eficiencia_cuanto <- as.numeric(as.character(endline_control$abanico_eficiencia_cuanto))
mean(endline_control$abanico_eficiencia_cuanto,na.rm=TRUE)/29
sd(endline_control$abanico_eficiencia_cuanto,na.rm=TRUE)/29



# Questions

#1. Compare when people use most energy (hour) to when they actually consume most energy (mas_gasta_tiempo, hora_pico_energia, baseline vs. endline, meter)
#2. Compare DAY  when people use most energy to when they actually consume most energy (dia_pico_energia_choices, baseline vs. endline, meter)
#3. Compare MONTH  when people use most energy to when they actually consume most energy (ano_pico_energia_choices, baseline vs. endline, meter)
#4. Season table(baseline$epoca_consumo)
