##### Analysis on the treatment and control endline

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






# Comparison


table(treatment_control_endline$Current_Group)


# Questions

#1. Compare when people use most energy (hour) to when they actually consume most energy (mas_gasta_tiempo, hora_pico_energia, baseline vs. endline, meter)
#2. Compare DAY  when people use most energy to when they actually consume most energy (dia_pico_energia_choices, baseline vs. endline, meter)
#3. Compare MONTH  when people use most energy to when they actually consume most energy (ano_pico_energia_choices, baseline vs. endline, meter)
#4. Season table(baseline$epoca_consumo)
