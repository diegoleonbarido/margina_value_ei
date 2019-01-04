analyze_financing <- read.csv('/Users/diego/Downloads/hola.csv')

treat <- subset(analyze_financing,analyze_financing$control_estudio=='estudio')
treat_solar <- subset(treat,treat$financiamiento_solar == 'si')
treat_finsi <- subset(treat,treat$financiamiento_eficiencia == 'si')

length(treat_finsi$start) #71% access to financing for efficient appliances
length(treat_solar$start) #77% access to financing for solar
length(treat$start) #

# Solar and Efficiency 
treat_finsi$financiamiento_eficiencia_tamano <- as.numeric(as.character(treat_finsi$financiamiento_eficiencia_tamano))
treat_finsi$financiamiento_eficiencia_meses <- as.numeric(as.character(treat_finsi$financiamiento_eficiencia_meses))
treat_finsi$financiamiento_solar_tamano <- as.numeric(as.character(treat_finsi$financiamiento_solar_tamano))
treat_finsi$financiamiento_solar_meses <- as.numeric(as.character(treat_finsi$financiamiento_solar_meses))

# Appliances
treat_finsi$focos_eficiencia_cuanto <- as.numeric(as.character(treat_finsi$focos_eficiencia_cuanto))
treat_finsi$ventana_eficiencia_cuanto <- as.numeric(as.character(treat_finsi$ventana_eficiencia_cuanto))
treat_finsi$botella_eficiencia_cuanto <- as.numeric(as.character(treat_finsi$botella_eficiencia_cuanto))
treat_finsi$cortina_eficiencia_cuanto <- as.numeric(as.character(treat_finsi$cortina_eficiencia_cuanto))
treat_finsi$refri_eficiencia_cuanto <- as.numeric(as.character(treat_finsi$refri_eficiencia_cuanto))
treat_finsi$abanico_eficiencia_cuanto <- as.numeric(as.character(treat_finsi$abanico_eficiencia_cuanto))
treat_finsi$tv_eficiencia_cuanto <- as.numeric(as.character(treat_finsi$tv_eficiencia_cuanto))


mean(treat_finsi$financiamiento_eficiencia_tamano, na.rm=TRUE)/30
#Average size of the loan is $US 1,400
mean(treat_finsi$financiamiento_eficiencia_meses, na.rm=TRUE)
#Arage number of repayment months is 21 months
mean(treat_finsi$financiamiento_solar_tamano, na.rm=TRUE)/30
#Average size of solar loan is $US 953
mean(treat_finsi$financiamiento_solar_meses, na.rm=TRUE)
#Average size of loan is 22 months

mean(treat_finsi$focos_eficiencia_cuanto, na.rm=TRUE)/30
mean(treat_finsi$ventana_eficiencia_cuanto, na.rm=TRUE)/30
mean(treat_finsi$botella_eficiencia_cuanto, na.rm=TRUE)/30
mean(treat_finsi$cortina_eficiencia_cuanto, na.rm=TRUE)/30
mean(treat_finsi$refri_eficiencia_cuanto, na.rm=TRUE)/30
mean(treat_finsi$abanico_eficiencia_cuanto, na.rm=TRUE)/30
mean(treat_finsi$tv_eficiencia_cuanto, na.rm=TRUE)/30

#Summary. In the treatment group 71% of people wanted access for more efficient appliances and 77% for financing solar
#  The average loan size for energy efficiency was $US 1,400 over a period of 21 months, and $953 for solar over 
# a period of 22 months.




##############  Control

control <- subset(analyze_financing,analyze_financing$control_estudio=='control')
control_solar <- subset(control,control$financiamiento_solar == 'si')
control_finsi <- subset(control,control$financiamiento_eficiencia == 'si')

control_finsi$financiamiento_eficiencia_tamano <- as.numeric(as.character(control_finsi$financiamiento_eficiencia_tamano))
control_finsi$financiamiento_eficiencia_meses <- as.numeric(as.character(control_finsi$financiamiento_eficiencia_meses))
control_finsi$financiamiento_solar_tamano <- as.numeric(as.character(control_finsi$financiamiento_solar_tamano))
control_finsi$financiamiento_solar_meses <- as.numeric(as.character(control_finsi$financiamiento_solar_meses))


control_finsi$focos_eficiencia_cuanto <- as.numeric(as.character(control_finsi$focos_eficiencia_cuanto))
control_finsi$ventana_eficiencia_cuanto <- as.numeric(as.character(control_finsi$ventana_eficiencia_cuanto))
control_finsi$botella_eficiencia_cuanto <- as.numeric(as.character(control_finsi$botella_eficiencia_cuanto))
control_finsi$cortina_eficiencia_cuanto <- as.numeric(as.character(control_finsi$cortina_eficiencia_cuanto))
control_finsi$refri_eficiencia_cuanto <- as.numeric(as.character(control_finsi$refri_eficiencia_cuanto))
control_finsi$abanico_eficiencia_cuanto <- as.numeric(as.character(control_finsi$abanico_eficiencia_cuanto))
control_finsi$tv_eficiencia_cuanto <- as.numeric(as.character(control_finsi$tv_eficiencia_cuanto))


length(control_finsi$start) #40% access to financing
length(control_solar$start) #61% access to financing
length(control$start)



mean(control_finsi$financiamiento_eficiencia_tamano, na.rm=TRUE)/30
#Average size of the loan is $807.74
mean(control_finsi$financiamiento_eficiencia_meses, na.rm=TRUE)
#Arage number of repayment months is 17 months
mean(control_finsi$financiamiento_solar_tamano, na.rm=TRUE)/30
#Average size of solar loan is $US 642
mean(control_finsi$financiamiento_solar_meses, na.rm=TRUE)
#Average size of loan is 12 months



mean(control_finsi$focos_eficiencia_cuanto, na.rm=TRUE)/30
mean(control_finsi$ventana_eficiencia_cuanto, na.rm=TRUE)/30
mean(control_finsi$botella_eficiencia_cuanto, na.rm=TRUE)/30
mean(control_finsi$cortina_eficiencia_cuanto, na.rm=TRUE)/30
mean(control_finsi$refri_eficiencia_cuanto, na.rm=TRUE)/30
mean(control_finsi$abanico_eficiencia_cuanto, na.rm=TRUE)/30
mean(control_finsi$tv_eficiencia_cuanto, na.rm=TRUE)/30

#Summary. In the treatment group 40% of people wanted access for more efficient appliances and 61% for financing solar
#  The average loan size for energy efficiency was $US 807 over a period of 17 months, and $642 for solar over 
# a period of 12 months.




					
