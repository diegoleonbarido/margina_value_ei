/************************************************
PROJECT: NICARAGUA EE
AUTHOR: DEREK WOLFSON 
PURPOSE: RUN ALL CLEANING FILES AND MERGE DATA
*************************************************/

/************************************************
* SECTION 0 - PREAMBLE
*************************************************/
clear all
set more off

local PROJ "G:\marginal_value_ei"
local PROJDATA "P:\Data"

/************************************************
* END SECTION 0 
*************************************************/

/************************************************
* SECTION 1 - BASELINE CLEANING
*************************************************/
import delimited using "`PROJDATA'/raw/baseline_survey.csv"

// rename the variables to english names
rename start start
rename end end
rename today today
rename deviceid deviceid
rename imei imei
rename control_estudio treatmentAssignment
rename encuesta_id surveyID
rename participacion_control participationControl
rename participacion_estudio participationTreatment
rename assigned_group assignedGroup
rename current_group currentGroup
rename tipo_encuesta respondentType
rename tariff_code tariffCode
rename num_personas hhMemberCount
rename medidor hhMeterOwnership
rename sexo gender
rename nivel_educacion educationalAttainment
rename store_type storeType
rename mas_gasta_tiempo highConsumptionPeriod
rename porque_mas_gasta_tiempo highConsumptionPeriodWhy
rename hora_pico_energia highConsumptionHour
rename dia_pico_energia_choices highConsumptionDay
rename ano_pico_energia_choices highConsumptionMonth
rename epoca_consumo highConsumptionSeason
rename bombillas aLightbulbCount
rename bombillas_horas aLightbulbHours
rename celular aCellphoneCount
rename celular_horas aCellphoneHours
rename internet aRouterCount
rename internet_horas aRouterHours
rename radio aRadioCount
rename radio_horas aRadioHours
rename television aTelevision
rename television_horas aTelevisionHours
rename computadora aComputer
rename computadora_horas aComputerHours
rename refrigerador aRefrigerator
rename refrigerador_horas aRefrigeratorHours
rename abanico aFan
rename abanico_horas aFanHour
rename aire_acondicionado aAirConditioning
rename aire_acondicionado_horas aAirConditioningHours
rename microondas aMicrowave
rename microondas_horas aMicrowaveHours
rename licuadora aBlender
rename licuadora_horas aBlenderHours
rename equipo aOtherEquipmentCount
rename equipo_horas aOtherEquipmentHours
rename lavadora aWashingMachineCount
rename lavadora_horas aWashingMachineHours
rename secadora aDryerCount
rename secadora_horas aDryerHours
rename plancha aIronCount
rename plancha_horas aIronHours
rename plancha_pelo aHairDryerCount
rename plancha_pelo_horas aHairDryerHours
rename consumo_abanico consumptionPercentFan
rename consumo_luces consumptionPercentLights
rename consumo_television consumptionPercentTV
rename consumo_celular consumptionPercentCellphone
rename consumo_radio consumptoinPercentRadio
rename consumo_refrigerador consumptionPercentFridge
rename efficiency_bulb bulbType
rename time_fridge fridgeOwnershipMonths
rename buy_fridge fridgePurchaseLocation
rename efficiency_tv tvType
rename interest_efficiency efficiencyInterest
rename estrategias_pasadas_efficiency efficiencyStrategies
rename fotos_est_eficiencia_energeticai fotos_est_eficienciaenergeticaI
rename v70 fotos_est_eficienciaenergeticaII
rename barreras_efficiency efficiencyBarriers
rename razon_eficiencia_energeticai efficiencyReasonI
rename razon_eficiencia_energeticaii efficiencyReasonII
rename razon_eficiencia_energeticaiii efficiencyReasonIII
rename gasto_electrico monthlyEnergyConsumption
rename gasto_electrico_cordobas monthlyEnergySpend
rename gasto_tarifa_electrica costPerKWH
rename gasto_agua monthlyWaterConsumption
rename gasto_agua_cordobas monthlyWaterSpend
rename gas_casa monthlyGasConsumption
rename gasto_gas monthlyGasSpend
rename ingresos_mensuales monthlyIncome
rename gastos_mensuales monthlyExpenditures
rename porcentaje_electricidad incomePercentEnergy
rename dificultad_pago billPaymentDifficulty
rename dificil_porque_pago dificil_porque_pago
rename muy_dificil_porque_pago muy_dificil_porque_pago
rename especifique_porque_dificil especifique_porque_dificil
rename especifique_porque_mdificil especifique_porque_mdificil
rename ahorros_si_no savingAbilityMonth
rename ahorros_cantidad savingAbilityAmount
rename gasto_servicios basicServicesSpend
rename gasto_cosas_basicas foodHealthEduSpend
rename gastos_otros_no_diversion otherNonLeisureSpend
rename gastos_diversion leisureSpend
rename ahorros_gastos saveOrSpendExtraMoney
rename ahorraria_si ahorraria_si
rename gastaria_si gastaria_si
rename control_de_consumo trackEnergyConsumption
rename como_contro_de_consumo trackEnergyConsumptionHow
rename cuanto_tiempo_dia_control trackEnergyConsumptionTime
rename cuantos_dias_control trackEnergyConsumptionDays
rename informacion_util trackEnergyConsumptionConserv
rename para_que_util_nada para_que_util_nada
rename para_que_util_masmenos para_que_util_masmenos
rename para_que_util_uil para_que_util_uil
rename para_que_util_muyutil para_que_util_muyutil
rename uso_informacion_disnorte trackEnergyConsumptionUse
rename comparte_papel_info trackEnergyConsumptionShare
rename comparte_papel_info_si trackEnergyConsumptionShareWhy
rename type_pago billPaymentMode
rename tiempo_pago billPaymentDuration
rename type_pago_quality billPaymentEase
rename porque_facil_pago porque_facil_pago
rename porque_relafacil_pago porque_relafacil_pago
rename porque_complicado_pago porque_complicado_pago
rename porque_muycomplicado_pago porque_muycomplicado_pago
rename tipo_telefono telephoneType
rename telefono_suyo telefono_suyo
rename tel_whatsapp tel_whatsapp
rename telefonos telefonos
rename internet_uso internet_uso
rename foto_gasto_electrico foto_gasto_electrico
rename foto_recibo foto_recibo
rename especifique_gasto_electrico especifique_gasto_electrico
rename coordenadasiii latlong
rename _coordenadasiii_latitude _coordenadasiii_latitude
rename _coordenadasiii_longitude _coordenadasiii_longitude
rename _coordenadasiii_altitude _coordenadasiii_altitude
rename _coordenadasiii_precision _coordenadasiii_precision
rename metainstanceid metainstanceid
rename _id _id
rename _uuid _uuid
rename _submission_time _submission_time
rename _index _index
rename _parent_table_name _parent_table_name
rename _parent_index _parent_index
rename _tags _tags
rename _notes _notes
rename _version _version
rename _duration _duration
rename focos_eficiencia_ahorro efficiencySavingBulb
rename focos_eficiencia_cuanto efficientcySavingBulbWTP
rename ventana_eficiencia_ahorro energySavingWindow
rename ventana_eficiencia_cuanto energySavingWindowWTP
rename botella_eficiencia_ahorro energySavingSolarBottle
rename botella_eficiencia_cuanto energySavingSolarBottleWTP
rename cortina_eficiencia_ahorro energySavingFridgeCurtain
rename cortina_eficiencia_cuanto energySavingFridgeCurtainWTP
rename refri_eficiencia_ahorro energySavingFridge
rename refri_eficiencia_cuanto energySavingFridgeWTP
rename abanico_eficiencia_ahorro energySavingFan
rename abanico_eficiencia_cuanto energySavingFanWTP
rename tv_eficiencia_ahorro energySavingTV
rename tv_eficiencia_cuanto energySavingTVWTP


// drop some bad variables
drop 	v156 v157 _coordenadasiii_latitude _coordenadasiii_longitude _coordenadasiii_altitude ///
		_coordenadasiii_precision metainstanceid _id _uuid _submission_time ///
		_index _parent_table_name _parent_index _tags _notes _version _duration
/************************************************
* END SECTION 0 
*************************************************/

