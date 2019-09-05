
use "G:\marginal_value_ei\Data\baselineData.dta", clear

local VARS hhType_HH hhType_ME /// household type
tarrifType_t0 tarrifType_t1 tarrifType_tjubil /// tarrif type
meter /// has meter
hhh_female /// female household head
edu_basico edu_diversificado edu_primaria edu_sin edu_univ  /// education
efficiencyInt_veryInterested efficiencyInt_interested efficiencyInt_indifferent efficiencyInt_notInterested /// interested efficiency
billPaymentDiff_easy billPaymentDiff_somewhatEasy billPaymentDiff_hard billPaymentDiff_veryHard ///
energyMore_morning energyMore_midday energyMore_afternoon energyMore_night energyMore_noSe ///
saveByMonth_yes moreMoney_save trackEnergy_yes ///
energyShareInfo_yes ///
energySaveInfo_very energySaveInfo_somewhat energySaveInfo_indiff energySaveInfo_notUseful ///
ventana_eficiencia_cuanto cortina_eficiencia_cuanto abanico_eficiencia_cuanto ///
gasto_electrico gasto_electrico_cordobas gasto_tarifa_electrica ///
gasto_agua gasto_agua_cordobas gasto_gas ///	
ingresos_mensuales gastos_mensuales ///
porcentaje_electricidad ///
gasto_servicios gasto_cosas_basicas gastos_otros_no_diversion gastos_diversion //


qui orth_out `VARS' using "G:\marginal_value_ei\output\BaselinebalanceTest", by(Current_Group) pcompare replace
mdesc `VARS'
