library(Hmisc)
library(lubridate)

treatment_control_endline <- read.csv("/Users/diego/Desktop/Projects_Code/marginal_value_ei/Data/treatment_control_endline.csv")

#First making sure that the dates are dates and Preparing the data

treatment_control_endline$date_previous_reading <- dmy(treatment_control_endline$fecha.factura.)
treatment_control_endline$date_current_reading <- dmy(treatment_control_endline$fecha.factura.ant..)

# Creating numeric variables
treatment_control_endline$csmo_energia_int <- as.numeric(as.character(gsub('kWh','',treatment_control_endline$csmo_energia)))
treatment_control_endline$energia_kwh <- as.numeric(as.character(treatment_control_endline$energia_kwh))/29
treatment_control_endline$total <- as.numeric(as.character(treatment_control_endline$total))/29
treatment_control_endline$tarifa <- as.numeric(as.character(treatment_control_endline$total))/29

# Creating variables we need for dates
treatment_control_endline$month <- month(treatment_control_endline$date_current_reading)
treatment_control_endline$year <- year(treatment_control_endline$date_current_reading)
dates_ongoing <- subset(treatment_control_endline,treatment_control_endline$timeline == "Ongoing Experiment")




    # Table 1. Means Tests Across Groups No Changes (kWh)
    aggregate(treatment_control_endline$csmo_energia_int, by=list(treatment_control_endline$control_estudio,treatment_control_endline$timeline,treatment_control_endline$Current_Group), FUN=mean, na.rm=TRUE)
    
    
    # Table 2. Means Tests Across Groups No Changes ($US - energy)
    aggregate(treatment_control_endline$energia_kwh, by=list(treatment_control_endline$control_estudio,treatment_control_endline$timeline,treatment_control_endline$Current_Group), FUN=mean, na.rm=TRUE)
    
    # Table 3. Means Tests Across Groups No Changes ($US - total)
    aggregate(treatment_control_endline$total, by=list(treatment_control_endline$control_estudio,treatment_control_endline$timeline,treatment_control_endline$Current_Group), FUN=mean, na.rm=TRUE)
    
    # Table 4 . Tariffs
    aggregate(treatment_control_endline$tarifa, by=list(treatment_control_endline$control_estudio,treatment_control_endline$timeline,treatment_control_endline$Current_Group), FUN=mean, na.rm=TRUE)
    



# Creating Variables for Data for a Year Before
date_201617_v1 <- subset(treatment_control_endline,(treatment_control_endline$month %in% c(9,10,11,12) &  (treatment_control_endline$year %in% c(2016))))
date_201617_v2 <- subset(treatment_control_endline,(treatment_control_endline$month %in% c(1) &  (treatment_control_endline$year %in% c(2017))))
date_201718_v1 <- subset(treatment_control_endline,(treatment_control_endline$month %in% c(9,10,11,12) &  (treatment_control_endline$year %in% c(2017))))
date_201718_v2 <- subset(treatment_control_endline,(treatment_control_endline$month %in% c(1) &  (treatment_control_endline$year %in% c(2018))))

dsub_tc_endline <- do.call("rbind", list(date_201617_v1, date_201617_v2, date_201718_v1,date_201718_v2))


    # Table 5. Means Tests Across Groups No Changes (kWh)
    aggregate(dsub_tc_endline$csmo_energia_int, by=list(dsub_tc_endline$control_estudio,dsub_tc_endline$timeline,dsub_tc_endline$Current_Group), FUN=mean, na.rm=TRUE)
    # Table 6. Means Tests Across Groups No Changes ($US - energy)
    aggregate(dsub_tc_endline$energia_kwh, by=list(dsub_tc_endline$control_estudio,dsub_tc_endline$timeline,dsub_tc_endline$Current_Group), FUN=mean, na.rm=TRUE)
    # Table 3. Means Tests Across Groups No Changes ($US - total)
    aggregate(dsub_tc_endline$total, by=list(dsub_tc_endline$control_estudio,dsub_tc_endline$timeline,dsub_tc_endline$Current_Group), FUN=mean, na.rm=TRUE)
    # Table 4 . Tariffs
    aggregate(dsub_tc_endline$tarifa, by=list(dsub_tc_endline$control_estudio,dsub_tc_endline$timeline,dsub_tc_endline$Current_Group), FUN=mean, na.rm=TRUE)

    
### Months before Sept 2016
### This was done to do a comparison to see if the effects are similar if we go back in time for the different groups
### This was hard to do since we don't have long long historical data for the control group as we have for the treatment group.
    
before_time <- subset(treatment_control_endline,treatment_control_endline$date_current_reading < dmy('01/09/2016'))
   
    # Table 5. Means Tests Across Groups No Changes (kWh)
    aggregate(before_time$csmo_energia_int, by=list(before_time$control_estudio,before_time$timeline,before_time$Current_Group), FUN=mean, na.rm=TRUE)
    # Table 6. Means Tests Across Groups No Changes ($US - energy)
    aggregate(dsub_tc_endline$energia_kwh, by=list(dsub_tc_endline$control_estudio,dsub_tc_endline$timeline,dsub_tc_endline$Current_Group), FUN=mean, na.rm=TRUE)
    # Table 3. Means Tests Across Groups No Changes ($US - total)
    aggregate(dsub_tc_endline$total, by=list(dsub_tc_endline$control_estudio,dsub_tc_endline$timeline,dsub_tc_endline$Current_Group), FUN=mean, na.rm=TRUE)
    # Table 4 . Tariffs
    aggregate(dsub_tc_endline$tarifa, by=list(dsub_tc_endline$control_estudio,dsub_tc_endline$timeline,dsub_tc_endline$Current_Group), FUN=mean, na.rm=TRUE)
    
    

# Month Diffs - they care about reducing month to month variability, so this is an important metric
    
  
this_id <- subset(treatment_control_endline,treatment_control_endline$encuesta_id==2)

this_id_ordered <- this_id[order(this_id$date_current_reading),]
# Drop Duplicate Dates
this_id_ordered <- 
  

  # Do means for 2015


    






