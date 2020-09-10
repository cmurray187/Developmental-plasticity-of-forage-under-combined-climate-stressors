append_metadata <- function(data){
  tempmeta <- filter(MO2_metadata, event_id == trial_id) %>%
    select(trial_id,temp,temp_treatment,num_embryos,vial_seawater_vol_ml,unique_id,avg_embryo_vol_ml,
           avg_embryo_SA_mm2)
  tempslopes <- data[,1:9]  
  metadata <- cbind(tempslopes,tempmeta) %>% 
    relocate(trial_id, unique_id, age_tank, sample_id,age_dpf,tank,pCO2,temp,temp_treatment,num_embryos,vial_seawater_vol_ml,avg_embryo_vol_ml, avg_embryo_SA_mm2, slope_umol_L_min, 
             std.error, p.value, r.squared)
  return(metadata)
}