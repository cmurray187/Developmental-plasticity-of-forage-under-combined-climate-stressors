###data input should be *_meta
subtract_blanks <- function(data){

#Convert MO2rates from umol/L/min to umol/hr using the seawater volume of each vial (total vial volume minus estimated total embryo volume or bead volume for blanks)
data <- data %>%
  mutate(MO2rate_umol_hr_raw = slope_umol_L_min*60*(vial_seawater_vol_ml/1000))

###Next, subtract out the oxygen change of blank vials from embryo vials. For the first trial, blank vials increased in DO slightly. 
###Take average of six blank vials, then apply to embryo vials. Last, calculate MO2 per individual embryo
blankvials <- data %>%
  filter(sample_id>8)
blankmeans <- blankvials %>%
  dplyr::summarize(Mean = mean(MO2rate_umol_hr_raw, na.rm=TRUE))
blankmeanscolumn <- rbind(blankmeans,blankmeans,blankmeans,blankmeans, blankmeans,blankmeans,blankmeans,blankmeans)
withembryos <- data%>%
  filter(sample_id<9)
withembryos <- withembryos%>%             
  mutate(MO2rate_umol_hr_blankcorrected = MO2rate_umol_hr_raw-blankmeanscolumn$Mean)
correctedMO2 <- withembryos %>%            
  mutate(MO2rate_umol_hr_indv = MO2rate_umol_hr_blankcorrected/num_embryos)
return(correctedMO2)
}