run_summary <- function(data){
  name <- deparse(substitute(data))
  sum <- as_tibble(c(name))%>%
    rename(run_name = value)
  sum$run_name = substr(sum$run_name,1,nchar(sum$run_name)-5)
  sum$trial_num = 1:nrow(sum)
  sum$measurement_count <- max(df$run_num)
  timeelapsed <- max(df$delta_time_mins)
  sum$total_run_time_mins <- timeelapsed
  withembryos <- data[1:8, ]
  embryomeans <- withembryos%>%
    group_by(pCO2) %>%
    dplyr::summarize(Mean = mean(slope_umol_L_min, na.rm=TRUE))
  lowCo2mean <- embryomeans$Mean[1]
  sum$lowCO2_umolLmin <-lowCo2mean
  highCo2mean <- embryomeans$Mean[2]
  sum$highCO2_umolLmin <-highCo2mean
  rsquaredembryomeans <- withembryos%>%
    group_by(pCO2) %>%
    dplyr::summarize(Mean = mean(r.squared, na.rm=TRUE))
  lowCo2rsquared <- rsquaredembryomeans$Mean[1]
  sum$lowCO2_meanrsquared <- lowCo2rsquared
  highCo2rsquared <-  rsquaredembryomeans$Mean[2]
  sum$highCO2_meanrsquared <-highCo2rsquared
  justblanks <- data %>%
    filter(sample_id>8)
  blankmeans <- justblanks%>%
    dplyr::summarize(Mean = mean(slope_umol_L_min, na.rm=TRUE))
  blanksmeans <- blankmeans$Mean[1]
  sum$blank_umolLmin <- blanksmeans
  sum <- sum %>%
    relocate(trial_num,run_name,measurement_count,total_run_time_mins,lowCO2_umolLmin,lowCO2_meanrsquared,highCO2_umolLmin,highCO2_meanrsquared,blank_umolLmin)
  return(sum)
}