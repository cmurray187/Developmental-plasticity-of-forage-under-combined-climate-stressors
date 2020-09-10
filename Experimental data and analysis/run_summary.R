run_summary <- function(data){
  name <- deparse(substitute(data))
  sum <- as_tibble(c(name))%>%
    rename(run_name = value)
  sum$measurement_count <- max(df$run_num)
  timeelapsed <- max(df$delta_time_mins)
  sum$total_run_time_mins <- timeelapsed
  withembryos <- data[1:8, ]
  embryomeans <- withembryos%>%
    group_by(pCO2) %>%
    dplyr::summarize(Mean = mean(slope_umol_L_min, na.rm=TRUE))
  lowCo2mean <- embryomeans$Mean[1]
  sum$lowCO2mean_umolLmin <-lowCo2mean
  highCo2mean <- embryomeans$Mean[2]
  sum$highCO2_umolLmin <-highCo2mean
  justblanks <- data %>%
    filter(sample_id>8)
  blankmeans <- justblanks%>%
    #group_by(pCO2) %>%
    dplyr::summarize(Mean = mean(slope_umol_L_min, na.rm=TRUE))
  blanksmeans <- blankmeans$Mean[1]
  sum$blankMO2_umolLmin <- blanksmeans
  #lowCo2mean <- blankmeans$Mean[1]
  #sum$blanklowCO2_umolLmin <-lowCo2mean
  #highCo2mean <- blankmeans$Mean[2]
  #sum$blankhighCO2_umolLmin <-highCo2mean
  return(sum)
}