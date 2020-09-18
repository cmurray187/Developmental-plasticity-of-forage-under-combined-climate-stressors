#Run one trial at a time

calculate_slopes2 <- function(data){
  MO2_models <- df %>%
    group_by(sample_id)%>%
    do(model = lm(DO_umol_L~delta_time_mins,data = .))
  
  sample1 <- MO2_models[1, ]
  fit1 <- sample1$model[[1]]
  sample1 <- tidy(sample1$model[[1]])
  sample1 <-  sample1[2, ]
  sample1$r.squared <- signif(summary(fit1)$adj.r.squared,5)
  
  sample2 <- MO2_models[2, ]
  fit2 <- sample2$model[[1]]
  sample2 <- tidy(sample2$model[[1]])
  sample2 <-  sample2[2, ]
  sample2$r.squared <- signif(summary(fit2)$adj.r.squared,5)
  
  sample3 <- MO2_models[3, ]
  fit3 <- sample3$model[[1]]
  sample3 <- tidy(sample3$model[[1]])
  sample3 <-  sample3[2, ]
  sample3$r.squared <- signif(summary(fit3)$adj.r.squared,5)
  
  sample4 <- MO2_models[4, ]
  fit4 <- sample4$model[[1]]
  sample4 <- tidy(sample4$model[[1]])
  sample4 <-  sample4[2, ]
  sample4$r.squared <- signif(summary(fit4)$adj.r.squared,5)
  
  sample5 <- MO2_models[5, ]
  fit5 <- sample5$model[[1]]
  sample5 <- tidy(sample5$model[[1]])
  sample5 <-  sample5[2, ]
  sample5$r.squared <- signif(summary(fit5)$adj.r.squared,5)
  
  sample6 <- MO2_models[6, ]
  fit6 <- sample6$model[[1]]
  sample6 <- tidy(sample6$model[[1]])
  sample6 <-  sample6[2, ]
  sample6$r.squared <- signif(summary(fit6)$adj.r.squared,5)
  
  sample7 <- MO2_models[7, ]
  fit7 <- sample7$model[[1]]
  sample7 <- tidy(sample7$model[[1]])
  sample7 <-  sample7[2, ]
  sample7$r.squared<- signif(summary(fit7)$adj.r.squared,5)
  
  sample8 <- MO2_models[8, ]
  fit8 <- sample8$model[[1]]
  sample8 <- tidy(sample8$model[[1]])
  sample8 <-  sample8[2, ]
  sample8$r.squared <- signif(summary(fit8)$adj.r.squared,5)
  
  sample9 <- MO2_models[9, ]
  fit9 <- sample9$model[[1]]
  sample9 <- tidy(sample9$model[[1]])
  sample9 <-  sample9[2, ]
  sample9$r.squared <- signif(summary(fit9)$adj.r.squared,5)
  
  
  sample10 <- MO2_models[10, ]
  fit10 <- sample10$model[[1]]
  sample10 <- tidy(sample10$model[[1]])
  sample10 <-  sample10[2, ]
  sample10$r.squared <- signif(summary(fit10)$adj.r.squared,5)
  
  sample11 <- MO2_models[11, ]
  fit11 <- sample11$model[[1]]
  sample11 <- tidy(sample11$model[[1]])
  sample11 <-  sample11[2, ]
  sample11$r.squared <- signif(summary(fit11)$adj.r.squared,5)
  
  sample12 <- MO2_models[12, ]
  fit12 <- sample12$model[[1]]
  sample12 <- tidy(sample12$model[[1]])
  sample12 <-  sample12[2, ]
  sample12$r.squared <- signif(summary(fit12)$adj.r.squared,5)
  
  sample13 <- MO2_models[13, ]
  fit13 <- sample13$model[[1]]
  sample13 <- tidy(sample13$model[[1]])
  sample13 <-  sample13[2, ]
  sample13$r.squared <- signif(summary(fit13)$adj.r.squared,5)
  
  
  slopes <- as_tibble(rbind(sample1,sample2,sample3,sample4,sample5,sample6,sample7,sample8,sample9,sample10,sample11,sample12,sample13))
  slopes$sample_id <- seq(from = 1, to = 14)
  meta <- as_tibble(df[1:14 ,4:7])
  
  finalslopes <- meta %>%
    left_join(slopes, by = "sample_id")
  
  tankname <- as_tibble(finalslopes$tank) %>%
    slice(1:8)
  combineblanks <- as_tibble(c("blank","blank","blank","blank","blank"))
  newtanks <- rbind(tankname,combineblanks)
  finalslopes$names <- newtanks$value
  finalslopes$age_tank <-paste(finalslopes$age_dpf,"-",finalslopes$names) 
  finalslopes <- select(finalslopes,-names, -statistic,-term) %>%
    rename(slope_umol_L_min = estimate)
  finalslopes <- finalslopes %>%
    select(age_tank,everything())

      return(finalslopes)
}
