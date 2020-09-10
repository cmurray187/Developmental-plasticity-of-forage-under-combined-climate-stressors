####The MO2 ANALYSIS####

###Packages----
library(tidyverse)
library(readxl)
library(broom)
library(tidymodels)
library(nlme)
library(performance)

###Source function----
source("calculate_slopes.R")
source("run_summary.R")
source("append_metadata.R")
source("subtract_blanks.R")

###Data----
MO2_metadata <- read_excel("Exp3_embryoMO2_metadata.xlsx", na = "NA")
MO2_data <- read_excel("Exp3_embryoMO2_data.xlsx", na = "NA")

###Trial 1: 6 dpf heatwave temp----
df <- as_tibble(MO2_data)%>%
  filter(trial_id == 1)%>%
  filter(run_num > 0)###Subset data if not all runs are wanted

###Plots
ggplot(df) +
  aes(x=delta_time_mins, y=DO_umol_L,) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~tank) +
  labs(title="Embryo MO2 heatwave temperature 6dpf", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()+
  xlab("Run time (mins)")+ylab("DO (umol/L)")+
  xlim(0,150)+ylim(200,315)+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        plot.title=element_text(size=28,face="bold"),
        plot.caption=element_text(size=16),
        strip.text = element_text(size = 20))

#ggsave("MO2slopes_heatwave_6dpf.jpeg", path = "figs", width = 10, height =12, dpi = 1200)

###Calculate slopes for oxygen change for each replicate and blank
MO2slopes_heatwave_6dpf_fits <- calculate_slopes(df)
###Note the slopes and fits for each measurement
#view(MO2slopes_heatwave_6dpf)

###Create a run summary report
MO2slopes_heatwave_6dpf_runsummary <- run_summary(MO2slopes_heatwave_6dpf_fits)
#view(MO2slopes_heatwave_6dpf_runsummary)

###Append metadata to slope measurements and remove fit statistics
event_id = 1
MO2slopes_heatwave_6dpf_meta <- append_metadata(MO2slopes_heatwave_6dpf_fits)
#view(MO2slopes_heatwave_6dpf_meta)

###Next convert MO2 rates to umol/hr, subtract out blank respiration rates, and present MO2 as umol/hr/individual.
###Convert MO2rates from umol/L/min to umol/hr using the seawater volume of each vial (total vial volume minus estimated total embryo volume or bead volume for blanks)
MO2slopes_heatwave_6dpf_final <- subtract_blanks(MO2slopes_heatwave_6dpf_meta)
#view(MO2slopes_heatwave_6dpf_final)


###End run

###Trial 2: 6 dpf ambient temp----
#set data
df <- as_tibble(MO2_data)%>%
  filter(trial_id == 2)%>%
  filter(run_num > 0)###Subset data if not all runs are wanted

###Plots
ggplot(df) +
  aes(x=delta_time_mins, y=DO_umol_L) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~tank) +
  labs(title="Embryo MO2 ambient temperature 6dpf", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()+
  xlab("Run time (mins)")+ylab("DO (umol/L)")+
  xlim(0,150)+ylim(230,315)+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        plot.title=element_text(size=28,face="bold"),
        plot.caption=element_text(size=16),
        strip.text = element_text(size = 20))

#ggsave("MO2slopes_ambienttemp_6dpf.jpeg", path = "figs", width = 10, height =12, dpi = 1200)

###Calculate slopes for oxygen change for each replicate and blank
MO2slopes_ambienttemp_6dpf_fits <- calculate_slopes(df)
###Note the slopes and fits for each measurement
#view(MO2slopes_ambienttemp_6dpf)

###Create a run summary report
MO2slopes_ambienttemp_6dpf_runsummary <- run_summary(MO2slopes_ambienttemp_6dpf_fits)
#view(MO2slopes_ambienttemp_6dpf_runsummary)

###Append metadata to slope measurements and remove fit statistics
event_id = 2
MO2slopes_ambienttemp_6dpf_meta <- append_metadata(MO2slopes_ambienttemp_6dpf_fits)
#view(MO2slopes_ambienttemp_6dpf_meta)

###Next convert MO2 rates to umol/hr, subtract out blank respiration rates, and present MO2 as umol/hr/individual.
###Convert MO2rates from umol/L/min to umol/hr using the seawater volume of each vial (total vial volume minus estimated total embryo volume or bead volume for blanks)
MO2slopes_ambienttemp_6dpf_final <- subtract_blanks(MO2slopes_ambienttemp_6dpf_meta)
###For this trial, blank DO levels actually increased during measurements, so embryo MO2 actually increased by correcting for blanks. 
#view(MO2slopes_ambienttemp_6dpf_final)

###End run



###Trial 3: 7 dpf heatwave temp----
df <- as_tibble(MO2_data)%>%
  filter(trial_id == 3)%>%
  filter(run_num > 0)###Subset data if not all runs are wanted

###Plots
ggplot(df) +
  aes(x=delta_time_mins, y=DO_umol_L) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~tank) +
  labs(title="Embryo MO2 heatwave temperature 7dpf", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()+
  xlab("Run time (mins)")+ylab("DO (umol/L)")+
  xlim(0,110)+ylim(200,300)+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        plot.title=element_text(size=28,face="bold"),
        plot.caption=element_text(size=16),
        strip.text = element_text(size = 20))

#ggsave("MO2slopes_heatwave_7dpf.jpeg", path = "figs", width = 10, height =12, dpi = 1200)

###Calculate slopes for oxygen change for each replicate and blank
MO2slopes_heatwave_7dpf_fits <- calculate_slopes(df)
###Note the slopes and fits for each measurement
#view(MO2slopes_heatwave_7dpf)

###Create a run summary report
MO2slopes_heatwave_7dpf_runsummary <- run_summary(MO2slopes_heatwave_7dpf_fits)
#view(MO2slopes_heatwave_7dpf_runsummary)

###Append metadata to slope measurements and remove fit statistics
event_id = 3
MO2slopes_heatwave_7dpf_meta <- append_metadata(MO2slopes_heatwave_7dpf_fits)
#view(MO2slopes_heatwave_7dpf_meta)

###Next convert MO2 rates to umol/hr, subtract out blank respiration rates, and present MO2 as umol/hr/individual.
###Convert MO2rates from umol/L/min to umol/hr using the seawater volume of each vial (total vial volume minus estimated total embryo volume or bead volume for blanks)
MO2slopes_heatwave_7dpf_final <- subtract_blanks(MO2slopes_heatwave_7dpf_meta)
#view(MO2slopes_heatwave_7dpf_final)

###End run


###Trial 4: 7 dpf ambient temp----
#set data
df <- as_tibble(MO2_data)%>%
  filter(trial_id == 4)%>%
  filter(run_num > 0)###Subset data if not all runs are wanted

###Plots
ggplot(df) +
  aes(x=delta_time_mins, y=DO_umol_L) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~tank) +
  labs(title="Embryo MO2 ambient temperature 7dpf", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()+
  xlab("Run time (mins)")+ylab("DO (umol/L)")+
  xlim(0,150)+ylim(230,330)+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        plot.title=element_text(size=28,face="bold"),
        plot.caption=element_text(size=16),
        strip.text = element_text(size = 20))

#ggsave("MO2slopes_ambienttemp_7dpf.jpeg", path = "figs", width = 10, height =12, dpi = 1200)

###Calculate slopes for oxygen change for each replicate and blank
MO2slopes_ambienttemp_7dpf_fits <- calculate_slopes(df)
###Note the slopes and fits for each measurement
#view(MO2slopes_ambienttemp_7dpf)

###Create a run summary report
MO2slopes_ambienttemp_7dpf_runsummary <- run_summary(MO2slopes_ambienttemp_7dpf_fits)
#view(MO2slopes_ambienttemp_7dpf_runsummary)

###Append metadata to slope measurements and remove fit statistics
event_id = 4
MO2slopes_ambienttemp_7dpf_meta <- append_metadata(MO2slopes_ambienttemp_7dpf_fits)
#view(MO2slopes_ambienttemp_7dpf_meta)

###Next convert MO2 rates to umol/hr, subtract out blank respiration rates, and present MO2 as umol/hr/individual.
###Convert MO2rates from umol/L/min to umol/hr using the seawater volume of each vial (total vial volume minus estimated total embryo volume or bead volume for blanks)
MO2slopes_ambienttemp_7dpf_final <- subtract_blanks(MO2slopes_ambienttemp_7dpf_meta)
###For this trial, blank DO levels actually increased during measurements, so embryo MO2 actually increased by correcting for blanks. 
#view(MO2slopes_ambienttemp_7dpf_final)

###End run
###Trial 5: 8 dpf heatwave temp----
df <- as_tibble(MO2_data)%>%
  filter(trial_id == 5)%>%
  filter(run_num > 0)###Subset data if not all runs are wanted

###Plots
ggplot(df) +
  aes(x=delta_time_mins, y=DO_umol_L) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~tank) +
  labs(title="Embryo MO2 heatwave temperature 8dpf", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()+
  xlab("Run time (mins)")+ylab("DO (umol/L)")+
  xlim(0,80)+ylim(175,280)+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        plot.title=element_text(size=28,face="bold"),
        plot.caption=element_text(size=16),
        strip.text = element_text(size = 20))

#ggsave("MO2slopes_heatwave_8dpf.jpeg", path = "figs", width = 10, height =12, dpi = 1200)

###Calculate slopes for oxygen change for each replicate and blank
MO2slopes_heatwave_8dpf_fits <- calculate_slopes(df)
###Note the slopes and fits for each measurement
#view(MO2slopes_heatwave_8dpf)

###Create a run summary report
MO2slopes_heatwave_8dpf_runsummary <- run_summary(MO2slopes_heatwave_8dpf_fits)
#view(MO2slopes_heatwave_8dpf_runsummary)

###Append metadata to slope measurements and remove fit statistics
event_id = 5
MO2slopes_heatwave_8dpf_meta <- append_metadata(MO2slopes_heatwave_8dpf_fits)
#view(MO2slopes_heatwave_8dpf_meta)

###Next convert MO2 rates to umol/hr, subtract out blank respiration rates, and present MO2 as umol/hr/individual.
###Convert MO2rates from umol/L/min to umol/hr using the seawater volume of each vial (total vial volume minus estimated total embryo volume or bead volume for blanks)
MO2slopes_heatwave_8dpf_final <- subtract_blanks(MO2slopes_heatwave_8dpf_meta)
#view(MO2slopes_heatwave_8dpf_final)

###End run



###Trial 6: 8 dpf ambient temp----
df <- as_tibble(MO2_data)%>%
  filter(trial_id == 6)%>%
  filter(run_num < 9)###Subset data if not all runs are wanted

###Plots
ggplot(df) +
  aes(x=delta_time_mins, y=DO_umol_L) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~tank) +
  labs(title="Embryo MO2 ambient temperature 8dpf", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()+
  xlab("Run time (mins)")+ylab("DO (umol/L)")+
  xlim(0,120)+ylim(230,330)+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24,face="bold"),
        plot.title=element_text(size=28,face="bold"),
        plot.caption=element_text(size=16),
        strip.text = element_text(size = 20))

#ggsave("MO2slopes_ambienttemp_8dpf.jpeg", path = "figs", width = 10, height =12, dpi = 1200)

###Calculate slopes for oxygen change for each replicate and blank
MO2slopes_ambienttemp_8dpf_fits <- calculate_slopes(df)
###Note the slopes and fits for each measurement
#view(MO2slopes_ambienttemp_8dpf)

###Create a run summary report
MO2slopes_ambienttemp_8dpf_runsummary <- run_summary(MO2slopes_ambienttemp_8dpf_fits)
#view(MO2slopes_ambienttemp_8dpf_runsummary)

###Append metadata to slope measurements and remove fit statistics
event_id = 6
MO2slopes_ambienttemp_8dpf_meta <- append_metadata(MO2slopes_ambienttemp_8dpf_fits)
#view(MO2slopes_ambienttemp_8dpf_meta)

###Next convert MO2 rates to umol/hr, subtract out blank respiration rates, and present MO2 as umol/hr/individual.
###Convert MO2rates from umol/L/min to umol/hr using the seawater volume of each vial (total vial volume minus estimated total embryo volume or bead volume for blanks)
MO2slopes_ambienttemp_8dpf_final <- subtract_blanks(MO2slopes_ambienttemp_8dpf_meta)
###For this trial, blank DO levels actually increased during measurements, so embryo MO2 actually increased by correcting for blanks. 
#view(MO2slopes_ambienttemp_8dpf_final)

###End run

####Summarize data----
all_regression_fits <- rbind(MO2slopes_heatwave_6dpf_meta,MO2slopes_ambienttemp_6dpf_meta,
                        MO2slopes_heatwave_7dpf_meta,MO2slopes_ambienttemp_7dpf_meta,
                        MO2slopes_heatwave_8dpf_meta,MO2slopes_ambienttemp_8dpf_meta)
all_run_summaries <- rbind(MO2slopes_heatwave_6dpf_runsummary,MO2slopes_ambienttemp_6dpf_runsummary,
                           MO2slopes_heatwave_7dpf_runsummary,MO2slopes_ambienttemp_7dpf_runsummary,
                           MO2slopes_heatwave_8dpf_runsummary,MO2slopes_ambienttemp_8dpf_runsummary)
all_MO2 <- rbind(MO2slopes_heatwave_6dpf_final,MO2slopes_ambienttemp_6dpf_final,MO2slopes_heatwave_7dpf_final,
                 MO2slopes_ambienttemp_7dpf_final,MO2slopes_heatwave_8dpf_final,MO2slopes_ambienttemp_8dpf_final)
all_MO2 <- all_MO2 %>%
  unite(treatment, temp_treatment, pCO2, remove = FALSE)
  

###Change sign and rename MO2rate_umol_hr_indv to simply MO2
all_MO2$MO2 <- -(all_MO2$MO2rate_umol_hr_indv)

all_MO2  <- all_MO2 %>%
      mutate(MO2_SA = (MO2/avg_embryo_SA_mm2))
#all_MO2  <- all_MO2 %>%
      #mutate(uncorrectedMO2 = -(MO2rate_umol_hr_raw/num_embryos))
    
####Analyze and plot----
###Set factors
all_MO2$pCO2 <- as.factor(all_MO2$pCO2); all_MO2$temp_treatment <- as.factor(all_MO2$temp_treatment); all_MO2$temp <- as.factor(all_MO2$temp)



###Analyze MO2 from 6 dpf----
MO2_6dpf <- all_MO2 %>%
          filter(age_dpf == 6)

model_6dpf <- lm(MO2_SA~pCO2*temp_treatment, data = MO2_6dpf); anova(model_6dpf)
check_model(model_6dpf)

###Plot
ggplot(MO2_6dpf) +
  aes(x=temp, y=MO2_SA, color = pCO2, group = pCO2) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  labs(title="Embryo MO2 heatwave temperature 6dpf", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()+
  xlab("Temperature (°C)")+ylab("MO2 (µmol hr-1individual-1mm-2)"  )



###Analyze MO2 from 7 dpf----
MO2_7dpf <- all_MO2 %>%
  filter(age_dpf == 7)

model_7dpf <- lm(MO2_SA~pCO2*temp_treatment, data = MO2_7dpf); anova(model_7dpf)
check_model(model_7dpf)

###Plot
ggplot(MO2_7dpf) +
  aes(x=temp_treatment, y=MO2_SA, group = pCO2, color = pCO2) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  labs(title="Embryo MO2 heatwave temperature 7dpf", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()+
  xlab("Temperature (°C)")+ylab("MO2 (µmol hr-1individual-1mm-2)")



###Analyze MO2 from 8 dpf----
MO2_8dpf <- all_MO2 %>%
  filter(age_dpf == 8)

model_8dpf <- lm(MO2_SA~pCO2*temp, data = MO2_8dpf); anova(model_8dpf)
check_model(model_8dpf)
model_performance(model_8dpf)
compare_performance(model_8dpf,model_8dpf_log, rank = TRUE)
###Plot
ggplot(MO2_8dpf) +
  aes(x=temp, y=MO2_SA, group = pCO2, color = pCO2) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  labs(title="Embryo MO2 heatwave temperature 8dpf", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()+
  xlab("Temperature (°C)")+ylab("MO2 (µmol hr-1individual-1mm-2)")


####Analyze all time points together----

#All data
model_alldpf <- lm(MO2_SA~pCO2*temp_treatment+age_dpf, data = all_MO2); anova(model_alldpf)
all_MO2$modelfit <- predict(model_alldpf)
ggplot(all_MO2) +
  aes(x=age_dpf, y=MO2,gorupt = treatment, color = treatment) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=TRUE)+
  labs(title="Embryo MO2 unde", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()
  #xlab("Run time (mins)")+ylab("DO (umol/L)")+
  #xlim(0,150)+ylim(200,315)
 
  
###Usw MO2_SA  
model_alldpf <- lm(MO2_SA~pCO2*temp_treatment+age_dpf, data = all_MO2); anova(model_alldpf)
model_performance(model_alldpf, rank = TRUE)
check_model(model_alldpf)
all_MO2$modelfit <- predict(model_alldpf)
ggplot(all_MO2) +
  aes(x=age_dpf, y=log(MO2_SA),group = treatment, color = treatment) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  labs(title="Embryo MO2 unde", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw() 
  
###Split by temp treatments
#Ambient temp
all_MO2_ambienttemp <- all_MO2 %>%
  filter(temp_treatment == "ambient")

###Models
model_alldpf_amb_withSA <- lm(MO2~age_dpf*pCO2*+avg_embryo_SA_mm2, data = all_MO2_ambienttemp); anova(model_alldpf_amb_withSA)
model_alldpf_amb_noSA <- lm(MO2~age_dpf*pCO2, data = all_MO2_ambienttemp); anova(model_alldpf_amb_noSA)
AIC(model_alldpf_amb_withSA,model_alldpf_amb_noSA)

all_MO2_ambienttemp$modelfit <- predict(model_alldpf_amb_noSA)
ggplot(all_MO2_ambienttemp) +
  aes(x=age_dpf, y=MO2, group = pCO2, color = pCO2) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  labs(title="Embryo MO2 ambient temp", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()

###Testing grounds
ggplot(all_MO2_ambienttemp) +
  aes(x=age_dpf, y=avg_embryo_SA_mm2, group = pCO2, color = pCO2) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)



###Heatwave
all_MO2_heatwave <- all_MO2 %>%
  filter(temp_treatment == "heatwave")


model_alldpf_heat_withSA <- lm(MO2~age_dpf*pCO2*+avg_embryo_SA_mm2, data = all_MO2_heatwave); anova(model_alldpf_heat_withSA)
model_alldpf_heat_noSA <- lm(MO2~age_dpf*pCO2, data = all_MO2_heatwave); anova(model_alldpf_heat_noSA)
AIC(model_alldpf_heat_withSA,model_alldpf_heat_noSA)

all_MO2_ambienttemp$modelfit <- predict(model_alldpf_amb_withSA)
ggplot(all_MO2_heatwave) +
  aes(x=age_dpf, y=MO2, group = pCO2, color = pCO2) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)+
  labs(title="Embryo MO2 heatwave temp", caption=paste0("Exp 3 Pacific herring heatwave x CO2 ")) +
  theme_bw()

###Testing grounds
ggplot(all_MO2_heatwave) +
  aes(x=age_dpf, y=avg_embryo_SA_mm2, group = pCO2, color = pCO2) +
  geom_point(size = 2)+
  geom_smooth(method=lm, se=FALSE)
