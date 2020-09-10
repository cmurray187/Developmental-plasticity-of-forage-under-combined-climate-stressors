###Code to produce summary data of MO2 embryo sizes by age and tank 

library(tidyverse)
library(lmerTest)
library(lme4)
library(readxl)

df = read_excel("Exp3_MO2_embryosize.xlsx")

#df$CO2 = as.factor(df$CO2); df$temp = as.factor(df$temp); df$tank = as.factor(df$tank);
#m1 =lmer(volumeml3~CO2*temp+age_dpf+(1|tank), data= df);anova(m1)

vol <- df %>%
    group_by(age_tank) %>%
    summarise(mean = mean(volumeml3), sd= sd(volumeml3), n = n())%>%
    as_tibble()
vol <- rename(vol, volmm3 = mean)
vol <- rename(vol, volsd = sd)

surfarea <- df %>%
  group_by(age_tank) %>%
  summarise(mean = mean(surfaceareamm2), sd= sd(surfaceareamm2), n = n())
surfarea <- rename(surfarea, SAmm2 = mean)
surfarea <- rename(surfarea, SAsd = sd)


one <- as_tibble(vol[ ,1:3])
two <- as_tibble(surfarea[ ,2:4])
embryosizesummary <- bind_cols(one,two)

meta <- as_tibble(df[ ,2:6])
joined_table <- meta %>%
  left_join(embryosizesummary, by = "age_tank")
joined_table

embryosize <- joined_table %>%
  arrange(age_dpf, tank)%>%
  filter(row_number() %% 10 == 0)
