###Murray, C.S., 2020. Early-life sensitivity of Pacific herring to combined high pCO2 and heatwave conditions 
###Analysis of morphometric traits


###Load packages----
library(factoextra)
library(tidyverse)
library(ggbiplot)
library(emmeans)
library(car)
library(graphics)
library(readxl)
library(cowplot)
library(knitr)
library(kableExtra)
library(formattable)
library(olsrr)
library(nlme)
source("shift_legend.R")
###Load data----
df <- read_excel("Exp3_morphometric_data.xlsx")


###Organize and explore data----
head(df)
#Create 'group' variable to sort four treatment categories
df <- df %>%
  unite(group,temp,CO2, remove = FALSE)
#Dry weight measurements were made in duplicate to increase accuracy. 
#Average together to produce final DW column
dryweights <- cbind(df$DW1,df$DW2)
df$DW <- rowMeans(dryweights[,c(1:2)])
df <- df %>%
  select(-DW1,-DW2)
#Create a table of mean trait values
trait_means <- df %>%
  group_by(group) %>%
  summarise_at(vars(SL,SBA,HW,REW,LEW,EW,PYBD,PVBD,YSA,DW),
               funs(mean,sd)) 
#Test if left and right eye widths vary significantly
t.test(df$REW,df$LEW, paired = TRUE)
ggplot(df, aes(x=group))+
  geom_boxplot(aes(y= REW, color = "blue"))+
  geom_boxplot(aes(y= LEW, color = "red")) 
#They do not, so mean EW will be used henceforth
eyewidths <- cbind(df$LEW,df$REW)
df$EW <- rowMeans(eyewidths[,c(1:2)])
df <- df %>%
  select(-REW,-LEW)

####Plot all traits (small multiples)----
####Small Multiple All Traits
df1 <- df 
#add new trait: YSA/SBA ratio
df1 <- df1 %>%
  mutate(YSAbySBA = YSA/SBA)

df1.pivot <- pivot_longer(df1, # dataframe to be pivoted
                                    cols = 10:18, # column names to be stored as a SINGLE variable
                                    names_to = "trait", # name of that new variable (column)
                                    values_to = "values") # name of new variable (column) storing all the values (data)
df1.pivot$trait <- factor(df1.pivot$trait, levels = c("SL","SBA","HW","EW","PYBD", "PVBD","YSA","DW","YSAbySBA"), 
                          labels = c("Standard length (mm)", "Somatic body area (mm^2)", "Head width (mm)", "Eye width (mm)", 
                                     "Post-yolk body depth (mm)", "Post-vent body depth (mm)", "Yolk sac area (mm^2)", 
                                     "Dry weight (mg)", "Yolk sac area/somatic body area ratio" ))

p <- ggplot(df1.pivot) +
  aes(x=group, y=values, color = group) + # you could interactively 'paint' different covariates onto this plot using the 'fill' aes
  geom_boxplot() +
  facet_wrap(~trait, scales = "free_x") +
  labs(title="Morphometric traits",
       caption=paste0("Murray, C.S. 2020. CO2 x heatwave effects in Pacific herring")) +
  theme_bw() +
  coord_flip()

alltraits <- grid.draw(shift_legend(p))
###PCA Analysis----

#First, let's check out a trait correlation matrix. This will inform how we evaluate the PCA.
traitcors = cor(df[,c(10:17)])
#Make a nice table
traitcors %>%
  kable(digits = 2) %>%
  kable_styling("striped", full_width = F)%>%
  column_spec(1:1, bold = T) 
#Strong correlations between SL, SBA, and PVBD, PYBD, and EW. DW and YSA correlate together, but not with other traits.

#PCA usng prcomp. Variables are centered and standardized prior to analysis. 
morpho.pca <- prcomp(df[,c(10:17)], center = TRUE,scale. = TRUE)
summary(morpho.pca)

#Adds PC scores to original df
scores = morpho.pca$x
morphoPCAdata <- cbind(df, scores)


#View scree plot 
fviz_eig(morpho.pca) +
  ggtitle("Scree Plot: Exp 3 PCA on Morphometric Data") + 
  theme(plot.title = element_text(hjust = 0.5))
#ggsave("Exp 3_PCA morhos_scree plot.png", width = 7, height = 5, path = "figs")
#First two PCs are account for 59% and 15% of variance (74% total). We may also want to examine PC3 and PC4 which cover an additional 15%.


###View trait contribution per PC. Red dashed lines indicate the percentage if all traits equally contributed to the PC.
###Traits listed above the red reference line are considered important contributors for that PC. 
PC1_contribs <- fviz_contrib(morpho.pca, choice = "var", axes = 1, top = 10)+ggtitle("Exp 3 Morphos: Trait Contribution to PC1")+ theme(plot.title = element_text(hjust = 0.5))
PC2_contribs <- fviz_contrib(morpho.pca, choice = "var", axes = 2, top = 10)+ggtitle("Exp 3 Morphos: Trait Contribution to PC2")+ theme(plot.title = element_text(hjust = 0.5))
PC3_contribs <- fviz_contrib(morpho.pca, choice = "var", axes = 3, top = 10)+ggtitle("Exp 3 Morphos: Trait Contribution to PC3")+ theme(plot.title = element_text(hjust = 0.5))
PC4_contribs <- fviz_contrib(morpho.pca, choice = "var", axes = 4, top = 10)+ggtitle("Exp 3 Morphos: Trait Contribution to PC4")+ theme(plot.title = element_text(hjust = 0.5))
PCcontribs <- cowplot::plot_grid(PC1_contribs,PC2_contribs,PC3_contribs,PC4_contribs); PCcontribs
#ggsave("Exp 3 Morphos Trait contribution to PC1-4.png", width = 9, height = 5, path = "figs")

###Most body size traits contribute to PC1 (except eye width, yolk sac area, and dry weight)
###DW and YSA overwhelmingly contribute to PC2 (make sense as these two traits correlate strongly)
###This suggests that the remaining yolk reserves drive DW (larvae get lighter as they consume yolk)
###PC3 is comprised of post-yolk body depth, HW and EW. We'll check later if PC3 affected by treatment conditions or constitutes random variation. 
###PC4 is driven largely by variation in EW, with some PVBD an SL. 


###Furthermore we can View how traits load and correlate with PCs

loadings <- as_tibble(morpho.pca$rotation)
Trait <- colnames(df[,10:17])
loadings <- cbind(Trait,loadings)
loadings <- loadings %>%
  kable(digits = 2) %>%
  kable_styling("striped", full_width = F)%>%
  column_spec(1:1, bold = T) 
loadings

var <- get_pca_var(morpho.pca)
PCtraitcors <- as_tibble(var$cor) 
PCtraitcors <- cbind(Trait,PCtraitcors)
PCtraitcors <- dplyr::rename(PCtraitcors, PC1 = Dim.1,PC2 = Dim.2,PC3 = Dim.3,PC4 = Dim.4,PC5 = Dim.5,PC6 = Dim.6,PC7 = Dim.7,PC8 = Dim.8,)
PCtraitcors <- PCtraitcors %>%
  kable(digits = 2) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(1:1, bold = T)
PCtraitcors


#A biplot can help show the relationship among variables (angles between PCs).
#And the location of individual larvae in relation to the traits and PC dimensions. 
#Ellipses show clustering within treatment groups. 
PCA_1_2_biplot <- ggbiplot(morpho.pca,ellipse=TRUE, groups=df$group)+
  ggtitle("Biplot: PC1 & PC2 from Exp 3 morphos")+ 
  theme(plot.title = element_text(hjust = 0.5))
PCA_1_2_biplot
#ggsave("Morphos PC1 and PC2.png", width = 9, height = 5, path = "figs")
#Right away we see that heatwave treatments cluster with negative PC1 scores, indicating a reduction in hatchling body size.
#Also, we see that body size traits angle to negative PC2 scores. Larger body size leads to smaller yolk reserves (and less DW).
#In terms of PC2, there seems to be a small divergence between CO2 treatments, especially at ambient temps. High CO2 larvae have more positive PC2 scores.

#Let's check PC3 and PC4 against PC1 for any obvious patterns.
PCA_1_3_biplot <- ggbiplot(morpho.pca, choices = c(1,3), ellipse=TRUE, groups=df$group)+
  ggtitle("Biplot: PC1 & PC3 from Exp 3 morphos")+ 
  theme(plot.title = element_text(hjust = 0.5))
#Heatwave larvae tend to have negative PC3 scores. HW and PYBD are negatively correlated with PC3, while other traits are neutral or positive, especially EW. 

PCA_1_4_biplot <- ggbiplot(morpho.pca, choices = c(1,4), ellipse=TRUE, groups=df$group)+
  ggtitle("Biplot: PC1 & PC4 from Exp 3 morphos")+ 
  theme(plot.title = element_text(hjust = 0.5))
#No real pattern between PC4 and treatment levels, EW is positively related while DW, SBA, and PVBD stand out as negative. 
#Let's see what the models say.



####Outlier tests----
###set treatment variables as factors
morphoPCAdata$CO2 <- as.factor(morphoPCAdata$CO2); morphoPCAdata$temp <- as.factor(morphoPCAdata$temp); morphoPCAdata$tank <- as.factor(morphoPCAdata$tank)
#Use cook's distance to infer influential data points. 
#Traditional threshold set at 0.021, however for this data set I'd like to relax the threshold to 0.04 to include more samples at the extremes.   
#PC1
modPC1 <- lm(PC1~CO2*temp, data=morphoPCAdata_PC1out)
ols_plot_cooksd_chart(modPC1)
#sample_id 44 is highly influential, although I can confirm that this was a very large larva and not a mistake or measurement artifact. 
#Let's see how the mixed model reacts to its inclusion. 
morphoPCAdata_PC1out <- morphoPCAdata %>% 
  filter(sample_id != 44 & 78)
#PC2
modPC2 <- lm(PC2~CO2*temp, data=morphoPCAdata)
ols_plot_cooksd_chart(modPC2)
#sample_id 101 is a clear influential point due to it's very large YSA. I double checked the measurement and it's correct. Let's see how the LMM reacts to it's inclusion.
morphoPCAdata_PC2out <- morphoPCAdata %>% 
  filter(sample_id != 101)
#PC3
modPC3 <- lm(PC3~CO2*temp, data=morphoPCAdata)
ols_plot_cooksd_chart(modPC3)
#No major influential point that I think needs to be dealt with. 
#PC4
modPC4 <- lm(PC4~CO2*temp, data=morphoPCAdata)
ols_plot_cooksd_chart(modPC4)
#Sample_id 166 is appears to be an influential point, cook's d ~0.045. I think it's okay to leave in an PC4 isn't so important. 
morphoPCAdata_PC4out <- morphoPCAdata %>% 
  filter(sample_id != 166)



####Mixed-effects model using nlme()----
#We'll test for significant effects of CO2 level and thermal regime on morphometric measurements using linear mixed-effects models.
#The models implement tank as a random effect to account for the common rearing environment of larvae from the same tank. 

###PC1----
lmePC1 <- lme(fixed = PC1~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmePC1)
emPC1= emmeans(lmePC1,pairwise~CO2*temp); emPC1
#Test for normal residuals 
qqnorm(resid(lmePC1)); qqline(resid(lmePC1)); shapiro.test(resid(lmePC1))
#Fails normality, but largely driven by influential point. 
#Variance homogeneity test
plot(lmePC1)
leveneTest(resid(lmePC1)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Fails leveneTest

#Since the model fails both assumptions tests, let's try the model without the influential point
lmePC1_out <- lme(fixed = PC1~CO2*temp, data = morphoPCAdata_PC1out, random = ~1|tank)
summary(LmmPC1_out)
emPC1= emmeans(lmePC1_out,pairwise~CO2*temp); emPC1
###Test for normal residuals 
qqnorm(resid(lmePC1_out)); qqline(resid(lmePC1_out)); shapiro.test(resid(lmePC1_out))
#Passes normality
###Variance homogeneity test
plot(lmePC1_out)
leveneTest(resid(lmePC1_out)~morphoPCAdata_PC1out$CO2*morphoPCAdata_PC1out$temp*morphoPCAdata_PC1out$tank)
#Fails levene's test and residuals vs. fitted values plot looks bad, with conical shape

#Because we can't fixed the heteroskedasticity by removing the influential point, let's switch to nlme function.
#Using the weights object we can allow for unequal variances between rearing groups (i.e., tanks).
###Final model for PC1
VItank <- varIdent(form = ~1|tank)
lmePC1_vi <- lme(fixed = PC1~CO2*temp, data = morphoPCAdata_PC1out, random = ~1|tank, weights = VItank)
anova(lmePC1_vi)
summary(mePC1_vi)
emPC1= emmeans(lmePC1_vi,pairwise~CO2*temp); emPC1
##
qqnorm(resid(lmePC1_vi)); qqline(resid(lmePC1_vi)); shapiro.test(resid(lmePC1_vi))
#Passes normality
plot(lmePC1_vi)
leveneTest(resid(summary(lmePC1_vi_ranslope))~morphoPCAdata_PC1out$CO2*morphoPCAdata_PC1out$temp*morphoPCAdata_PC1out$tank)
#Fails Levene's test, but standardized residuals vs. fitted values looks much better than before, 
#and good enough to proceed with this model. I don't think further attempts to remove influential points or standardize the fit will improve the model. 

#Make scatted plot the includes fitted PC1 values per replicate tank and all individual larval PC1 scores.
PC1fits <- fitted(lmePC1_vi)
ggplot(morphoPCAdata_PC1out)+
  geom_point(aes(x = group, y = PC1, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  geom_point(aes(x = group , y = PC1fits, color = group),color = "black", size = 8 )+
  geom_point(aes(x = group , y = PC1fits, color = group), size = 6) +
  theme_bw()


###PC2 analysis----
lmePC2 <- lme(fixed = PC2~CO2*temp, data = morphoPCAdata, random = ~1|tank)
lmePC2_ranslope <- lme(fixed = PC2~CO2*temp, data = morphoPCAdata, random = list(tank = pdDiag(~CO2*temp-1)))
anova(lmePC2,lmePC2_ranslope)
summary(lmePC2)
summary(lmePC2_ranslope)
#High CO2 larvae had significantly higher PC2 scores (larger yolks and heavier at hatch)
qqnorm(resid(lmePC2)); qqline(resid(lmePC2)); shapiro.test(resid(lmePC2))
#Passes normality
plot(lmePC2)
leveneTest(resid(lmePC2)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Passes levene
#Let's make sure that the influential point isn't driving the pCO2 effect.
lmePC2_out <- lme(fixed = PC2~CO2*temp, data = morphoPCAdata_PC2out, random = ~1|tank)
summary(lmePC2_out); anova(lmePC2_out)
#The effect is roughly the same, let's retain the influential point.
#Plot fitted and measured values
PC2fits <- fitted(lmePC2)
ggplot(morphoPCAdata,aes(x = group , y = PC2fits, color = group)) +
  geom_point(size = 6)+
  geom_point(aes(x = group, y = PC2, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  theme_bw()



###PC3 analysis---
lmePC3 <- lme(fixed = PC3~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmePC3)
#Sig. heatwave effect, heatwave larvae significantly smaller PC3 scores. 
#HW and PYBD load negatively onto PC3 (-52 & -60), while SL and EW are positively loaded (.37 & 44)
#Given this divergence, we'll have to check each trait individually to further tease out the effect
qqnorm(resid(lmePC3)); qqline(resid(lmePC3)); shapiro.test(resid(lmePC3))
#Passes normality
plot(lmePC3)
leveneTest(resid(lmePC3)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Passes assumption of heterogeneous variance


###PC4 analysis---
lmePC4 <- lme(fixed = PC4~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmePC4)
#No treatment effects
qqnorm(resid(lmePC4)); qqline(resid(lmePC4)); shapiro.test(resid(lmePC4))
#Passes normality
plot(lmePC4)
leveneTest(resid(lmePC4)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Passes assumption of heterogeneous variance
#I think we can ignore PC4 from here on out. 



###Individual trait analysis----
##SL analysis----
lmeSL <- lme(fixed = SL~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmeSL)
#Sig. effect of temperature. Heatwave larvae are significantly shorter. 
qqnorm(resid(lmeSL)); qqline(resid(lmeSL)); shapiro.test(resid(lmeSL))
#Passes normality
plot(lmeSL)
leveneTest(resid(lmeSL)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Fails assumption of heterogeneous variance
#Try model that includes weights for unequal group variances
#Final model for SL
lmeSL_VI <- lme(fixed = SL~CO2*temp, data = morphoPCAdata, random = ~1|tank,weights = VItank)
summary(lmeSL_VI)
#Sig. effect of temperature. Heatwave larvae are significantly shorter. 
qqnorm(resid(lmeSL_VI)); qqline(resid(lmeSL_VI)); shapiro.test(resid(lmeSL_VI))
#Passes normality
plot(lmeSL_VI)
leveneTest(resid(lmeSL_VI)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Fails Levene's test, but standardized residuals vs. fitted values looks much better.
#Plot fitted and measured values
morphoPCAdata$SLfits <- fitted(lmeSL_VI)
ggplot(morphoPCAdata,aes(x = group , y = SLfits, color = group)) +
  geom_point(size = 6)+
  geom_point(aes(x = group, y = SL, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  theme_bw()

##SBA analysis----
lmeSBA <- lme(fixed = SBA~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmeSBA)
#Sig. effect of temperature. Heatwave larvae are significantly shorter. 
qqnorm(resid(lmeSBA)); qqline(resid(lmeSBA)); shapiro.test(resid(lmeSBA))
#Passes normality
plot(lmeSBA)
leveneTest(resid(lmeSBA)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Fails assumption of heterogeneous variance
#Try model that includes weights for unequal group variances
#Final model for SL
lmeSBA_VI <- lme(fixed = SBA~CO2*temp, data = morphoPCAdata, random = ~1|tank,weights = VItank)
summary(lmeSBA_VI)
#Sig. effect of temperature. Heatwave larvae are significantly smaller. 
qqnorm(resid(lmeSBA_VI)); qqline(resid(lmeSBA_VI)); shapiro.test(resid(lmeSBA_VI))
#Fails normality
plot(lmeSBA_VI)
leveneTest(resid(lmeSBA_VI)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Fails Levene's test, but standardized residuals vs. fitted values looks much better.
#Plot fitted and measured values
morphoPCAdata$SBAfits <- fitted(lmeSBA_VI)
ggplot(morphoPCAdata,aes(x = group , y = SBAfits, color = group)) +
  geom_point(size = 6)+
  geom_point(aes(x = group, y = SBA, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  theme_bw()


##HW analysis----
lmeHW <- lme(fixed = HW~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmeHW)
#No treatment effects on HW. 
qqnorm(resid(lmeHW)); qqline(resid(lmeHW)); shapiro.test(resid(lmeHW))
#Passes normality
plot(lmeHW)
leveneTest(resid(lmeHW)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Fails levene's test, try weights(varIdent(~1|tank))
#Final HW model
lmeHW_VI <- lme(fixed = HW~CO2*temp, data = morphoPCAdata, random = ~1|tank, weights = VItank)
summary(lmeHW_VI)
#No treatment effects on HW.  
qqnorm(resid(lmeHW_VI)); qqline(resid(lmeHW_VI)); shapiro.test(resid(lmeHW_VI))
#Passes normality
plot(lmeHW_VI)
leveneTest(resid(lmeHW_VI)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Use weighted model
morphoPCAdata$HWfits <- fitted(lmeHW_VI)
ggplot(morphoPCAdata,aes(x = group , y = HWfits, color = group)) +
  geom_point(size = 6)+
  geom_point(aes(x = group, y = HW, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  theme_bw()



##EW analysis----
lmeEW <- lme(fixed = EW~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmeEW)
#Sig. effect of temperature, heatwave larvae have smaller eyes. CO2 effect is close to being significant.  
qqnorm(resid(lmeEW)); qqline(resid(lmeEW)); shapiro.test(resid(lmeEW))
#Passes normality
plot(lmeEW)
leveneTest(resid(lmeEW)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Passes levene
morphoPCAdata$EWfits <- fitted(lmeEW)
#Plot
ggplot(morphoPCAdata,aes(x = group , y = EWfits, color = group)) +
  geom_point(size = 6)+
  geom_point(aes(x = group, y = EW, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  theme_bw()



##PYBD analysis----
lmePYBD <- lme(fixed = PYBD~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmePYBD)
#No sig. treatment effects on PYBD.   
qqnorm(resid(lmePYBD)); qqline(resid(lmePYBD)); shapiro.test(resid(lmePYBD))
#Passes normality
plot(lmePYBD)
leveneTest(resid(lmePYBD)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Passes levene's
#Plot
morphoPCAdata$PYBDfits <- fitted(lmePYBD)
ggplot(morphoPCAdata,aes(x = group , y = PYBDfits, color = group)) +
  geom_point(size = 6)+
  geom_point(aes(x = group, y = PYBD, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  theme_bw()


##PVBD analysis----
lmePVBD <- lme(fixed = PVBD~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmePVBD)
#Sig. temp effect, heatwave larvae have shorter PVBD.     
qqnorm(resid(lmePVBD)); qqline(resid(lmePVBD)); shapiro.test(resid(lmePVBD))
#Passes normality
plot(lmePVBD)
leveneTest(resid(lmePVBD)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Passes levene's
#Plot
morphoPCAdata$PVBDfits <- fitted(lmePVBD)
ggplot(morphoPCAdata,aes(x = group , y = PVBDfits, color = group)) +
  geom_point(size = 6)+
  geom_point(aes(x = group, y = PVBD, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  theme_bw()



##YSA analysis---- still needs work
lmeYSA <- lme(fixed = YSA~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmeYSA)
#Sig. effect of temp and CO2. Heatwave larvae hatch with smaller yolks, and high CO2 slightly increased yolk size.     
qqnorm(resid(lmeYSA)); qqline(resid(lmeYSA)); shapiro.test(resid(lmeYSA))
#Fail's normality
plot(lmeYSA)
leveneTest(resid(lmeYSA)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Passes levene's
#Let's test the fit of log(YSA)
lmeYSA_log <- lme(fixed = log(YSA)~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmeYSA_log)
#Sig. effect of temp and CO2.     
qqnorm(resid(lmeYSA_log)); qqline(resid(lmeYSA_log)); shapiro.test(resid(lmeYSA_log))
#Normality looks improved but not perfect.
plot(lmeYSA_log)
leveneTest(resid(lmeYSA_log)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#But the log transformation really increased variance heteroskedasticity. Let's see if this can be fixed with weights. 
lmeYSA_log_VI <- lme(fixed = YSA~CO2*temp, data = morphoPCAdata, random = ~1|tank, weights = VItank)
summary(lmeYSA_log_VI)
qqnorm(resid(lmeYSA_log_VI)); qqline(resid(lmeYSA_log_VI)); shapiro.test(resid(lmeYSA_log_VI))
#Normality looks improved but not perfect.
plot(lmeYSA_log_VI)
leveneTest(resid(lmeYSA_log_VI)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)




###DW analysis----
lmeDW <- lme(fixed = DW~CO2*temp, data = morphoPCAdata, random = ~1|tank)
summary(lmeDW)
#No treatment effects on DW    
qqnorm(resid(lmeDW)); qqline(resid(lmeDW)); shapiro.test(resid(lmeDW))
#Passes normality
plot(lmeDW)
leveneTest(resid(lmeDW)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)
#Passes levene's

#Plot
morphoPCAdata$DWfits <- fitted(lmeDW)
ggplot(morphoPCAdata) +
  geom_point(aes(x = group , y = DWfits, color = group), shape = 24, fill = "grey", size =4)+
  geom_point(aes(x = group, y = DW, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  theme_bw()

####Small Multiple All Traits
morphoPCAdata.pivot <- morphoPCAdata[,c(1:21)]
morphoPCAdata.pivot <- pivot_longer(morphoPCAdata.pivot, # dataframe to be pivoted
                                    cols = 10:21, # column names to be stored as a SINGLE variable
                                    names_to = "trait", # name of that new variable (column)
                                    values_to = "values") # name of new variable (column) storing all the values (data)

ggplot(morphoPCAdata.pivot) +
  aes(x=group, y=values, color = group) + # you could interactively 'paint' different covariates onto this plot using the 'fill' aes
  geom_boxplot() +
  facet_wrap(~trait, scales = "free_x") +
  labs(title="PCA 'small multiples' plot",
       caption=paste0("produced on ", Sys.time())) +
  theme_bw() +
  coord_flip()













###YSA/SBA analysis----
lmeYSAbySBA <- lme(fixed = YSAbySBA~CO2*temp, data = df1, random = ~1|tank, weight = VItank )
summary(lmeYSAbySBA)
emPC1= emmeans(lmeYSAbySBA,pairwise~CO2*temp); emPC1
#No treatment effects on DW    
qqnorm(resid(lmeYSAbySBA)); qqline(resid(lmeYSAbySBA)); shapiro.test(resid(lmeYSAbySBA))
#Passes normality
plot(lmeYSAbySBA)
leveneTest(resid(lmeYSAbySBA)~morphoPCAdata$CO2*morphoPCAdata$temp*morphoPCAdata$tank)

df1$YSAbySBAfits <- fitted(lmeYSAbySBA)
ggplot(df1,aes(x = group , y = YSAbySBAfits, color = group)) +
  geom_point(size = 6)+
  geom_point(aes(x = group, y = YSAbySBA, color = group), position = position_jitterdodge(dodge.width = 0.07), size = 2)+
  theme_bw()
