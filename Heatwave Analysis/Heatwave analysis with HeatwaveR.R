###install and load packages
#install.packages("heatwaveR")
library(heatwaveR)
library(dplyr)
library(tidyverse)
library(readxl)
library(openxlsx)


###load dataset
setwd("C:/Users/Christopher Murray/Documents/GitHub/Fish-Ecophysiology/Heatwave Analysis")
data = read_xlsx("PadillaBay_EnviromentalConditions_2002-2020.xlsx")

#data = Mumford_Cove_2015_2020
data$t<- convertToDate(data$t)

###Detect events
ts = ts2clm(data, climatologyPeriod = c("2015-4-14","2020-2-4"), pctile = 90)
mhw = detect_event(ts)
# View just a few metrics
mhw$event %>%
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative, rate_onset) %>%
  dplyr::arrange(-intensity_max) %>%
  head(5)

mhw = lolli_plot(mhw, metric = "intensity_max")
mhw



###Detect acidification events
ts_10th = ts2clm(data, climatologyPeriod = c("2015-4-14","2020-2-4"), pctile = 10)
mcs <- detect_event(ts_10th, coldSpells = TRUE)
# View just a few metrics
mcs$event %>%
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative, rate_onset) %>%
  dplyr::arrange(intensity_max) %>%
  head(5)


acid_event= lolli_plot(mcs, metric = "intensity_max")+ labs(x = "Peak Date", y = "Minimum pH")
acid_event

event_line(mhw, spread = 180, metric = "intensity_max", start_date = "2002-2-15", end_date = "2020-3-1")

###Detect low DO events
ts_10th = ts2clm(data, climatologyPeriod = c("2015-4-14","2020-2-4"), pctile = 10)
mlowDO <- detect_event(ts_10th, coldSpells = TRUE)
# View just a few metrics
mlowDO$event %>%
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative, rate_onset) %>%
  dplyr::arrange(intensity_max) %>%
  head(5)


lowDO_event = lolli_plot(mlowDO, metric = "intensity_max")+ labs(x = "Peak Date", y = "Minimum DO (% saturation)")
lowDO_event

event_line(mhw, spread = 180, metric = "intensity_max", start_date = "2002-2-15", end_date = "2020-3-1")
           


###combine plots

library(cowplot)
cowplot::plot_grid(mhw, acid_event, lowDO_event,  ncol = 1, nrow = 3,
                   rel_heights = c( 4, 4), label_size = 0, hjust = 4, scale= 1, align = "v")

ggsave("Padilla Bay heatwaves, acidifcation, and low DO events.png", dpi = 1200, width = 5, height = 5, path = "figs")
