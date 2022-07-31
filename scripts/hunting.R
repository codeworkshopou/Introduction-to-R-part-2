# Pop Eco Model Assignment
# Estimating N to compare populations with/without hunting

setwd("C:/Users/Kolbm/Documents/R workshop/Introduction to R P2 2022 CK/")

library(dplyr)
library(rgdal)
library(lmtest)
library(ggplot2)
library(forecast)
library(tseries) 

#####################################################################
############################################# Clean Data ############

# Harvest #'s  - H: MI,WI,MN M: OH, L: OK 
# Occurrence # - H: OH,WI,MI M: MN, L: OK

######## eBird - retreived 10/2020 
ebirds = read.csv("data/ebird_noF.csv", fileEncoding="UTF-8-BOM") # no Females
ebirds <- subset(ebirds, YEAR<2020 & YEAR>2014)
ebirds <- ebirds[ebirds$STATE == "Michigan" | ebirds$STATE == "Wisconsin" | ebirds$STATE == "Minnesota" | ebirds$STATE == "Ohio" | ebirds$STATE == "Oklahoma",] 
ebirds <- ebirds %>% 
  filter(OBSERVATION.COUNT != "X")
ebirds<- ebirds[rep(row.names(ebirds), ebirds$OBSERVATION.COUNT),]
ebird <- ebirds
colnames(ebird)[colnames(ebird) == "STATE"] <- "state"
colnames(ebird)[colnames(ebird) == "YEAR"] <- "year"
ebird <- rename(count(ebird, year, state), Freq = n)
colnames(ebird)[colnames(ebird) == "Freq"] <- "counts"

######## USGS Banding data ##########################################
# retrieved 7/6/2021 from USGS staff 
# Starting from 1929
# Sex codes: 0=U, 4=M, 5=F, 6=M on recap, 7=F on recap
# Age codes: 0=U, 1=AHY, 2=HY, 3=Juv, 4=Local, 5=SY, 6=ASY, 7=Third yr, 8=After third yr
# Bird status: 2=Transported, 3=Normal wild, 4=Hand reared/game farm, 5=Injured, 6=Experimental, 7=Rehabed and released, 8= +24 hrs Experiment
bands = read.csv("data/bands.csv", fileEncoding="UTF-8-BOM")
bands <- subset(bands, SEX_CODE == "U" | SEX_CODE == "M") 
bands <- subset(bands, BANDING_YEAR<2020 & BANDING_YEAR>2014)
bands <- bands[bands$STATE == "MI" | bands$STATE == "WI" | bands$STATE == "MN" | bands$STATE == "OH" | bands$STATE == "OK",] 
usgs <- bands
usgs$STATE[usgs$STATE == "MI"] <- "Michigan"
usgs$STATE[usgs$STATE == "WI"] <- "Wisconsin"
usgs$STATE[usgs$STATE == "MN"] <- "Minnesota"
usgs$STATE[usgs$STATE == "OH"] <- "Ohio"
usgs$STATE[usgs$STATE == "OK"] <- "Oklahoma"
colnames(usgs)[colnames(usgs) == "STATE"] <- "state"
colnames(usgs)[colnames(usgs) == "BANDING_YEAR"] <- "year"
usgs <- rename(count(usgs, year, state), Freq = n)
colnames(usgs)[colnames(usgs) == "Freq"] <- "counts"
usgs <- usgs[c("state", "year", "counts")]

######## iNaturalist, all woodcock observations #####################
# Females removed from comments since no sex column
inats = read.csv("data/inaturalist.csv")
inats <- subset(inats, year<2020 & year>2014)
inats <- inats[inats$place_state_name == "Michigan" | inats$place_state_name == "Wisconsin" | inats$place_state_name == "Minnesota" | inats$place_state_name == "Ohio" | inats$place_state_name == "Oklahoma",] 
inat <- inats
colnames(inat)[colnames(inat) == "place_state_name"] <- "state"
inat <- rename(count(inat, year, state), Freq = n)
colnames(inat)[colnames(inat) == "Freq"] <- "counts"
inat <- inat[c("state", "year", "counts")]

######## Global Biodiversity Information Facility ###################
# Citation: Citation GBIF.org (15 October 2020) GBIF Occurrence Download https://doi.org/10.15468/dl.jfru86
# https://www.gbif.org/citation-guidelines
# eBird and iNaturalist removed for duplicates, no sex column
# No Oklahoma data
gbifs = read.csv("data/gbif.csv", fileEncoding="UTF-8-BOM")
gbifs <- subset(gbifs, year<2020 & year>2014)
gbifs <- gbifs %>% 
  filter(basisOfRecord == "HUMAN_OBSERVATION")
gbifs <- gbifs[gbifs$stateProvince == "Michigan" | gbifs$stateProvince == "Wisconsin" | gbifs$stateProvince == "Minnesota" | gbifs$stateProvince == "Ohio" | gbifs$stateProvince == "Oklahoma",] 
gbif <- gbifs
colnames(gbif)[colnames(gbif) == "stateProvince"] <- "state"
gbif <- rename(count(gbif, year, state), Freq = n)
colnames(gbif)[colnames(gbif) == "Freq"] <- "counts"
gbif <- gbif[c("state", "year", "counts")]

######## Harvest Data from FWS ######################################
# 2015-2019 per state 
harvest = read.csv("data/harvest_data.csv", fileEncoding="UTF-8-BOM")
harvest <- harvest[c("State", "Year", "Woodcock.Harvest", "Harvest.per.Hunter")]
harvest <- harvest[harvest$State == "MI" | harvest$State == "WI" | harvest$State == "MN" | harvest$State == "OH" | harvest$State == "OK",] 
harvest$State[harvest$State == "MI"] <- "Michigan"
harvest$State[harvest$State == "WI"] <- "Wisconsin"
harvest$State[harvest$State == "MN"] <- "Minnesota"
harvest$State[harvest$State == "OH"] <- "Ohio"
harvest$State[harvest$State == "OK"] <- "Oklahoma"
names(harvest) <- c("state", "year", "harvest", "harvest_per_hunter")

######## Join datasets ##############################################

df <- rbind(ebird, usgs, inat, gbif)
df <- df %>% 
  group_by(year, state) %>% 
  summarise(count = sum(counts))
df <- data.frame(df)
df <- merge(df, harvest, by=c("state","year"), all=T)

#####################################################################
############################################# Analysis ##############

######## Hunting Take Standardization - % of max ######################

# Joined dataset w/ standardization
df <- read.csv("data/25_rows.csv")

######## Regression ###################################################

plot(count ~ harvest, df)
plot(count ~ harvest_st, df)
plot(count ~ harvest_per_hunter, df)
plot(count ~ year, df)
cor(df$count, df$year)
cor(df$count, df$harvest)
cor(df$count, df$harvest_per_hunter)
cor(df$count, df$harvest_st)

# highest R sq
lm_h <- lm(count ~ harvest, df)
summary(lm_h)$adj.r.squared
lm_hst <- lm(count ~ harvest_st, df)
summary(lm_hst)$adj.r.squared
ggplot(df, aes(count, harvest_st)) + 
  geom_point(aes(col = state)) +
  geom_smooth(se = FALSE,
              method = "lm",
              formula = y ~ x)

lm_hph <- lm(count ~ harvest_per_hunter, df)
summary(lm_hph)$adj.r.squared
ggplot(df, aes(harvest_per_hunter, count)) +   # Draw ggplot2 scatterplot with smooth curve
  geom_point() +
  geom_smooth(se = FALSE,
              method = "lm",
              formula = y ~ x)

plot(count ~ harvest_per_hunter, df)
lines(df$count, predict(lm_hph, data.frame(x=df$count)), col='red')

lm_y <- lm(count ~ year, df)
summary(lm_y)$adj.r.squared
lm_hy <- lm(count ~ harvest + year, df)
summary(lm_hy)$adj.r.squared
lm_hhph <- lm(count ~ harvest_st + harvest_per_hunter, df)
summary(lm_hhph)$adj.r.squared
lm_yh <- lm(harvest ~ count + year, df)
summary(lm_yh)$adj.r.squared

df_mi <- df %>% 
  filter(state == "Michigan")
df_wi <- df %>% 
  filter(state == "Wisconsin")
df_mn <- df %>% 
  filter(state == "Minnesota")
df_oh <- df %>% 
  filter(state == "Ohio")
df_ok <- df %>% 
  filter(state == "Oklahoma")

ggplot(df, aes(year, count, color=state)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
ggplot(df, aes(year, harvest_st, color=state)) +
  geom_point() +
  geom_smooth(method="loess", col="black")
ggplot(df, aes(count, harvest_per_hunter, color=state)) +
  geom_point() +
  geom_smooth(method="loess", col="black")

### MI 
# count
ggplot(df_mi, aes(year, count)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest_st
ggplot(df_mi, aes(count, harvest_st)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest per hunter ****************************
ggplot(df_mi, aes(count, harvest_per_hunter)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

### WI 
# count ************************************
ggplot(df_wi, aes(year, count)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest_st
ggplot(df_wi, aes(count, harvest_st)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest per hunter ****************************
ggplot(df_wi, aes(count, harvest_per_hunter)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

### MN 
# count ***************************
ggplot(df_mn, aes(year, count)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest_st
ggplot(df_mn, aes(count, harvest_st)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest per hunter ****************************
ggplot(df_mn, aes(count, harvest_per_hunter)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

### OH 
# count
ggplot(df_oh, aes(year, count)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest_st **************************
ggplot(df_oh, aes(count, harvest_st)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest per hunter 
ggplot(df_oh, aes(count, harvest_per_hunter)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

### OK
# count
ggplot(df_ok, aes(year, count)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest_st **************************
ggplot(df_ok, aes(count, harvest_st)) +
  geom_point() +
  geom_smooth(method="lm", col="black")
# count harvest per hunter 
ggplot(df_ok, aes(count, harvest_per_hunter)) +
  geom_point() +
  geom_smooth(method="lm", col="black")

# All 5 by state
ggplot(df, aes(harvest_per_hunter, count, color=state)) +
  geom_point() +
  geom_smooth(method="lm", col="black") +
  facet_wrap(vars(state)) +
  theme(legend.position = "none")
ggplot(df, aes(harvest_st, count, color=state)) +
  geom_point() +
  geom_smooth(method="lm", col="black") +
  facet_wrap(vars(state)) +
  theme(legend.position = "none")
ggplot(df, aes(harvest, count, color=state)) +
  geom_point() +
  geom_smooth(method="lm", col="black") +
  facet_wrap(vars(state), scales='free') +
  theme(legend.position = "none")

# All 5 by year
ggplot(df, aes(harvest_per_hunter, count, color=state)) +
  geom_point() +
  geom_smooth(method="lm", col="black") +
  facet_wrap(vars(year)) 
ggplot(df, aes(harvest, count, color=state)) +
  geom_point() +
  geom_smooth(method="lm", col="black") +
  facet_wrap(vars(year)) 
ggplot(df, aes(harvest_st, count, color=state)) +
  geom_point() +
  geom_smooth(method="lm", col="black") +
  facet_wrap(vars(year)) 

######## Forecasting N ################################################

# Not enough data to accurately predict

# MI
df_mi <- df %>% 
  filter(state == "Michigan")
df$count_mi = ts(df_mi[, c('count')])
ggtsdisplay(df$count_mi,,ylab ="Number of Woodcock in Michigan",xlab="Year")
df_fit<-auto.arima(df$count_mi[1:5], trace = TRUE)# compares all models to each other 
checkresiduals(df_fit) # ARIMA (0,1,0) best model is drift, residuals are centered around 0, auto correlation are sig? 
fcast <- forecast(df_fit, h=5)
plot(fcast)
lines(df$count_mi)
accuracy(fcast,df$count_ts)

# WI
df_wi <- df %>% 
  filter(state == "Wisconsin")
df$count_wi = ts(df_wi[, c('count')])
ggtsdisplay(df$count_wi,,ylab ="Number of Woodcock in Wisconsin",xlab="Year")
df_fit<-auto.arima(df$count_wi[1:5], trace = TRUE)# compares all models to each other 
checkresiduals(df_fit) # ARIMA (0,1,0) best model is drift, residuals are centered around 0, auto correlation are sig? 
fcast <- forecast(df_fit, h=5)
plot(fcast)
lines(df$count_wi)
accuracy(fcast,df$count_wi)

# MN
df_mn <- df %>% 
  filter(state == "Minnesota")
df$count_mn = ts(df_mn[, c('count')])
ggtsdisplay(df$count_mn,,ylab ="Number of Woodcock in Minnesota",xlab="Year")
df_fit<-auto.arima(df$count_mn[1:5], trace = TRUE)# compares all models to each other 
checkresiduals(df_fit) # ARIMA (0,1,0) best model is drift, residuals are centered around 0, auto correlation are sig? 
fcast <- forecast(df_fit, h=5)
plot(fcast)
lines(df$count_mn)
accuracy(fcast,df$count_mn)

# OH
df_oh <- df %>% 
  filter(state == "Ohio")
df$count_oh = ts(df_oh[, c('count')])
ggtsdisplay(df$count_oh,,ylab ="Number of Woodcock in Ohio",xlab="Year")
df_fit<-auto.arima(df$count_oh[1:5], trace = TRUE)# compares all models to each other 
checkresiduals(df_fit) # ARIMA (0,1,0) best model is drift, residuals are centered around 0, auto correlation are sig? 
fcast <- forecast(df_fit, h=5)
plot(fcast)
lines(df$count_oh)
accuracy(fcast,df$count_oh)

# OK
df_ok <- df %>% 
  filter(state == "Oklahoma")
df$count_ok = ts(df_ok[, c('count')])
ggtsdisplay(df$count_ok,,ylab ="Number of Woodcock in Oklahoma",xlab="Year")
df_fit<-auto.arima(df$count_ok[1:5], trace = TRUE)# compares all models to each other 
checkresiduals(df_fit) # ARIMA (0,1,0) best model is drift, residuals are centered around 0, auto correlation are sig? 
fcast <- forecast(df_fit, h=5)
plot(fcast)
lines(df$count_ok)
accuracy(fcast,df$count_ok)
