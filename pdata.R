#################################
# Final output script for manuscript
# 12/14/17 @ 9:35PM
rm(list=ls())

#################################
rm(list=ls())
options(scipen = 999) # remove annoying scientific notation


if (dir.exists("C:/Users/Administrator/Documents/311")){
	setwd("C:/Users/Administrator/Documents/311") } else if
	(dir.exists("G:/math/651project")) { setwd("G:/math/651project")
	} else { setwd("C:/Users/michael/Documents/311") }

library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)


if (dir.exists("C:/Users/Administrator/Documents/311")){
		dat <- fread("C:\\Users\\Administrator\\Documents\\311v4.csv") 
	} else if 	(dir.exists("G:/math/311")) {	
		dat <- fread("G:/math/311/311v4.csv")
	} else { dat <- fread("C:\\Users\\michael\\Documents\\311v4.csv")  }

missing_blocks <- dat %>%
	filter(Income == 0) %>%
	dplyr::select(block) %>%
	distinct() %>%
	pull()

missing_blocks_val <- c(64235, 35438, 121265, 91609, 57565, 153705, 
	94496, 35438, 25588, 33990, 80250, 65387, 48847, 25588, 153992, 
	24735, 29542)
missing_merge <- data.frame(block = missing_blocks,
	 temp_val = missing_blocks_val)

fixed_data <- dat %>% 
	filter(Income == 0) %>%
	left_join(missing_merge, by="block") %>%
	mutate(Income = ifelse(Income == 0, temp_val, Income))

dat <- dat %>%
	filter(Income != 0) %>%
	bind_rows(fixed_data)

# Add in zipPopulation to standardize establishments
zip <- read.csv("zipop.csv") %>%
	tbl_df()

dat <- dat %>%
	left_join(zip, by = "ZIPCODE") %>%
	mutate(zipPopulation = ifelse(zipPopulation == 0 | is.na(zipPopulation), 1, zipPopulation)) %>%
	mutate(pEstablishments = establishments/zipPopulation)

# Add in time/season covariates
weather_threshold <- 0.1
dat <- dat %>%
	mutate(
		ADDDATE_wday = lubridate::wday(ADDDATE, label = TRUE),
		ADDDATE_month = lubridate::month(ADDDATE, label = TRUE),
		ADDDATE_hour = lubridate::hour(ymd_hms(ADDDATE)),
		ADDDATE_morning = as.factor(ifelse(ADDDATE_hour > 6 & 
			ADDDATE_hour < 12, 1, 0)),
		ADDDATE_summer = as.factor(ifelse(ADDDATE_month %in%
			 c("May", "Jun", "Jul", "Aug"), 1, 0)),
		rain_bool = (rain_interval>weather_threshold),
		rain_heavy_bool = (rain_heavy_interval>weather_threshold),
		snow_bool = (snow_interval>weather_threshold))

#################################
# Model 1 (multivariate linear regression)

reg_data <- dat %>%
	filter(!is.na(wait)) %>%
	filter(SERVICECODEDESCRIPTION == "Pothole") %>%
	mutate(
		Prop.commuters = Commuters/Population,
		Prop.drivers = Drivers/Population,
		Prop.vacant = Vacant/Households) %>%
	dplyr::select(wait,
								temp_daily,
								rain_bool,
								rain_heavy_bool,
								snow_bool,
								Prop.commuters,
								Prop.drivers,
								Income,
								Rent,
								Gini,
								Prop.vacant,
								commute.time,
								hours.worked,
								Pmoved,
								pWhite,
								pHispanic,
								pPoverty,
								pRetIncome,
								pOwned,
								pEstablishments,
								ADDDATE_morning,
								ADDDATE_summer)

fit <- lm(log(wait) ~ ., data=reg_data)

datleaf <- dat %>%
	filter(!is.na(wait)) %>%
	filter(SERVICECODEDESCRIPTION == "Pothole") %>%
	mutate(
		Prop.commuters = Commuters/Population,
		Prop.drivers = Drivers/Population,
		Prop.vacant = Vacant/Households) %>%
	dplyr::select(wait,LATITUDE, LONGITUDE,block,
	
								temp_daily,
								rain_bool,
								rain_heavy_bool,
								snow_bool,
								Prop.commuters,
								Prop.drivers,
								Income,
								Rent,
								Gini,
								Prop.vacant,
								commute.time,
								hours.worked,
								Pmoved,
								pWhite,
								pHispanic,
								pPoverty,
								pRetIncome,
								pOwned,
								pEstablishments,
								ADDDATE_morning,
								ADDDATE_summer)


