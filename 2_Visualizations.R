#### GROUP & VISUALIZE GRANULARITY ####
## Seasons
## ifelse to set the season attribute
season.plotly.lines <- imputed.table %>%
  mutate(Season = ifelse(month == "Jan"| month == "Feb" | month == "Mar","Winter", ifelse(month == "Apr" | month == "May" | month == "Jun", "Spring", ifelse(month == "Jul" | month == "Aug" | month == "Sep", "Summer", "Autumn")))) %>%
  group_by(Season) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>% 
  plot_ly(x = ~Season, y = ~Submeter_1/1000,
          name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Submeter_2/1000,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Submeter_3/1000,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~Other/1000,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Seasonal energy consumption",
         xaxis = list(title = "Season"),
         yaxis = list (title = "kWh"),
         legend = list(orientation = "h",xanchor = "center",x = 0.5, y= -0.2))
 
## Seasonal plot
season.plot <- imputed.table %>%
  mutate(Season = ifelse(month == "Jan"| month == "Feb" | month == "Mar","Winter", ifelse(month == "Apr" | month == "May" | month == "Jun", "Spring", ifelse(month == "Jul" | month == "Aug" | month == "Sep", "Summer", "Autumn")))) %>%
  group_by(Season) %>%
  dplyr::summarize_at(vars(Submeter_1,Submeter_2,Submeter_3, Other), funs(sum)) %>% 
  ggplot(aes(x = as.factor(Season))) +
  geom_line(aes(y = Other/1000, group=1, color = "Other Areas")) +
  geom_line(aes(y = Submeter_1/1000, group=1, color = "Kitchen")) +
  geom_line(aes(y = Submeter_2/1000, group=1, color = "Laundry Room")) +
  geom_line(aes(y = Submeter_3/1000, group=1, color = "Water Heater & AC")) +
  theme_minimal() +
  labs(title = "Seasonal energy consumption",
       x = "Season", y = "Total enegry consumption (kWh)") +
  scale_color_manual(labels = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Areas"), values = wes_palette("Darjeeling1", n = 4)) +
  theme(legend.position="bottom", legend.title=element_blank())


##### Vizualizations to study granularity ####
aggregated_df <- c()
plots.gap.sub <- c()
plots.gap.sumsub <- c()
granularity <- c("year", "month", "day", "week", "hour")


for(g in granularity){
  aggregated_df[[g]] <- imputed.table %>%
    group_by(DateTime=floor_date(DateTime, g)) %>%
    summarise(GAP=round(sum(GAP/1000), 3),
              Other = round(sum(Other/1000), 3), 
              Submeter_1=round(sum(Submeter_1/1000), 3), 
              Submeter_2=round(sum(Submeter_2/1000), 3), 
              Submeter_3=round(sum(Submeter_3/1000), 3), 
              Submeters=round(sum(Submeters/1000), 3)) 
  
## Global Power vs Submetering in [[g]] 
plots.gap.sumsub[[g]] <- ggplot(data = aggregated_df[[g]], aes(x = DateTime)) +
    geom_line(aes(y = Submeters, color = "Submeters")) +
    geom_line(aes(y = GAP, color = "Global Power")) +
    theme_minimal()+
    labs(title = paste("Global Power vs Submeters per", g),
         x = "Time",
         y = "Power (kWh)") +
    theme(legend.position="bottom", legend.title=element_blank())
}
plots.gap.sumsub[["month"]]
plots.gap.sumsub[["year"]]
plots.gap.sumsub[["week"]]
plots.gap.sumsub[["day"]]
plots.gap.sumsub[["hour"]]


#### Power Consumption per submeter monthly ####
plot_ly(aggregated_df[["month"]], 
        x = ~aggregated_df[["month"]]$DateTime, 
        y = ~aggregated_df[["month"]]$Submeter_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~aggregated_df[["month"]]$Submeter_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~aggregated_df[["month"]]$Submeter_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~aggregated_df[["month"]]$Other,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = paste("Power Consumption per Submeter Monthly"),
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"), 
         legend = list(orientation = "h",xanchor = "center",x = 0.5, y= -0.3))


##### WEEKDAY MEAN CONSUMPTION #####
## Mean consumption based on the day of the week of a general day of 2007, 2008, 2009
by_wday <- aggregated_df[["day"]] %>%
  filter(year(DateTime) %in% c(2007,2008,2009)) %>%
  mutate(Weekday = wday(DateTime, label = TRUE, abbr = FALSE, week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(Weekday) %>%
  summarise(Other = mean(Other), Submeter_1=mean(Submeter_1),
            Submeter_2=mean(Submeter_2), Submeter_3 = mean(Submeter_3)) %>%
  ggplot(aes(x = as.factor(Weekday))) +
  geom_line(aes(y = Other, group=1, color = "Other Areas")) +
  geom_line(aes(y = Submeter_1, group=1, color = "Kitchen")) +
  geom_line(aes(y = Submeter_2, group=1, color = "Laundry Room")) +
  geom_line(aes(y = Submeter_3, group=1, color = "Water Heater & AC")) +
  theme_minimal() +
  labs(title = "Mean energy consumption for general day of week",
       x = "Day of week", y = "Mean energy consumption (kWh)") +
  scale_color_manual(labels = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Areas"), values = wes_palette("Darjeeling1", n = 4)) +
  theme(legend.position="bottom", legend.title=element_blank())


## Mean consumption based on the month of the year of a general month of 2007, 2008, 2009
by_month <- aggregated_df[["month"]] %>%
  filter(year(DateTime) %in% c(2007,2008,2009)) %>%
  mutate(mean_month = month(DateTime, label = TRUE, abbr = FALSE)) %>%
  group_by(mean_month) %>%
  summarise(Other = mean(Other), Submeter_1=mean(Submeter_1),
            Submeter_2=mean(Submeter_2), Submeter_3 = mean(Submeter_3)) %>%
  ggplot(aes(x = as.factor(mean_month))) +
  geom_line(aes(y = Other, group=1, color = "Other Areas")) +
  geom_line(aes(y = Submeter_1, group=1, color = "Kitchen")) +
  geom_line(aes(y = Submeter_2, group=1, color = "Laundry Room")) +
  geom_line(aes(y = Submeter_3, group=1, color = "Water Heater & AC")) +
  theme_minimal() +
  labs(title = "Mean energy consumption for general month of year",
       x = "Month of year", y = "Mean energy consumption (kWh)") +
  scale_color_manual(labels = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Areas"), values = wes_palette("Darjeeling1", n = 4)) +
  theme(legend.position="bottom", legend.title=element_blank())


## Mean consumption based on the hour of the day of a general hour of 2007, 2008, 2009
by_hour <- aggregated_df[["hour"]] %>%
  filter(year(DateTime) %in% c(2007,2008,2009)) %>%
  mutate(mean_hour = hour(DateTime)) %>%
  group_by(mean_hour) %>%
  summarise(Other = mean(Other), Submeter_1=mean(Submeter_1),
            Submeter_2=mean(Submeter_2), Submeter_3 = mean(Submeter_3)) %>%
  ggplot(aes(x = as.factor(mean_hour))) +
  geom_line(aes(y = Other, group=1, color = "Other Areas")) +
  geom_line(aes(y = Submeter_1, group=1, color = "Kitchen")) +
  geom_line(aes(y = Submeter_2, group=1, color = "Laundry Room")) +
  geom_line(aes(y = Submeter_3, group=1, color = "Water Heater & AC")) +
  scale_color_manual(labels = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Areas"), values = wes_palette("Darjeeling1", n = 4)) +
  theme_minimal() +
  labs(title = "Mean Energy consumption for general hour of day",
       x = "Hour of day", y = "Mean energy consumption (kWh)") +
  theme(legend.position="bottom", legend.title=element_blank())


#### NA CalanderMap of pad.table by GAP ####
date.table <- pad.table %>%
  select(DateTime, GAP) %>%
  separate(col=DateTime, into=c("Date", "Time"), sep=" ") %>%
  mutate(Date =lubridate::ymd(Date)) %>%
  mutate(year=year(Date)) %>% # can be omitted for 2006
  filter(year!=2006) %>% # can be omitted for 2006
  mutate(Missing = ifelse(is.na(GAP), 1, 0)) %>%
  group_by(Date) %>%
  summarise(counts=sum(Missing))
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R") 
pad.calendar<-calendarHeat(date.table$Date, date.table$counts, varname="Missing Data", color = "w2b") #"r2b"


#### Subsetting Data  ####
## April Week 18 2007 
AprWeek18_2007 <- filter(imputed.table, year== "2007", month == "Apr", week>=17 & week<=18)

plot_ly(AprWeek18_2007, x = ~AprWeek18_2007$DateTime, y = ~AprWeek18_2007$Submeter_1/1000, #plot week
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_lines(y = ~AprWeek18_2007$Submeter_2/1000,
            name = 'Laundry Room', mode = 'lines') %>%
  add_lines(y = ~AprWeek18_2007$Submeter_3/1000,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_lines(y = ~AprWeek18_2007$Other/1000,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Energy consumption per submeter in April, Week 18, 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Energy consumption (kWh)"),
         legend = list(orientation = "h",xanchor = "center",x = 0.5, y= -0.15))


#### August 13 2009 ####
Aug13_2009 <- filter(imputed.table, year== "2009", month == "Aug", day >= 12 & day <=16)

plot_ly(Aug13_2009, x = ~Aug13_2009$DateTime, y = ~Aug13_2009$Submeter_1/1000, #plot week
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_lines(y = ~Aug13_2009$Submeter_2/1000,
            name = 'Laundry Room', mode = 'lines') %>%
  add_lines(y = ~Aug13_2009$Submeter_3/1000,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_lines(y = ~Aug13_2009$Other/1000,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Energy consumption per submeter August 13, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Energy consumption (kWh)"),
         legend = list(orientation = "h",xanchor = "center",x = 0.5, y= -0.15))


## August 2010
Aug2010 <- filter(imputed.table, year== "2010", month == "Aug")

plot_ly(Aug2010, x = ~Aug2010$DateTime, y = ~Aug2010$Submeter_1/1000, #plot week
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_lines(y = ~Aug2010$Submeter_2/1000,
            name = 'Laundry Room', mode = 'lines') %>%
  add_lines(y = ~Aug2010$Submeter_3/1000,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_lines(y = ~Aug2010$Other/1000,
            name = 'Other Areas', mode = 'lines') %>%
  layout(title = "Energy consumption per submeter in August, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Energy consumption (kWh)"),
         legend = list(orientation = "h",xanchor = "center",x = 0.5, y= -0.15))

#### Modelling ####
## Preparing analysis data

## Subset data by month and summarise total energy usage
Cmonth <-aggregated_df[["month"]] %>%
  filter(year (DateTime) %in% c(2007, 2008, 2009, 2010))

## Create time series object with month frequency
tsmonth9 <- ts(Cmonth, frequency = 12, start=c(2007,1), end=c(2010,1))

## Plotting by month
autoplot(tsmonth9[, c("Submeter_1", "Submeter_2", "Submeter_3", "Other")]) +
  labs(title="Total monthly consumption", x="Year", y="Energy consumption (kWh)") +
  theme_minimal() +
  scale_color_manual(labels = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Areas"), values = wes_palette("Darjeeling1", n = 4)) +
  theme(legend.position="bottom", legend.title=element_blank())



## Subset data by day and summarise total energy usage in winter
CdayWinter <- aggregated_df[["day"]] %>%
  filter(year(DateTime) %in% c(2007, 2008, 2009, 2010)) %>%
  filter(month(DateTime) %in% c(1, 2, 3)) %>%
  mutate(Weekday = wday(DateTime, label = TRUE, abbr = FALSE, week_start = getOption("lubridate.week.start", 1))) %>%
  mutate(Week =week(DateTime))

## Continuity
CdayWinter <- CdayWinter[-c(91:95, 182:186, 272:276, 359:361), ]
View(CdayWinter)
head(CdayWinter, n=14)
## Create time series object by week in winter
tsdaywinter9 <- ts(CdayWinter, frequency = 7, start=c(2007,1), end=c(2010,1))

## Create object to save
save(CdayWinter,
     file = "~/Desktop/DA_Online/IoT Analytics/2 Visualize and Analyze Energy Data/CdayWinter.RData")  

## Plotting by week in winter
autoplot(tsdaywinter9[, c("Submeter_1", "Submeter_2", "Submeter_3", "Other")]) +
  labs(title="Total weekly consumption in winter", x="Year", y="Energy consumption (kWh)") +
  theme_minimal() +
  scale_color_manual(labels = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Areas"), values = wes_palette("Darjeeling1", n = 4)) +
  theme(legend.position="bottom", legend.title=element_blank())


## Subset data by day and summarise total energy usage in summer
CdaySummer <- aggregated_df[["day"]] %>%
  filter(year(DateTime) %in% c(2007, 2008, 2009, 2010)) %>%
  filter(month(DateTime) %in% c(7, 8, 9)) %>%
  mutate(Weekday = wday(DateTime, label = TRUE, abbr = FALSE, week_start = getOption("lubridate.week.start", 1)))
head(CdaySummer)

## Continuity
View(CdaySummer)
CdaySummer <- CdaySummer[-c(1, 93:98, 356:368), ]

## Create time series object by week in summer
tsdaysummer9 <- ts(CdaySummer, frequency = 7, start=c(2007,1), end=c(2010,1))

## Plotting by week in summer
autoplot(tsdaysummer9[, c("Submeter_1", "Submeter_2", "Submeter_3", "Other")]) +
  labs(title="Total weekly consumption in summer", x="Year", y="Energy consumption (kWh)") +
  theme_minimal() +
  scale_color_manual(labels = c("Kitchen", "Laundry Room", "Water Heater & AC", "Other Areas"), values = wes_palette("Darjeeling1", n = 4)) +
  theme(legend.position="bottom", legend.title=element_blank())


#### DATA PARTITION and  FORECASTING (MONTH) ####

## create data partition ts_month (2007-2010.11)
ts_month <- ts(Cmonth, frequency = 12, start=c(2007,1))

## Other Areas
train_month_SM0 <- window(ts_month[,"Other"], start = c(2007,1), end = c(2010,1))
test_month_SM0 <- window(ts_month[,"Other"], start= c(2010,1))

## Kitchen
train_month_SM1 <- window(ts_month[,"Submeter_1"], start = c(2007,1), end = c(2010,1))
test_month_SM1 <- window(ts_month[,"Submeter_1"], start= c(2010,1))

## Laundry Room
train_month_SM2 <- window(ts_month[,"Submeter_2"], start = c(2007,1), end = c(2010,1))
test_month_SM2 <- window(ts_month[,"Submeter_2"], start= c(2010,1))

## Water Heater & AC
train_month_SM3 <- window(ts_month[,"Submeter_3"], start = c(2007,1), end = c(2010,1))
test_month_SM3 <- window(ts_month[,"Submeter_3"], start= c(2010,1))

## GAP
train_month_GAP <- window(ts_month[,"GAP"], start = c(2007,1), end = c(2010,1))
test_month_GAP <- window(ts_month[,"GAP"], start= c(2010,1))


#### Linear regression (month) ####
## Other Areas
modelTSLM_month_SM0 <-tslm(train_month_SM0 ~ trend + season)
predictionTSLM_month_SM0_10 = forecast(modelTSLM_month_SM0, h=10) # forecast 10 months ahead
accuracy(predictionTSLM_month_SM0_10, test_month_SM0) #check metrics (vs prediction test)

## Kitchen
modelTSLM_month_SM1 <-tslm(train_month_SM1 ~ trend + season)
predictionTSLM_month_SM1_10 = forecast(modelTSLM_month_SM1, h=10) # forecast 10 months ahead
accuracy(predictionTSLM_month_SM1_10, test_month_SM1) #check metrics (vs prediction test)

## Laundry Room
modelTSLM_month_SM2 <-tslm(train_month_SM2 ~ trend + season)
predictionTSLM_month_SM2_10 = forecast(modelTSLM_month_SM2, h=10) # forecast 10 months ahead
accuracy(predictionTSLM_month_SM2_10, test_month_SM2) #check metrics (vs prediction test)

## Water Heater & AC
modelTSLM_month_SM3 <-tslm(train_month_SM3 ~ trend + season)
predictionTSLM_month_SM3_10 = forecast(modelTSLM_month_SM3, h=10) # forecast 10 months ahead
accuracy(predictionTSLM_month_SM3_10, test_month_SM3) #check metrics (vs prediction test)

## GAP
modelTSLM_month_GAP <-tslm(train_month_GAP ~ trend + season)
predictionTSLM_month_GAP_10 = forecast(modelTSLM_month_GAP, h=10) # forecast 10 months ahead
accuracy(predictionTSLM_month_GAP_10, test_month_GAP) #check metrics (vs prediction test)

## Plotting prediction 10 ahead
## Other Areas
autoplot(predictionTSLM_month_SM0_10) +
xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: Other Areas (monthly), 10 months ahead")

## Kitchen
autoplot(predictionTSLM_month_SM1_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: Kitchen (monthly), 10 months ahead")

## Laundry Room
autoplot(predictionTSLM_month_SM2_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: Laundry Room (monthly), 10 months ahead")

## Water Heater & AC
autoplot(predictionTSLM_month_SM3_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: Water Heater & AC (monthly), 10 months ahead")

## GAP
autoplot(predictionTSLM_month_GAP_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: GAP (monthly), 10 months ahead") 
  


# Comaparison plot of fitted and actual data
## Other Areas
autoplot(ts_month[,"Other"], series="Actual") +
  autolayer(predictionTSLM_month_SM0_10, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Other Areas (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Kitchen
autoplot(ts_month[,"Submeter_1"], series="Actual") +
  autolayer(predictionTSLM_month_SM1_10, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Kitchen (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Laundry Room
autoplot(ts_month[,"Submeter_2"], series="Actual") +
  autolayer(predictionTSLM_month_SM2_10, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Laundry Room (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Water Heater & AC
autoplot(ts_month[,"Submeter_3"], series="Actual") +
  autolayer(predictionTSLM_month_SM3_10, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Water Heater & AC (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# GAP
autoplot(ts_month[,"GAP"], series="Actual") +
  autolayer(predictionTSLM_month_GAP_10, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("GAP (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Future forecast
## Other Areas (forecast 20 months ahead)
predictionTSLM_month_SM0_20 <- forecast(modelTSLM_month_SM0, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (Other Areas): 20 months forecasting")
predictionTSLM_month_SM0_20

## Kitchen (forecast 20 months ahead)
predictionTSLM_month_SM1_20 <- forecast(modelTSLM_month_SM1, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (Kitchen): 20 months forecasting")
predictionTSLM_month_SM1_20

## Laundry Room (forecast 20 months ahead)
predictionTSLM_month_SM2_20 <- forecast(modelTSLM_month_SM2, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (Laundry Room): 20 months forecasting")
predictionTSLM_month_SM2_20

## Water Heater & AC (forecast 20 months ahead)
predictionTSLM_month_SM3_20 <- forecast(modelTSLM_month_SM3, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (Water Heater & AC): 20 months forecasting")
predictionTSLM_month_SM3_20

## GAP (forecast 20 months ahead)
predictionTSLM_month_GAP_20 <- forecast(modelTSLM_month_GAP, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (GAP): 20 months forecasting")
predictionTSLM_month_GAP_20


#### Holt-Winters (month) ####
## Other Areas
modelHW_month_SM0 <-HoltWinters(train_month_SM0)
predictionHW_month_SM0_10 = forecast(modelHW_month_SM0, h=10) # forecast 10 months ahead
accuracy(predictionHW_month_SM0_10, test_month_SM0) #check metrics (vs prediction test)

## Kitchen
modelHW_month_SM1 <-HoltWinters(train_month_SM1)
predictionHW_month_SM1_10 = forecast(modelHW_month_SM1, h=10) # forecast 10 months ahead
accuracy(predictionHW_month_SM1_10, test_month_SM1) #check metrics (vs prediction test)

## Laundry Room
modelHW_month_SM2 <-HoltWinters(train_month_SM2)
predictionHW_month_SM2_10 = forecast(modelHW_month_SM2, h=10) # forecast 10 months ahead
accuracy(predictionHW_month_SM2_10, test_month_SM2) #check metrics (vs prediction test)

## Water Heater & AC
modelHW_month_SM3 <-HoltWinters(train_month_SM3)
predictionHW_month_SM3_10 = forecast(modelHW_month_SM3, h=10) # forecast 10 months ahead
accuracy(predictionHW_month_SM3_10, test_month_SM3) #check metrics (vs prediction test)

## GAP
modelHW_month_GAP <-HoltWinters(train_month_GAP)
predictionHW_month_GAP_10 = forecast(modelHW_month_GAP, h=10) # forecast 10 months ahead
accuracy(predictionHW_month_GAP_10, test_month_GAP) #check metrics (vs prediction test)

## Plotting prediction 10 ahead
## Other Areas
autoplot(predictionHW_month_SM0_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Winters: Other Areas (monthly), 10 months ahead")

## Kitchen
autoplot(predictionHW_month_SM1_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Winters: Kitchen (monthly), 10 months ahead")

## Laundry Room
autoplot(predictionHW_month_SM2_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Winters: Laundry Room (monthly), 10 months ahead")

## Water Heater & AC
autoplot(predictionHW_month_SM3_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Winters: WaterHeater&AC (monthly), 10 months ahead")

## GAP
autoplot(predictionHW_month_GAP_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Winters: GAP (monthly), 10 months ahead") 


# Comaparison plot of fitted and actual data
## Other Areas
autoplot(ts_month[,"Other"], series="Actual") +
  autolayer(predictionHW_month_SM0_10, series="Holt-Winters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Other Areas (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Kitchen
autoplot(ts_month[,"Submeter_1"], series="Actual") +
  autolayer(predictionHW_month_SM1_10, series="Holt-Winters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Kitchen (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Laundry Room
autoplot(ts_month[,"Submeter_2"], series="Actual") +
  autolayer(predictionHW_month_SM2_10, series="Holt-WInters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Laundry Room (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Water Heater & AC
autoplot(ts_month[,"Submeter_3"], series="Actual") +
  autolayer(predictionHW_month_SM3_10, series="Holt-Winters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Water Heater & AC (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# GAP
autoplot(ts_month[,"GAP"], series="Actual") +
  autolayer(predictionHW_month_GAP_10, series="Holt-Winters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("GAP (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Future forecast ##
## Other Areas (# forecast 20 months ahead)
predictionHW_month_SM0_20 <- forecast(modelHW_month_SM0, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Winters (Other Areas): 20 months forecasting")
predictionHW_month_SM0_20

## Kitchen (forecast 20 months ahead)
predictionHW_month_SM1_20 <- forecast(modelHW_month_SM1, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Wintrs (Kitchen): 20 months forecasting")
predictionHW_month_SM1_20

## Laundry Room (forecast 20 months ahead)
predictionHW_month_SM2_20 <- forecast(modelHW_month_SM2, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Winters (Laundry Room): 20 months forecasting")
predictionHW_month_SM2_20

## Water Heater & AC (forecast 20 months ahead)
predictionHW_month_SM3_20 <- forecast(modelHW_month_SM3, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Winters (Water Heater & AC): 20 months forecasting")
predictionHW_month_SM3_20

## GAP (forecast 20 months ahead)
predictionHW_month_GAP_20 <- forecast(modelHW_month_GAP, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Holt-Winters (GAP): 20 months forecasting")
predictionHW_month_GAP_20


#### Arima (month) ####
## Other Areas
modelArima_month_SM0 <-auto.arima(train_month_SM0)
predictionArima_month_SM0_10 = forecast(modelArima_month_SM0, h=10) # forecast 10 months ahead
accuracy(predictionArima_month_SM0_10, test_month_SM0) #check metrics (vs prediction test)

## Kitchen
modelArima_month_SM1 <-auto.arima(train_month_SM1)
predictionArima_month_SM1_10 = forecast(modelArima_month_SM1, h=10) # forecast 10 months ahead
accuracy(predictionArima_month_SM1_10, test_month_SM1) #check metrics (vs prediction test)

## Laundry Room
modelArima_month_SM2 <-auto.arima(train_month_SM2)
predictionArima_month_SM2_10 = forecast(modelArima_month_SM2, h=10) # forecast 10 months ahead
accuracy(predictionArima_month_SM2_10, test_month_SM2) #check metrics (vs prediction test)

## Water Heater & AC
modelArima_month_SM3 <-auto.arima(train_month_SM3)
predictionArima_month_SM3_10 = forecast(modelArima_month_SM3, h=10) # forecast 10 months ahead
accuracy(predictionArima_month_SM3_10, test_month_SM3) #check metrics (vs prediction test)

## GAP
modelArima_month_GAP <-auto.arima(train_month_GAP)
predictionArima_month_GAP_10 = forecast(modelArima_month_GAP, h=10) # forecast 10 months ahead
accuracy(predictionArima_month_GAP_10, test_month_GAP) #check metrics (vs prediction test)

## Plotting prediction 10 ahead
## Other Areas
autoplot(predictionArima_month_SM0_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima: Other Areas (monthly), 10 months ahead")

## Kitchen
autoplot(predictionArima_month_SM1_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima: Kitchen (monthly), 10 months ahead")

## Laundry Room
autoplot(predictionArima_month_SM2_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima: Laundry Room (monthly), 10 months ahead")

## Water Heater & AC
autoplot(predictionArima_month_SM3_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima: Water Heater & AC (monthly), 10 months ahead")

## GAP
autoplot(predictionArima_month_GAP_10) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima: GAP (monthly), 10 months ahead") 


# Comaparison plot of fitted and actual data
## Other Areas
autoplot(ts_month[,"Other"], series="Actual") +
  autolayer(predictionArima_month_SM0_10, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Other Areas (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Kitchen
autoplot(ts_month[,"Submeter_1"], series="Actual") +
  autolayer(predictionArima_month_SM1_10, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Kitchen (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Laundry Room
autoplot(ts_month[,"Submeter_2"], series="Actual") +
  autolayer(predictionArima_month_SM2_10, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Laundry Room (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Water Heater & AC
autoplot(ts_month[,"Submeter_3"], series="Actual") +
  autolayer(predictionArima_month_SM3_10, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Water Hater & AC (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# GAP
autoplot(ts_month[,"GAP"], series="Actual") +
  autolayer(predictionArima_month_GAP_10, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("GAP (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Future forecast (monthly)
## Other Areas (# forecast 20 months ahead)
predictionArima_month_SM0_20 <- forecast(modelArima_month_SM0, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (Other Areas): 20 months forecasting")
predictionArima_month_SM0_20

## Kitchen (forecast 20 months ahead)
predictionArima_month_SM1_20 <- forecast(modelArima_month_SM1, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (Kitchen): 20 months forecasting")
predictionArima_month_SM1_20

## Laundry Room (forecast 20 months ahead)
predictionArima_month_SM2_20 <- forecast(modelArima_month_SM2, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (Laundry Room): 20 months forecasting")
predictionArima_month_SM2_20

## Water Heater & AC (forecast 20 months ahead)
predictionArima_month_SM3_20 <- forecast(modelArima_month_SM3, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (Water Heater & AC): 20 months forecasting")
predictionArima_month_SM3_20

## GAP (forecast 20 months ahead)
predictionArima_month_GAP_20 <- forecast(modelArima_month_GAP, h=20) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (GAP): 20 months forecasting")
predictionArima_month_GAP_20






#### Comaparison plot of Fitted and Actual Values (month) ####
autoplot(ts_month[,"GAP"], series="Actual") +
  autolayer(predictionTSLM_month_GAP_10, series="TSLM", PI= FALSE) +
  autolayer(predictionHW_month_GAP_10, series="Holt-Winters", PI= FALSE) +
  autolayer(predictionArima_month_GAP_10, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") + 
  ggtitle("Comparison of Models (GAP): Actual vs Fitted (monthly)") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())



#### DATA PARTITION and  FORECASTING (Weekly in Winter) ####
View(CdayWinter)
## create data partition ts_month (2007-2010.11)
ts_weekly_winter <- ts(CdayWinter, start=c(2007,1), end=c(2010,7), frequency = 7)
head(ts_weekly_winter, n=14)
## Other Areas
train_weekly_winter_SM0 <- window(ts_weekly_winter[,"Other"], start = c(2007,1), end=c(2010,1))
test_weekly_winter_SM0 <- window(ts_weekly_winter[,"Other"], start=c(2010,1))

## Kitchen
train_weekly_winter_SM1 <- window(ts_weekly_winter[,"Submeter_1"], start = c(2007,1), end = c(2010,1))
test_weekly_winter_SM1 <- window(ts_weekly_winter[,"Submeter_1"], start= c(2010,1))

## Laundry Room
train_weekly_winter_SM2 <- window(ts_weekly_winter[,"Submeter_2"], start = c(2007,1), end = c(2010,1))
test_weekly_winter_SM2 <- window(ts_weekly_winter[,"Submeter_2"], start= c(2010,1))

## Water Heater & AC
train_weekly_winter_SM3 <- window(ts_weekly_winter[,"Submeter_3"], start = c(2007,1), end = c(2010,1))
test_weekly_winter_SM3 <- window(ts_weekly_winter[,"Submeter_3"], start= c(2010,1))

## GAP
train_weekly_winter_GAP <- window(ts_weekly_winter[,"GAP"], start = c(2007,1), end = c(2010,1))
test_weekly_winter_GAP <- window(ts_weekly_winter[,"GAP"], start= c(2010,1))


#### Linear regression (weekly in winter) ####
## Other Areas
modelTSLM_weekly_winter_SM0 <-tslm(train_weekly_winter_SM0 ~ trend + season)
predictionTSLM_weekly_winter_SM0_12 = forecast(modelTSLM_weekly_winter_SM0, h=7) # forecast 12 weeks ahead
accuracy(predictionTSLM_weekly_winter_SM0_12, test_weekly_winter_SM0) #check metrics (vs prediction test)

## Kitchen
modelTSLM_weekly_winter_SM1 <-tslm(train_weekly_winter_SM1 ~ trend + season)
predictionTSLM_weekly_winter_SM1_12 = forecast(modelTSLM_weekly_winter_SM1, h=7) # forecast 12 weeks ahead
accuracy(predictionTSLM_weekly_winter_SM1_12, test_weekly_winter_SM1) #check metrics (vs prediction test)

## Laundry Room
modelTSLM_weekly_winter_SM2 <-tslm(train_weekly_winter_SM2 ~ trend + season)
predictionTSLM_weekly_winter_SM2_12 = forecast(modelTSLM_weekly_winter_SM2, h=7) # forecast 12 weeks ahead
accuracy(predictionTSLM_weekly_winter_SM2_12, test_weekly_winter_SM2) #check metrics (vs prediction test)

## Water Heater & AC
modelTSLM_weekly_winter_SM3 <-tslm(train_weekly_winter_SM3 ~ trend + season)
predictionTSLM_weekly_winter_SM3_12 = forecast(modelTSLM_weekly_winter_SM3, h=7) # forecast 12 weeks ahead
accuracy(predictionTSLM_weekly_winter_SM3_12, test_weekly_winter_SM3) #check metrics (vs prediction test)

## GAP
modelTSLM_weekly_winter_GAP <-tslm(train_weekly_winter_GAP ~ trend + season)
predictionTSLM_weekly_winter_GAP_12 = forecast(modelTSLM_weekly_winter_GAP, h=7) # forecast 12 weeks ahead
accuracy(predictionTSLM_weekly_winter_GAP_12, test_weekly_winter_GAP) #check metrics (vs prediction test)

## Plotting prediction 4 weeks ahead
## Other Areas
autoplot(predictionTSLM_weekly_winter_SM0_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: Other Areas (weekly in winter), 12 weeks ahead")

## Kitchen
autoplot(predictionTSLM_weekly_winter_SM1_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: Kitchen (weekly in winter), 12 weeks ahead")

## Laundry Room
autoplot(predictionTSLM_weekly_winter_SM2_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: Laundry Room (weekly in winter), 12 weeks ahead")

## Water Heater & AC
autoplot(predictionTSLM_weekly_winter_SM3_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: Water Heater & AC (weekly in winter), 12 weeks ahead")

## GAP
autoplot(predictionTSLM_weekly_winter_GAP_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM: GAP (weekly in winter), 12 weeks ahead") 



# Comaparison plot of fitted and actual data
## Other Areas
autoplot(ts_weekly_winter[,"Other"], series="Actual") +
  autolayer(predictionTSLM_weekly_winter_SM0_12, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Other Areas (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Kitchen
autoplot(ts_weekly_winter[,"Submeter_1"], series="Actual") +
  autolayer(predictionTSLM_weekly_winter_SM1_12, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Kitchen (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Laundry Room
autoplot(ts_weekly_winter[,"Submeter_2"], series="Actual") +
  autolayer(predictionTSLM_weekly_winter_SM2_12, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Laundry Room (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Water Heater & AC
autoplot(ts_weekly_winter[,"Submeter_3"], series="Actual") +
  autolayer(predictionTSLM_weekly_winter_SM3_12, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Water Hater & AC (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# GAP
autoplot(ts_weekly_winter[,"GAP"], series="Actual") +
  autolayer(predictionTSLM_weekly_winter_GAP_12, series="TSLM", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("GAP (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Future forecast
## Other Areas (forecast 24 weeks ahead)
predictionTSLM_weekly_winter_SM0_24 <- forecast(modelTSLM_weekly_winter_SM0, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (Other Areas): 24 weeks forecasting")
predictionTSLM_weekly_winter_SM0_24

## Kitchen (forecast 24 weeks ahead)
predictionTSLM_weekly_winter_SM1_24 <- forecast(modelTSLM_weekly_winter_SM1, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (Kitchen): 24 weeks forecasting")
predictionTSLM_weekly_winter_SM1_24

## Laundry Room (forecast 24 weeks ahead)
predictionTSLM_weekly_winter_SM2_24 <- forecast(modelTSLM_weekly_winter_SM2, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (Laundry Room): 24 weeks forecasting")
predictionTSLM_weekly_winter_SM2_24

## Water Heater & AC (forecast 24 weeks ahead)
predictionTSLM_weekly_winter_SM3_24 <- forecast(modelTSLM_weekly_winter_SM3, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (Water Heater & AC): 24 weeks forecasting")
predictionTSLM_weekly_winter_SM3_24

## GAP (forecast 24 weeks ahead)
predictionTSLM_weekly_winter_GAP_24 <- forecast(modelTSLM_weekly_winter_GAP, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("TSLM (GAP): 24 weeks forecasting")
predictionTSLM_weekly_winter_GAP_24


#### Halt Winters (weekly in winter) ####
## Other Areas
modelHW_weekly_winter_SM0 <-HoltWinters(train_weekly_winter_SM0)
predictionHW_weekly_winter_SM0_12 = forecast(modelHW_weekly_winter_SM0, h=7) # forecast 12 weeks ahead
accuracy(predictionHW_weekly_winter_SM0_12, test_weekly_winter_SM0) #check metrics (vs prediction test)

## Kitchen
modelHW_weekly_winter_SM1 <-HoltWinters(train_weekly_winter_SM1)
predictionHW_weekly_winter_SM1_12 = forecast(modelHW_weekly_winter_SM1, h=7) # forecast 12 weeks ahead
accuracy(predictionHW_weekly_winter_SM1_12, test_weekly_winter_SM1) #check metrics (vs prediction test)

## Laundry Room
modelHW_weekly_winter_SM2 <-HoltWinters(train_weekly_winter_SM2)
predictionHW_weekly_winter_SM2_12 = forecast(modelHW_weekly_winter_SM2, h=7) # forecast 12 weeks ahead
accuracy(predictionHW_weekly_winter_SM2_12, test_weekly_winter_SM2) #check metrics (vs prediction test)

## Water Heater & AC
modelHW_weekly_winter_SM3 <-HoltWinters(train_weekly_winter_SM3)
predictionHW_weekly_winter_SM3_12 = forecast(modelHW_weekly_winter_SM3, h=7) # forecast 12 weeks ahead
accuracy(predictionHW_weekly_winter_SM3_12, test_weekly_winter_SM3) #check metrics (vs prediction test)

## GAP
modelHW_weekly_winter_GAP <-HoltWinters(train_weekly_winter_GAP)
predictionHW_weekly_winter_GAP_12 = forecast(modelHW_weekly_winter_GAP, h=7) # forecast 12 weeks ahead
accuracy(predictionHW_weekly_winter_GAP_12, test_weekly_winter_GAP) #check metrics (vs prediction test)

## Plotting prediction 12 weeks ahead
## Other Areas
autoplot(predictionHW_weekly_winter_SM0_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Winters (weekly in winter): Other Areas, 12 weeks ahead")

## Kitchen
autoplot(predictionHW_weekly_winter_SM1_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Winters (weekly in winter): Kitchen, 12 weeks ahead")

## Laundry Room
autoplot(predictionHW_weekly_winter_SM2_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Winters (weekly in winter): Laundry Room, 12 weeks ahead")

## Water Heater & AC
autoplot(predictionHW_weekly_winter_SM3_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Winters (weekly in winter): Water Heater & AC, 12 weeks ahead")

## GAP
autoplot(predictionHW_weekly_winter_GAP_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Winters (weekly in winter): GAP, 12 weeks ahead") 


# Comaparison plot of fitted and actual data
## Other Areas
autoplot(ts_weekly_winter[,"Other"], series="Actual") +
  autolayer(predictionHW_weekly_winter_SM0_12, series="Halt-Winters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Other Areas (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Kitchen
autoplot(ts_weekly_winter[,"Submeter_1"], series="Actual") +
  autolayer(predictionHW_weekly_winter_SM1_12, series="Halt-Winters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Kitchen (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Laundry Room
autoplot(ts_weekly_winter[,"Submeter_2"], series="Actual") +
  autolayer(predictionHW_weekly_winter_SM2_12, series="Halt-WInters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Laundry Room (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Water Heater & AC
autoplot(ts_weekly_winter[,"Submeter_3"], series="Actual") +
  autolayer(predictionHW_weekly_winter_SM3_12, series="Halt-Winters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Water Hater & AC (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# GAP
autoplot(ts_weekly_winter[,"GAP"], series="Actual") +
  autolayer(predictionHW_weekly_winter_GAP_12, series="Halt-Winters", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("GAP (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Future forecast (forecast 24 weeks ahead)
## Other Areas
predictionHW_weekly_winter_SM0_24 <- forecast(modelHW_weekly_winter_SM0, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Winters (Other Areas): 24 weeks forecasting (weekly in winter)")
predictionHW_weekly_winter_SM0_24

## Kitchen 
predictionHW_weekly_winter_SM1_24 <- forecast(modelHW_weekly_winter_SM1, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Wintrs (Kitchen): 24 weeks forecasting (weekly in winter)")
predictionHW_weekly_winter_SM1_24

## Laundry Room
predictionHW_weekly_winter_SM2_24 <- forecast(modelHW_weekly_winter_SM2, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Winters (Laundry Room): 24 weeks forecasting (weekly in winter)")
predictionHW_weekly_winter_SM2_20

## Water Heater & AC 
predictionHW_weekly_winter_SM3_24 <- forecast(modelHW_weekly_winter_SM3, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Winters (Water Heater & AC): 24 weeks forecasting (weekly in winter)")
predictionHW_weekly_winter_SM3_24

## GAP 
predictionHW_weekly_winter_GAP_24 <- forecast(modelHW_weekly_winter_GAP, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Halt-Winters (GAP): 24 weeks forecasting (weekly in winter)")
predictionHW_weekly_winter_GAP_24


#### Arima (weekly in winter) ####
## Other Areas
modelArima_weekly_winter_SM0 <-auto.arima(train_weekly_winter_SM0)
predictionArima_weekly_winter_SM0_12 = forecast(modelArima_weekly_winter_SM0, h=7) # forecast 12 weeks ahead
accuracy(predictionArima_weekly_winter_SM0_12, test_weekly_winter_SM0) #check metrics (vs prediction test)

## Kitchen
modelArima_weekly_winter_SM1 <-auto.arima(train_weekly_winter_SM1)
predictionArima_weekly_winter_SM1_12 = forecast(modelArima_weekly_winter_SM1, h=7) # forecast 12 weeks ahead
accuracy(predictionArima_weekly_winter_SM1_12, test_weekly_winter_SM1) #check metrics (vs prediction test)

## Laundry Room
modelArima_weekly_winter_SM2 <-auto.arima(train_weekly_winter_SM2)
predictionArima_weekly_winter_SM2_12 = forecast(modelArima_weekly_winter_SM2, h=7) # forecast 12 weeks ahead
accuracy(predictionArima_weekly_winter_SM2_12, test_weekly_winter_SM2) #check metrics (vs prediction test)

## Water Heater & AC
modelArima_weekly_winter_SM3 <-auto.arima(train_weekly_winter_SM3)
predictionArima_weekly_winter_SM3_12 = forecast(modelArima_weekly_winter_SM3, h=7) # forecast 12 weeks ahead
accuracy(predictionArima_weekly_winter_SM3_12, test_weekly_winter_SM3) #check metrics (vs prediction test)

## GAP
modelArima_weekly_winter_GAP <-auto.arima(train_weekly_winter_GAP)
predictionArima_weekly_winter_GAP_12 = forecast(modelArima_weekly_winter_GAP, h=7) # forecast 12 weeks ahead
accuracy(predictionArima_weekly_winter_GAP_12, test_weekly_winter_GAP) #check metrics (vs prediction test)

## Plotting prediction 10 ahead
## Other Areas
autoplot(predictionArima_weekly_winter_SM0_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (weekly in winter): Other Areas, 12 weeks ahead")

## Kitchen
autoplot(predictionArima_weekly_winter_SM1_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (weekly in winter): Kitchen, 12 weeks ahead")

## Laundry Room
autoplot(predictionArima_weekly_winter_SM2_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (weekly in winter): Laundry Room, 12 weeks ahead")

## Water Heater & AC
autoplot(predictionArima_weekly_winter_SM3_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (weekly in winter): Water Heater & AC, 12 weeks ahead")

## GAP
autoplot(predictionArima_weekly_winter_GAP_12) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (weekly in winter): GAP, 12 weeks ahead") 


# Comaparison plot of fitted and actual data
## Other Areas
autoplot(ts_weekly_winter[,"Other"], series="Actual") +
  autolayer(predictionArima_weekly_winter_SM0_12, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Other Areas (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Kitchen
autoplot(ts_weekly_winter[,"Submeter_1"], series="Actual") +
  autolayer(predictionArima_weekly_winter_SM1_12, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Kitchen (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Laundry Room
autoplot(ts_weekly_winter[,"Submeter_2"], series="Actual") +
  autolayer(predictionArima_weekly_winter_SM2_12, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Laundry Room (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# Water Heater & AC
autoplot(ts_weekly_winter[,"Submeter_3"], series="Actual") +
  autolayer(predictionArima_weekly_winter_SM3_12, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Water Hater & AC (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

# GAP
autoplot(ts_weekly_winter[,"GAP"], series="Actual") +
  autolayer(predictionArima_weekly_winter_GAP_12, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("GAP (weekly in winter): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

## Future forecast (forecast 24 weeks ahead)
## Other Areas 
predictionArima_weekly_winter_SM0_24 <- forecast(modelArima_weekly_winter_SM0, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (Other Areas): 24 wekks forecasting (weekly in winter)")
predictionArima_weekly_winter_SM0_24

## Kitchen
predictionArima_weekly_winter_SM1_24 <- forecast(modelArima_weekly_winter_SM1, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (Kitchen): 24 weeks forecasting (weekly in winter)")
predictionArima_weekly_winter_SM1_24

## Laundry Room (forecast 20 months ahead)
predictionArima_weekly_winter_SM2_24 <- forecast(modelArima_weekly_winter_SM2, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (Laundry Room): 24 weeks forecasting (weekly in winter)")
predictionArima_weekly_winter_SM2_24

## Water Heater & AC (forecast 20 months ahead)
predictionArima_weekly_winter_SM3_24 <- forecast(modelArima_weekly_winter_SM3, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (Water Heater & AC): 24 weeks forecasting (weekly in winter)")
predictionArima_weekly_winter_SM3_24

## GAP (forecast 20 months ahead)
predictionArima_weekly_winter_GAP_24 <- forecast(modelArima_weekly_winter_GAP, h=14) %>%
  autoplot() +
  xlab("Year") + ylab("kWh") +
  ggtitle("Arima (GAP): 24 weeks forecasting (weekly in winter)")
predictionArima_weekly_winter_GAP_24



#### Comaparison plot of Fitted and Actual Values (weekly in winter) ####
autoplot(ts_weekly_winter[,"GAP"], series="Actual") +
  autolayer(predictionTSLM_weekly_winter_GAP_12, series="TSLM", PI= FALSE) +
  autolayer(predictionHW_weekly_winter_GAP_12, series="Holt-Winters", PI= FALSE) +
  autolayer(predictionArima_weekly_winter_GAP_12, series="Arima", PI= FALSE) +
  xlab("Year") + ylab("kWh") + 
  ggtitle("Comparison of Models (GAP): Actual vs Fitted (weekly in winter)") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

#### Boxplot ####
box.tidy.table <- tidy.table %>%
  filter(!year %in% c(2006,2010))  %>%
  ggplot(aes(x=factor(Consumption), y=Watt_hour/1000, color=Consumption)) + labs(x="Submeter", y="Energy consumption (kWh)") +
  scale_color_manual(labels = c("Other Areas", "Kitchen", "Laundry Room", "Water Heater & AC"), values = wes_palette("Royal1", n = 4)) +
  geom_boxplot() +
  # Add a vertical line at 1000 kWh level
  geom_hline(aes(yintercept = 0.04), size=2, color="darkgrey")+
  theme(legend.position="bottom", legend.title=element_blank())
   
  

accuracy(predictionTSLM_month_GAP_10, test_month_GAP) #check metrics (vs prediction test)
accuracy(predictionHW_month_GAP_10, test_month_GAP) #check metrics (vs prediction test)
accuracy(predictionArima_month_GAP_10, test_month_GAP) #check metrics (vs prediction test)
