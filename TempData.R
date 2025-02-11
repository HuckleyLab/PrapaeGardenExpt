library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(nlme)
library(lme4)
library(stringr)

# Load required libraries
library(lubridate)
library(tidyr)
library(readxl)

#Taylor version: https://github.com/taylorhatcher/WARP2024/tree/main

#toggle between desktop (y) and laptop (n)
desktop<- "y"

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/WeatherData/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/WeatherData/")

#1997
temp.1997= read.csv("CUH.metdata.1997_viii12_19.csv") #8/12 - 8/26
temp.1997$Period<- 1
temp2.1997= read.csv("CUH.metdata.1997_viii20_26.csv")
temp2.1997$Period<- 2
temp.1997= rbind(temp.1997,temp2.1997)
#format time
min <- str_sub(temp.1997$Time,-2,-1)
min[min %in% c(":0",":5")]<- paste(":0", str_sub(min,-1,-1), sep="")
hr<- str_sub(temp.1997$Time,-4,-3)
hr[hr==""]<- "0"
temp.1997$hm<- paste(hr, min, sep=":")

temp.1999= read.csv("FormattedHistoricMetDataFieldSln.csv") #8/18-8/25

temp.2024= read.csv("combined_loggers_2024.csv") #6/21 - 8/18

#format data


#plot time series

#-----------
# calculate daily means for met data from 1999
dailyhistoriclogger_means <- kinghistmetdata %>%
  group_by(Variable, DATE) %>%
  summarise(MeanTemperature = mean(TAIR, na.rm = TRUE), .groups = 'drop')

# Plot daily mean historic temperature distributions 
ggplot(dailyhistoriclogger_means, aes(x = MeanTemperature, color = Variable, group = Variable)) +
  geom_density(alpha = 0.7) +
  scale_fill_viridis_d() +
  labs(
    title = "1999 Daily Mean Temperature Distributions",
    x = "Mean Temperature (°C)",
    y = "Density",
    color = "Logger"
  ) +
  theme_classic(base_size = 20)



# Manually assign 'Interval' to historic data 
dailyhistoriclogger_means <- dailyhistoriclogger_means %>%
  mutate(Interval = "1999")  

# Assign 'Interval' to the 2024 data (already done in your code)
dailylogger_means <- dailylogger_means %>%
  mutate(Interval = factor(Interval)) 

# Combine both 2024 and 1999 data
combined_means <- bind_rows(
  dailylogger_means %>% mutate(Source = "2024"),
  dailyhistoriclogger_means %>% mutate(Source = "1999")
)

# Plotting
ggplot() +
  # Plot density lines for 2024 data
  geom_density(data = filter(combined_means, Source == "2024"),
               aes(x = MeanTemperature, color = Interval, group = Interval),
               size = 1) +
  
  # Plot density lines for 1999 data
  geom_density(data = filter(combined_means, Source == "1999"),
               aes(x = MeanTemperature, color = Interval, group = Interval),
               size = 1, alpha = 0.7) +
  
  # Color scales for the intervals
  scale_color_viridis_d(option = "D") +
  
  labs(
    title = "1999 vs 2024 Operative Temp Logger Daily Mean Distribution",
    x = "Mean Temperature (°C)",
    y = "Density",
    color = "Date Interval"
  ) +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#----------------
  # Figure 2. TPC comparison
  
  #save figure
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  pdf("Fig2_PrapaeTPC_2024.pdf",height = 6, width = 15)
  tpc.plot.h + tpc.plot +tpc.all.plot
  dev.off()
  
  
  #==================


  

  # Plot all TM columns as separate lines
  AllTemperaturePlot <- ggplot(histmicroclimdata_long, aes(x = DATETIME, y = Value, color = Variable)) +
    geom_point() +
    scale_color_viridis_d()+
    labs(
      title = "Historic 1999 Field Seln Microclimate Temperatures",
      x = "Datetime",
      y = "Temperature (°C)",
      color = "Sensor"
    ) +
    theme_minimal()
  
  #print Temperature plot plot
  print(AllTemperaturePlot)
  
  # Calculate temp extremes for data frame
  temp_extremes <- histmicroclimdata_long %>%
    group_by(Variable) %>%
    summarise(T_min = min(Value, na.rm = TRUE),
              T_max = max(Value, na.rm = TRUE),
              .groups = 'drop')
  
  # Reshape the temp_extremes data into long format for plotting
  temp_extremes_long <- temp_extremes %>%
    pivot_longer(cols = c(T_min, T_max), names_to = "Temperature_Type", values_to = "Temperature")
  
  # Min and Max distribution plot for each temperature logger
  histminmaxplot <- ggplot(temp_extremes_long, aes(x = Variable, y = Temperature, fill = Temperature_Type)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_viridis_d() +
    labs(
      title = "1999 Field Seln Minimum and Maximum Temperatures by Logger",
      x = "Logger",
      y = "Temperature (°C)",
      fill = "Temperature Type"
    ) +
    theme_classic(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(histminmaxplot)
  
  # Combine DATE and LONGTIME into a single datetime column
  histhourly_means <- histmicroclimdata_long %>%
    mutate(HourlyTime = floor_date(DATETIME, "hour")) %>%
    group_by(HourlyTime, Variable) %>%
    summarise(MeanTemperature = mean(Value, na.rm = TRUE),
              .groups = 'drop')
  # Calculate hourly means for each temperature logger
  histhourly_means <- histmicroclimdata_long %>%
    mutate(HourlyTime = floor_date(DATETIME, "hour")) %>% # Round datetime to the nearest hour
    group_by(HourlyTime, Variable) %>%                   # Group by rounded hour and logger
    summarise(MeanTemperature = mean(Value, na.rm = TRUE), .groups = 'drop') # Calculate mean
  
  # Plot hourly means for each temperature logger in the field
  hourly_plot <- ggplot(histhourly_means, aes(x = HourlyTime, y = MeanTemperature, color = Variable)) +
    geom_line() + # Use line plot to show trends
    scale_color_viridis_d() + 
    labs(
      title = "1999 Field Seln Hourly Mean Temperatures by Logger",
      x = "Datetime",
      y = "Mean Temperature (°C)",
      color = "Sensor"
    ) +
    theme_minimal() + # Clean plot style
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
    )
  
  # Print the hourly data plot
  print(hourly_plot)
  
  #===========================
  
  # Define the start and end dates for filtering
  start_date <- as.POSIXct("2024-06-22", format = "%Y-%m-%d")
  end_date <-
    as.POSIXct("2024-08-15 23:59:59", format = "%Y-%m-%d %H:%M:%S")
  
  # Convert datetime to just date for daily means
  filtered_data_day <- long_data%>%
    mutate(DATE = as.Date(datetime))
  
  # Divide logger information into time intervals so it is easier to visualize, split into experiment times when caterpillars were in the field 
  filtered_data_day <- long_data %>%
    mutate(DATE = as.Date(datetime)) %>%
    mutate(Interval = case_when(
      DATE >= as.Date("2024-06-21") & DATE <= as.Date("2024-07-03") ~ "2024-06-21 to 2024-07-03",
      DATE >= as.Date("2024-07-03") & DATE <= as.Date("2024-07-28") ~ "2024-07-03 to 2024-07-28",
      DATE >= as.Date("2024-07-28") & DATE <= as.Date("2024-08-18") ~ "2024-07-28 to 2024-08-18",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(Interval))
  
  # Calculate daily mean temperatures for all loggers combined
  dailylogger_means <- filtered_data_day %>%
    group_by(DATE, Interval) %>%
    summarise(MeanTemperature = mean(Temperature, na.rm = TRUE), .groups = 'drop')
  
  # Plot daily mean temperature distributions for each time interval indicated above
  ggplot(dailylogger_means, aes(x = MeanTemperature, color = Interval)) +
    geom_density(size = 1, alpha = 1) +
    scale_fill_viridis_d() +
    labs(
      title = "2024 Daily Mean Temperature Distributions",
      x = "Mean Temperature (°C)",
      y = "Density",
      fill = "Date Interval"
    ) +
    theme_classic(base_size = 18)
  
  # Load in historic met data frame
  kinghistmetdata <- read.csv("FormattedHistoricMetDataFieldSln.csv")
  
 