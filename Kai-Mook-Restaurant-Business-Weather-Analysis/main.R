
# Install and import libraries
library(tidyverse)
library(lubridate)
library(GGally)
library(zoo)
library(rpart)
library(rpart.plot)
library(Metrics)

#-------------------------------------------------------------------------------
# Import data
weather_data <- read.csv("weather_data.csv", sep = ",")
View(weather_data)

sales_data <- read.csv("sales_data.csv", sep = ",")
View(sales_data)

#-------------------------------------------------------------------------------
# Explore the data
str(weather_data)
dim(weather_data)
colnames(weather_data)

str(sales_data)
dim(sales_data)
colnames(sales_data)

#-------------------------------------------------------------------------------
# Find NAs and 0s
colSums(is.na(weather_data))
colSums(weather_data == 0)

colSums(is.na(sales_data))
colSums(sales_data == 0)

#------------------------------ Formatting Data --------------------------------
# Remove column
sales_data <- subset(sales_data, select = -c(X))

# Rename columns
colnames(weather_data) <- c("date", "min_temp", "max_temp", "rainfall", "evap", 
                            "sun_sh_hr", "dir_max_wind", "spd_max_wind", "tm_max_wind", 
                            "9am_temp", "9am_hum", "9am_cld_amnt", "9am_wind_dir", "9am_wind_spd", "9am_pres",
                            "3pm_temp", "3pm_hum", "3pm_cld_amnt", "3pm_wind_dir", "3pm_wind_spd", "3pm_pres",
                            "rain_today", "rain_tmrw")

colnames(sales_data) <- c("date", "day", "ubereats", "menulog", "deliveroo", "card", "cash", "total", "petty_cash")

# Format "Date" columns
weather_data$date <- as.Date(weather_data$date, format = "%d/%m/%Y")
weather_data <- weather_data[order(as.Date(weather_data$date, format = "%d/%m/%Y")), ]

sales_data$date <- as.Date(sales_data$date, format = "%d/%m/%Y")

# Create "month", "year", and "year_month" variable
weather_data$month <- months(as.POSIXlt(weather_data$date, format = "%Y-%m-%d"))
weather_data$year <- year(as.POSIXlt(weather_data$date, format = "%Y-%m-%d"))
weather_data$yr_month <- as.yearmon(weather_data$date)

sales_data$month <- months(as.POSIXlt(sales_data$date, format = "%Y-%m-%d"))
sales_data$year <- year(as.POSIXlt(sales_data$date, format = "%Y-%m-%d"))
sales_data$yr_month <- as.yearmon(sales_data$date)

# Remove rows from "weather_data" so number of rows is equivalent 
weather_data <- subset(weather_data, date > as.Date("2021-09-26"))
weather_data <- subset(weather_data, date < as.Date("2022-08-19"))

# Change NAs to 0
sales_data[is.na(sales_data)] <- 0

# Impute missing values
weather_data <- as.data.frame(weather_data %>% group_by(yr_month) %>%
                                mutate(max_temp = ifelse(is.na(max_temp),
                                                         mean(max_temp, na.rm = TRUE), max_temp)))

weather_data <- as.data.frame(weather_data %>% group_by(yr_month) %>%
                                mutate(rainfall = ifelse(is.na(rainfall),
                                                         mean(rainfall, na.rm = TRUE), rainfall)))

weather_data <- as.data.frame(weather_data %>% group_by(yr_month) %>%
                                mutate(sun_sh_hr = ifelse(is.na(sun_sh_hr),
                                                         mean(rainfall, na.rm = TRUE), sun_sh_hr)))

weather_data <- as.data.frame(weather_data %>% group_by(yr_month) %>%
                                mutate(spd_max_wind = ifelse(is.na(spd_max_wind),
                                                         mean(spd_max_wind, na.rm = TRUE), spd_max_wind)))

weather_data <- as.data.frame(weather_data %>% group_by(yr_month) %>%
                                mutate(`9am_hum` = ifelse(is.na(`9am_hum`),
                                                         mean(`9am_hum`, na.rm = TRUE), `9am_hum`)))

weather_data <- as.data.frame(weather_data %>% group_by(yr_month) %>%
                                mutate(`9am_cld_amnt` = ifelse(is.na(`9am_cld_amnt`),
                                                          mean(`9am_cld_amnt`, na.rm = TRUE), `9am_cld_amnt`)))
#-------------------------------------------------------------------------------
# Create master data frame
master_df <- cbind(weather_data, sales_data)
View(master_df)

master_df <- subset(master_df, select = -c(24, 25, 26, 27, 28, 35))

# Convert "rain_today" and "rain_tmrw" to binary
master_df$rain_today <- ifelse(master_df$rain_today == "Yes", 1, 0)
master_df$rain_tmrw <- ifelse(master_df$rain_tmrw == "Yes", 1, 0)

# Remove rows where "total" = 0
colSums(master_df == 0)
master_df <- master_df[!master_df$total == 0, ]

master_df$month <- as.integer(factor(master_df$month, levels = month.name))
master_df$wk_num <- strftime(master_df$date, format = "%V")

#-------------------------------------------------------------------------------
# Correlation
corr_weather <- subset(weather_data, select = -c(1, 7, 9, 13, 14, 19, 20, 22:26))
View(corr_weather)
ggcorr(corr_weather, label = TRUE)

corr_sales <- subset(sales_data, select = -c(1, 2, 10, 11, 12))
View(corr_sales)
ggcorr(corr_sales, label = TRUE)

corr <- cbind(corr_weather, corr_sales)
View(corr)
ggcorr(corr, label = TRUE)

corr_master_df <- subset(master_df, select = -c(1:3, 5:21, 32))
View(corr_master_df)
ggcorr(corr_master_df, label = TRUE)

# Delivery Sales Gathered
master_df2 <- master_df %>%
  gather("service_type", "delivery_sales", 24:26)
View(master_df2)

#-------------------------------------------------------------------------------
# Delivery sales subset
delivery_sales <- subset(sales_data, select = -c(1, 2, 6, 7, 8, 9, 10, 11))
View(delivery_sales)

delivery_sales <- delivery_sales %>%
  gather("service_type", "sales", -yr_month, 1:3)

delivery_sales <- delivery_sales %>% 
  group_by(yr_month, service_type) %>%
  summarise(
    mean = mean(sales, na.rm = TRUE)
  )

#-------------------------------------------------------------------------------
# Average weather
avg_weather_subset <- weather_data %>%
  group_by(yr_month) %>%
  summarise(
    min_temp = mean(min_temp, na.rm = TRUE),
    max_temp = mean(max_temp, na.rm = TRUE),
    rainfall = mean(rainfall, na.rm = TRUE),
    evap = mean(evap, na.rm = TRUE),
    sun_sh_hr = mean(sun_sh_hr, na.rm = TRUE),
    spd_max_wind = mean(spd_max_wind, na.rm = TRUE),
    `9am_temp` = mean(`9am_temp`, na.rm = TRUE),
    `9am_hum` = mean(`9am_hum`, na.rm = TRUE),
    `9am_cld_amnt` = mean(`9am_cld_amnt`, na.rm = TRUE),
    `9am_pres` = mean(`9am_pres`, na.rm = TRUE),
    `3pm_temp` = mean(`3pm_temp`, na.rm = TRUE),
    `3pm_hum` = mean(`3pm_hum`, na.rm = TRUE),
    `3pm_cld_amnt` = mean(`3pm_cld_amnt`, na.rm = TRUE),
    `3pm_pres` = mean(`3pm_pres`, na.rm = TRUE),
    year = mean(year, na.rm = TRUE)
  )
View(avg_weather_subset)

# Average sales
avg_sales_subset <- sales_data %>%
  group_by(yr_month) %>%
  summarise(
    ubereats = mean(ubereats, na.rm = TRUE),
    menulog = mean(menulog, na.rm = TRUE),
    deliveroo = mean(deliveroo, na.rm = TRUE),
    card = mean(card, na.rm = TRUE),
    cash = mean(cash, na.rm = TRUE),
    total = mean(total, na.rm = TRUE),
    petty_cash = mean(petty_cash, na.rm = TRUE),
    year = mean(year, na.rm = TRUE)
  )
View(avg_sales_subset)


avg_mnth_master_df <- master_df %>%
  group_by(yr_month) %>%
  summarise(
    min_temp = mean(min_temp, na.rm = FALSE),
    max_temp = mean(max_temp, na.rm = FALSE),
    rainfall = mean(rainfall, na.rm = FALSE),
    ubereats = mean(ubereats, na.rm = FALSE),
    menulog = mean(menulog, na.rm = FALSE),
    deliveroo = mean(deliveroo, na.rm = FALSE),
    card = mean(card, na.rm = FALSE),
    cash = mean(cash, na.rm = FALSE),
    total = mean(total, na.rm = FALSE)
  )
View(avg_mnth_master_df)

# Correlation Matrix by Monthly Averages
corr_avg_mnth_master_df <- subset(avg_mnth_master_df, select = -c(1))
View(corr_avg_mnth_master_df)
ggcorr(corr_avg_mnth_master_df, label = TRUE)


avg_wk_master_df <- master_df %>%
  group_by(wk_num) %>%
  summarise(
    min_temp = mean(min_temp, na.rm = FALSE),
    max_temp = mean(max_temp, na.rm = FALSE),
    rainfall = mean(rainfall, na.rm = FALSE),
    ubereats = mean(ubereats, na.rm = FALSE),
    menulog = mean(menulog, na.rm = FALSE),
    deliveroo = mean(deliveroo, na.rm = FALSE),
    card = mean(card, na.rm = FALSE),
    cash = mean(cash, na.rm = FALSE),
    total = mean(total, na.rm = FALSE)
  )

#
corr_avg_wk_master_df <- subset(avg_wk_master_df, select = -c(1))
View(corr_avg_wk_master_df)
ggcorr(corr_avg_wk_master_df, label = TRUE)

#-------------------------------------------------------------------------------
#
mean_rainfall_by_yrmonth <- weather_data %>% 
  group_by(yr_month) %>%
  summarise(
    mean_rainfall = mean(rainfall, na.rm = TRUE)
  )
View(mean_rainfall_by_yrmonth)

#
mean_sales_by_yrmonth <- sales_data %>%
  group_by(yr_month) %>%
  summarise(
    mean_sales = mean(total, na.rm = TRUE)
  )
View(mean_sales_by_yrmonth)

# 
mean_sales_rainfall <- cbind(mean_sales_by_yrmonth, mean_rainfall_by_yrmonth)
mean_sales_rainfall <- subset(mean_sales_rainfall, select = -c(3))
mean_sales_rainfall$mean_rainfall <- mean_sales_rainfall$mean_rainfall * 1000
View(mean_sales_rainfall)

# Gather "mean_sales" and "mean_rainfall"
mean_sales_rainfall <- mean_sales_rainfall %>%
  gather("means", "sales_rainfall", -yr_month)

#----------------------------------- Plots -------------------------------------
# Histogram of Rainfall
ggplot(weather_data, aes(x = rainfall)) +
  geom_histogram() +
  xlab("Rainfall (mm)") +
  ylab("Counts") +
  ggtitle("Distribution of Rainfall (mm)")

# Box plot of Rainfall
ggplot(weather_data, aes(x = rainfall)) +
  geom_boxplot() +
  xlab("Rainfall (mm)") +
  ylab("Counts") +
  ggtitle("Distribution of Rainfall (mm)")

# Histogram of Income
ggplot(sales_data, aes(x = total)) +
  geom_histogram() +
  xlab("Sales ($)") +
  ylab("Counts") +
  ggtitle("Distribution of Sales ($)")

# Box plot of Sales
ggplot(sales_data, aes(x = Total)) +
  geom_boxplot() +
  xlab("Sales ($)") +
  ylab("Counts") +
  ggtitle("Distribution of Sales ($)")

#
ggplot(weather_data, aes(x = month, y = rainfall)) +
  geom_point()

#
ggplot(sales_data, aes(x = month, y = total)) +
  geom_point()

# Time Series of Average Monthly Rainfall
ggplot(mean_rainfall_by_yrmonth, aes(x = yr_month, y = mean_rainfall)) +
  geom_line() +
  xlab("Time") +
  ylab("Mean rainfall (mm)") +
  ggtitle("Mean Rainfall (mm) of the Last 14 Months")

# Time Series of Average Monthly Sales
ggplot(mean_sales_by_yrmonth, aes(x = yr_month, y = mean_sales)) +
  geom_line() +
  xlab("Time") +
  ylab("Mean Sales ($)") +
  ggtitle("Mean Sales ($) of the Last 14 Months")

# Time Series of Sales and Rainfall
ggplot(mean_sales_rainfall, aes(x = yr_month, y = sales_rainfall)) +
  geom_line(aes(color = means)) +
  xlab("Time") +
  ylab("Sales ($) and Rainfall (μm)") +
  ggtitle("Time Series of Average Monthly Sales and Rainfall")

# Time Series of Delivery Service Sales
ggplot(delivery_sales, aes(x = yr_month, y = mean)) +
  geom_line(aes(color = service_type)) +
  xlab("Time") +
  ylab("Mean Sales") +
  ggtitle("Average Delivery Sales Over the Last 12 Months")

# Scatter Plot of Total Sales Against Rainfall 
ggplot(corr_avg_master_df, aes(x = rainfall, y = total)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Rainfall (mm)") +
  ylab("Total Sales ($)") +
  ggtitle("Scatter Plot of Total Sales Against Rainfall")

# Scatter Plot of Ubereats Sales Against Rainfall
ggplot(corr_avg_master_df, aes(x = rainfall, y = ubereats)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Rainfall (mm)") +
  ylab("UberEats Sales ($)") +
  ggtitle("Scatter Plot of UberEats Sales Against Rainfall")

# Scatter Plot of Menulog Sales Against Rainfall
ggplot(corr_avg_master_df, aes(x = rainfall, y = menulog)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Rainfall (mm)") +
  ylab("Menulog Sales ($)") +
  ggtitle("Scatter Plot of Menulog Sales Against Rainfall")

# Scatter Plot of Deliveroo Sales Against Rainfall
ggplot(corr_avg_master_df, aes(x = rainfall, y = deliveroo)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Rainfall (mm)") +
  ylab("Deliveroo Sales ($)") +
  ggtitle("Scatter Plot of Deliveroo Sales Against Rainfall")

# Scatter Plot of Cash Payments Against Rainfall
ggplot(corr_avg_master_df, aes(x = rainfall, y = cash)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Rainfall (mm)") +
  ylab("Cash Payments ($)") +
  ggtitle("Scatter Plot of Cash Payments Against Rainfall")

# Scatter Plot of Card Payments Against Rainfall
ggplot(corr_avg_master_df, aes(x = rainfall, y = card)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Rainfall (mm)") +
  ylab("Card Payments ($)") +
  ggtitle("Scatter Plot of Card Payments Against Rainfall")

# 
ggplot(master_df, aes(x = rainfall, y = total)) +
  geom_point() +
  geom_smooth(method = "lm")

#-------------------------------------------------------------------------------
#Split train and test data from weather_data
df_size <- floor(0.75 * nrow(corr_weather))

set.seed(123)
train_ind <- sample(seq_len(nrow(corr_weather)), size = df_size)

train <- corr_weather[train_ind, ]
test <- corr_weather[-train_ind, ]

#-------------------------------------------------------------------------------
# Linear Model
model_linear <- lm(rainfall ~ min_temp + max_temp + `9am_pres` + `3pm_pres`, data = train)

anova(model_linear)

plot(model_linear)

pred = predict(model_linear, test, method = "anova")
rmse_lm = rmse(pred, test$rainfall)
print(rmse_lm)

# Decision Tree Model
model_decision_tree <- rpart(rainfall ~ min_temp + max_temp + `9am_pres` + `3pm_pres`, 
                method = "anova", data = train, control =rpart.control(minsplit =1,minbucket=1, cp=0))

pred2 = predict(model_decision_tree, test, method = "anova")

err2 = sum(pred2 - test$rainfall)

rmse_tree = rmse(pred2, test$rainfall)

print(rmse_tree)

# Save decision tree and .png file
png(file = "decTreeGFG.png", width = 600, 
    height = 600)

# Plot the decision tree model
plot(model_decision_tree, uniform = TRUE,
     main = "Decision Tree")
text(model_decision_tree, use.n = TRUE, cex = .7)

#-------------------------------------------------------------------------------
# SVM
model_svm <- svm(rainfall ~ ., weather_data)

#-------------------------------------------------------------------------------
# Saving the file
dev.off()

# Print model
print(fit)






