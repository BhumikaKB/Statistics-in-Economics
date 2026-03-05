#req libraries
library(dplyr)
library(ineq)
library(ggplot2)
library(tidyr)
library(readxl)
library(ineq)
library(rlang)
library(vctrs)
library(cli)


#----------GDP----------

# Read CSV
gdp_data <- read.csv("Stats_Economic_Analysis_India-GDP.csv",
                     stringsAsFactors = FALSE) #ensures that text (like Yr) stays as character, not a factor

# Convert GDP column to numeric 
gdp_data$GDP_Constant_Price <- as.numeric(gsub(",", "", gdp_data$GDP_Constant_Price))

# Mean GDP
mean_gdp <- mean(gdp_data$GDP_Constant_Price)  #gives avg gdp
print(paste("Mean GDP:", mean_gdp))

# Median GDP
median_gdp <- median(gdp_data$GDP_Constant_Price)
print(paste("Median GDP:", median_gdp)) 

# SD
sd_gdp <- sd(gdp_data$GDP_Constant_Price)
print(paste("SD GDP:", sd_gdp))

# Percentage Growth Rate
gdp_data$GDP_Growth_Rate <- c(
  NA,
  diff(gdp_data$GDP_Constant_Price) /   #diff() → diff btwn consecutive years
    head(gdp_data$GDP_Constant_Price, -1) * 100
)
print(gdp_data[, c("Year", "GDP_Growth_Rate")])

# Simple Linear Regression
gdp_data$Year_Num <- 1:nrow(gdp_data)
gdp_model <- lm(GDP_Constant_Price ~ Year_Num, data = gdp_data)
summary(gdp_model)

# Plot GDP trend
ggplot(gdp_data, aes(x = Year_Num, y = GDP_Constant_Price)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "lightgreen") +
  scale_x_continuous(breaks = gdp_data$Year_Num, labels = gdp_data$Year) +
  labs(title = "GDP Trend at Constant Prices",
       x = "Year",
       y = "GDP (in million ₹)") +
  theme_minimal()


#----------INCOME DISPARITY----------
#reading data from csv 
decile_data <- read.csv("Stats_Economic_Analysis_India-Income_Disparity.csv")


# Select only needed columns
library(readr)
decile_cols <- paste0("decile", 1:10)
decile_data[decile_cols] <- lapply(decile_data[decile_cols], as.numeric)

decile_data <- decile_data %>%
  filter(!is.na(reporting_year))

decile_data[decile_cols] <- lapply(decile_data[decile_cols], function(x) {
  as.numeric(gsub(",", "", trimws(x)))
})

#Mean
#---shows overall income level, used to compare growth across yrs
decile_data$Mean_R <- rowMeans(decile_data[, decile_cols])

#Median Income
#---middle populn income, not affected by xtreme vals,  better indicator of living stds
decile_data$Median_R <- apply(decile_data[, decile_cols], 1, median)

#Gini Coeff
#---measures income inequality, val 0-1
decile_data$Gini_R <- apply(decile_data[, decile_cols], 1, function(x) {
  ineq(x, type = "Gini")
})

#Skewness
#shape of income distribun, 
decile_data$Skewness_R <- decile_data$Mean_R - decile_data$Median_R

#Decile ratio
#---shows gap btwn rich 10% n poor 10%
decile_data$decile_ratio_r <- decile_data$decile10 / decile_data$decile1
head(decile_data$decile_ratio_r)


plot_data <- decile_data %>%
  select(reporting_year, Mean_R, Median_R) %>%
  pivot_longer(cols = c(Mean_R, Median_R),
               names_to = "Measure",
               values_to = "Value")

# mean + median plot
ggplot(plot_data, aes(x = reporting_year, y = Value, color = Measure)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean vs Median Income Over Time",
       x = "Year",
       y = "Income")

#gini trend (Income inequality trend)
ggplot(decile_data, aes(x = reporting_year, y = Gini_R)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Income Inequality (Gini Coefficient)",
       x = "Year",
       y = "Gini Coefficient")

#skewness trend (Income concentration)
ggplot(decile_data, aes(x = reporting_year, y = Skewness_R)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Income Skewness",
       x = "Year",
       y = "Skewness (Mean − Median)")

# rich poor gap (Decile ratio)
ggplot(decile_data, aes(x = reporting_year, y = decile_ratio_r)) +
  geom_line() +
  geom_point() +
  labs(title = "Top 10% to Bottom 10% Income Ratio",
       x = "Year",
       y = "Decile Ratio (D10 / D1)")

#outputs
decile_data$Mean_R
decile_data$Median_R
decile_data$Gini_R
decile_data$Skewness_R
decile_data$decile_ratio_r

summary_table <- decile_data %>%
  select(reporting_year, Mean_R, Median_R, Gini_R, Skewness_R, decile_ratio_r)

print(summary_table)


#---------- ECONOMIC GROWTH----------
# Read CSV
india_gdp_growth <- read.csv("india_gdp_growth.csv")

# View data
head(india_gdp_growth)

india_gdp_growth <- india_gdp_growth[grepl("^[0-9]{4}$", india_gdp_growth$Year), ]
india_gdp_growth$GDP_Growth <- as.numeric(india_gdp_growth$GDP_Growth)

#mean (shows avg income growth rate)
mean_growth <- mean(india_gdp_growth$GDP_Growth)
mean_growth

#median
median_growth <- median(india_gdp_growth$GDP_Growth)
median_growth

#SD (Growth volatility)
sd_growth <- sd(india_gdp_growth$GDP_Growth)
sd_growth

india_gdp_growth$Year <- as.numeric(india_gdp_growth$Year)

#decade wise growth analysis
india_gdp_growth <- india_gdp_growth %>%
  mutate(Decade = paste0(floor(Year/10)*10, "s"))

decade_growth <- india_gdp_growth %>%
  group_by(Decade) %>%
  summarise(Average_Growth = mean(GDP_Growth))

decade_growth

#growth trend graph
library(ggplot2)

ggplot(india_gdp_growth, aes(x = as.numeric(Year), y = GDP_Growth)) +
  geom_line() +
  geom_point() +
  labs(
    title = "India: GDP Growth Rate Trend (1961–2024)",
    x = "Year",
    y = "GDP Growth (%)"
  )

#Regression (trend analysis)
india_gdp_growth$Year_Num <- 1:nrow(india_gdp_growth)

growth_model <- lm(GDP_Growth ~ Year_Num, data = india_gdp_growth)
summary(growth_model)


#----------POVERTY----------
# Poverty file
poverty_raw <- read.csv("pip_new.csv")
colnames(poverty_raw)

#for poverty data
india_poverty <- poverty_raw %>%
  filter(country_name == "India") %>%
  select(reporting_year, headcount)

colnames(india_poverty) <- c("Year", "Poverty")

india_poverty$Year <- as.numeric(india_poverty$Year)
india_poverty$Poverty <- as.numeric(india_poverty$Poverty)


india_poverty <- na.omit(india_poverty)
india_poverty

#descriptive statistics
# Mode function
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Poverty stats
mean_pov   <- mean(india_poverty$Poverty)
median_pov <- median(india_poverty$Poverty)
mode_pov   <- get_mode(india_poverty$Poverty)
sd_pov     <- sd(india_poverty$Poverty)

mean_pov; median_pov; mode_pov; sd_pov

##lorenz curve and gini coefficient for poverty data
income_values <- india_poverty$Poverty

# Lorenz Curve
plot(Lc(income_values),
     main = "Lorenz Curve for India Poverty Data",
     col = "red")

# Gini Coefficient
gini_income <- Gini(income_values)
gini_income

#GDP vs POVERTY
# Extract GDP from same dataset
india_gdp <- poverty_raw %>%
  filter(country_name == "India") %>%
  select(reporting_year, reporting_gdp)

colnames(india_gdp) <- c("Year", "GDP")

india_gdp$Year <- as.numeric(india_gdp$Year)
india_gdp$GDP <- as.numeric(india_gdp$GDP)

# Merge ONLY for graph
gdp_poverty <- inner_join(india_poverty, india_gdp, by = "Year")

# Plot comparison graph
ggplot(gdp_poverty, aes(x = Year)) +
  geom_line(aes(y = GDP, color = "GDP"), linewidth = 1) +
  geom_line(aes(y = Poverty * max(GDP), color = "Poverty"), linewidth = 1) +
  scale_y_continuous(
    name = "GDP",
    sec.axis = sec_axis(~ . / max(gdp_poverty$GDP), name = "Poverty Ratio")
  ) +
  labs(title = "GDP vs Poverty Trend in India",
       x = "Year",
       color = "Indicator") +
  theme_minimal()



#correlation
# Correlation between GDP and Poverty
cor_gdp_poverty <- cor(gdp_poverty$GDP, gdp_poverty$Poverty, use = "complete.obs")
cor_gdp_poverty


# Linear regression model
poverty_model <- lm(Poverty ~ GDP, data = gdp_poverty)

# Summary of model
summary(poverty_model)


