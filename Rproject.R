library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(kableExtra)
# Set the working directory where your data is located
setwd("C:/Users/asus/Desktop/archive")

# Read the dataset
ev_sales <- read_csv("IEA-EV-dataEV salesCarsHistorical.csv")

# Check the structure of the dataset
str(ev_sales)

# View the first few rows of the dataset
head(ev_sales)

# Handling missing values 
data <- na.omit(ev_sales)  # Remove rows with NA values



# Filter the data for visualization
ggplot(data, aes(x = value, y = region, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Value", y = "Region", title = "Historic Sales of Electric Vehicles") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))

without_world <- subset(data, region != 'World')
without_world <- without_world[order(-without_world$value), ]



ggplot(without_world, aes(x = value, y = region)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(x = "Value", y = "Region", title = "Historic Sales of Electric Vehicles") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))





# Créer un barplot en utilisant ggplot avec l'ordre croissant des régions par rapport à 'value'
p <- ggplot(without_world, aes(x = value, y = reorder(region, value))) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(x = "Value", y = "Region", title = "Historic Sales of Electric Vehicles") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))


# Modifying x-axis labels to display numbers in regular format
p + scale_x_continuous(labels = scales::number_format(scale = 1e-6))


# Créer un barplot en utilisant ggplot avec l'ordre croissant des régions par rapport à 'value'
p <- ggplot(without_world, aes(x = value, y = reorder(region, value))) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(x = "Value", y = "Region", title = "Historic Sales of Electric Vehicles") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))

# Modifying x-axis labels to display numbers without scientific notation
p + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

# global demand for electric vehicles over yearss

ggplot(without_world, aes(x = year, y = value)) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "Year", y = "Value", title = "Trend in Global Demand for Electric Vehicles") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))




powertrain_counts <- table(data$powertrain)

# Convert the counts to a dataframe
powertrain_df <- data.frame(powertrain = names(powertrain_counts),
                            counts = as.numeric(powertrain_counts))

# Create a pie chart using ggplot
ggplot(powertrain_df, aes(x = "", y = counts, fill = powertrain)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Powertrain") +
  ggtitle("Distribution of Powertrain Types") +
  theme_void() +
  theme(legend.position = "bottom")



ggplot(without_world, aes(x = year, y = value, color = powertrain)) +
  geom_point() +
  labs(x = "Year", y = "Value", title = "Scatter plot of Electric Vehicle Sales by Powertrain") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))




# Filter the dataframe for relevant columns and group by 'year' and 'powertrain'
grouped_data <- data %>%
  filter(powertrain %in% c('BEV', 'PHEV')) %>%
  group_by(year, powertrain) %>%
  summarise(total_value = sum(value)) %>%
  arrange(desc(year))  # Sort by year in descending order

# Plotting a bar plot to compare aggregated values of BEV and PHEV over the years
ggplot(grouped_data, aes(x = factor(year), y = total_value/1e6, fill = powertrain)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Year", y = "Total Value (Millions)", title = "Comparison of BEV and PHEV Total Values Over Years") +
  scale_fill_manual(values = c("BEV" = "blue", "PHEV" = "green")) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, max(grouped_data$total_value/1e6) + 5, by = 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10), 
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top")



# Filter the dataframe for relevant columns and group by 'year' and 'powertrain'
grouped_data <- data %>%
  filter(powertrain %in% c('BEV', 'PHEV')) %>%
  group_by(year, powertrain) %>%
  summarise(total_value = sum(value)) %>%
  arrange(desc(year))  # Sort by year in descending order

# Plotting a bar plot to compare aggregated values of BEV and PHEV over the years
ggplot(grouped_data, aes(x = factor(year), y = total_value/1e6, fill = powertrain)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Year", y = "Total Value (Millions)", title = "Comparison of BEV and PHEV Total Values Over Years") +
  scale_fill_manual(values = c("BEV" = "blue", "PHEV" = "green")) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, max(grouped_data$total_value/1e6) + 5, by = 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10), 
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top")









# Grouping by 'year' and 'powertrain', calculating the sum of 'value'
grouped_data <- data %>%
  group_by(year, powertrain) %>%
  summarise(total_value = sum(value)) %>%
  arrange(desc(year))  # Sort by year in descending order

# Displaying the aggregated data as a table
grouped_table <- kable(grouped_data, "html") %>%
  kable_styling(full_width = FALSE)

grouped_table  # Print the formatted table




# Filter the data for BEV (Battery Electric Vehicles)
bev_data <- without_world %>%
  filter(powertrain == 'BEV')

# Filter the data for PHEV (Plug-in Hybrid Electric Vehicles)
phev_data <- without_world %>%
  filter(powertrain == 'PHEV')

# Linear regression to predict sales for BEV in 2024
lm_model_bev <- lm(value ~ year, data = bev_data)
predicted_sales_bev <- predict(lm_model_bev, newdata = data.frame(year = 2024))

# Linear regression to predict sales for PHEV in 2024
lm_model_phev <- lm(value ~ year, data = phev_data)
predicted_sales_phev <- predict(lm_model_phev, newdata = data.frame(year = 2024))

# Create a dataframe for the predictions
predictions_df <- data.frame(Powertrain = c('BEV', 'PHEV'),
                             Predicted_Sales = c(predicted_sales_bev, predicted_sales_phev))

# Plotting the predicted sales for BEV and PHEV in 2024
ggplot(predictions_df, aes(x = Powertrain, y = Predicted_Sales, fill = Powertrain)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  labs(x = 'Powertrain', y = 'Predicted Sales', title = 'Predicted Sales for BEV and PHEV in 2024') +
  theme_minimal()

# Get unique countries in the dataset
unique_countries <- unique(without_world$region)

# Initialize an empty dataframe to store the predictions
all_predictions <- data.frame()

# Loop through each country
for (country in unique_countries) {
  # Filter data for the current country and BEV
  bev_data <- without_world %>%
    filter(powertrain == 'BEV' & region == country)
  
  # Filter data for the current country and PHEV
  phev_data <- without_world %>%
    filter(powertrain == 'PHEV' & region == country)
  
  # Linear regression for BEV in the current country
  lm_model_bev <- lm(value ~ year, data = bev_data)
  predicted_sales_bev <- predict(lm_model_bev, newdata = data.frame(year = 2024))
  
  # Linear regression for PHEV in the current country
  lm_model_phev <- lm(value ~ year, data = phev_data)
  predicted_sales_phev <- predict(lm_model_phev, newdata = data.frame(year = 2024))
  
  # Create a dataframe for the predictions in the current country
  country_predictions <- data.frame(Powertrain = c('BEV', 'PHEV'),
                                    Predicted_Sales = c(predicted_sales_bev, predicted_sales_phev),
                                    Country = country)
  
  # Bind predictions for the current country to the overall dataframe
  all_predictions <- bind_rows(all_predictions, country_predictions)
}

# Plotting the predicted sales for BEV and PHEV in 2024 for each country on the same plot
ggplot(all_predictions, aes(x = Country, y = Predicted_Sales, fill = Powertrain)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  labs(x = 'Country', y = 'Predicted Sales', title = 'Predicted Sales for BEV and PHEV in 2024 by Country') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = 'Powertrain')



