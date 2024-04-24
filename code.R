#install.packages("ggplot2")
#install.packages("patchwork")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("knitr")
#install.packages("rmarkdown")
#install.packages("maps")


# Load the ggplot2 library
library(ggplot2)
library(patchwork)
library(readr)
library(dplyr)
library(knitr)
library(rmarkdown)
library(maps)


data <- read_csv("unicef_inicator_2.csv")

data$obs_value <- data$`Estimated number of children (aged 0-17 years) who have lost one or both parents`
data <- data[order(data$obs_value), ]
data$year <- as.numeric(data$`year`)

filtered_data <- filter(data,year >= 2000)
filtered_data <-filter(filtered_data, country != 'Uzbekistan')

population_meta_data <- read_csv("unicef_metadata.csv")

children_population_data <- read_csv("children_population.csv")
children_population_data <- inner_join(children_population_data,population_meta_data,by=c("year","country") )
children_population_data$total_children_population <- children_population_data$`Children population` /100 * children_population_data$`Population, total`


grouped_children_population_data <- children_population_data %>%
  group_by(year) %>%
  summarise(total_children_population = sum(total_children_population))


grouped_data <- filtered_data %>%
  group_by(year) %>%
  summarise(sum_value = sum(obs_value))

joined_data <- inner_join(grouped_data, grouped_children_population_data, by = "year")
joined_data$percentage <- joined_data$sum_value * 100 / joined_data$total_children_population

# Create a bar chart
bar_plot <- ggplot(grouped_data, aes(x = year, y = sum_value)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"), limits = c(0, 100000000)) +
  theme_minimal() +
  labs(x = "Year", y = "Total no. of children losing one or both parents")  # Replace "Population total" with "obs_value"
print(bar_plot)

# Create a line plot with connected data points
line_plot <- ggplot(joined_data, aes(x = year, y = percentage)) +
  geom_point(color="red",size=1) +
  geom_line(color = "red", size = 1) +    # Connect data points with lines
  scale_y_continuous(labels = scales::comma_format(suffix = "%"), limits = c(0, 20)) +
  labs(x = "Year", y = "% of children losing their parents")

combined_plot <- bar_plot + line_plot
# Print the combined plot
print(combined_plot)

grouped_children_population_data <- children_population_data %>%
  group_by(country) %>%
  summarise(total_children_population = sum(total_children_population))


grouped_data <- filtered_data %>%
  group_by(country) %>%
  summarise(sum_value = sum(obs_value))

joined_data <- inner_join(grouped_data, grouped_children_population_data, by = "country")
joined_data$percentage <- joined_data$sum_value * 100 / joined_data$total_children_population

poor_performing_countries <- filter(joined_data, percentage > 10)
poor_performing_countries <- as.vector(poor_performing_countries$country)

world_map <- map_data("world")

# Merge country data with world map data
merged_data <- merge(world_map, joined_data, by.x = "region", by.y = "country", all.x = TRUE)

# Plot the world map with heatmap
heatmap_plot <- ggplot() +
  geom_polygon(data =  merged_data, aes(x = long, y = lat, group = group, fill = percentage)) +
  #  scale_fill_gradient(low = "blue", high = "lightblue", na.value = "grey") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey") +
  theme_void()

# Show the plot
print(heatmap_plot)

health_exp_per_capita <- read_csv('health_exp_per_capita.csv')

health_exp_per_capita <- health_exp_per_capita %>%
  group_by(country) %>%
  summarise(total_health_gdp = sum(`Health Exp per Capita (USD)`))

joined_data <- inner_join(joined_data,health_exp_per_capita, by='country')                            

# Create the scatter plot using ggplot
ggplot(data = joined_data, aes(x = percentage, y = total_health_gdp )) +
  geom_point(color = ifelse(joined_data$country %in% poor_performing_countries ,"red","blue"), size = 3) +
  labs(title = "Health Expenditure analysis on a few sample countries",
       x = "% of children losing their parents",
       y = "Health expenditure per capita (USD)")

population_meta_data <- population_meta_data %>%
  group_by(country) %>%
  summarise(total_gdp_per_capita = sum(`GDP per capita (constant 2015 US$)`))


joined_data <- inner_join(joined_data,population_meta_data, by='country')                            


# Create the scatter plot using ggplot
ggplot(data = joined_data, aes(x = percentage, y = total_gdp_per_capita )) +
  geom_point(color = ifelse(joined_data$country %in% poor_performing_countries ,"red","blue"), size = 3) +
  labs(title = "Looking into the GDP per capita of poor performing countries",
       x = "% of children losing their parents",
       y = "GDP per capita (USD)")

child_labour_data <- read_csv('children_employment.csv')

child_labour_data <- child_labour_data %>%
  group_by(Country) %>%
  summarise(total_child_labour_rate = sum(`% of children in employment ages 7-14`))


poverty_rate <- read_csv('poverty_rate.csv')

poverty_rate <- poverty_rate %>%
  group_by(Country) %>%
  summarise(total_poverty_rate = sum(`Poverty rate at $3.65 a day (PPP adjusted)`))


poverty_child_labour_data <- inner_join(child_labour_data,poverty_rate, by = 'Country')


# Create the scatter plot using ggplot
ggplot(data = poverty_child_labour_data, aes(x = total_poverty_rate, y = total_child_labour_rate )) +
  geom_point(color = ifelse(poverty_child_labour_data$Country %in% poor_performing_countries ,"red","blue"), size = 3) +
  scale_y_continuous(labels = scales::comma_format(suffix = "%"), limits = c(0, 100)) +
  scale_x_continuous(labels = scales::comma_format(suffix = "%"), limits = c(0, 100)) +
  labs(title = "Quick Analysis on the worst countries that can affect these children",
       x = "Poverty rate in %",
       y = "Child labour rate in %") +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "topright")
#end
