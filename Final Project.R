install.packages("nycflights13")

library(nycflights13) 2
data("flights")

library(ggplot2)
ggplot(flights, aes(x = carrier, y = dep_delay)) + geom_boxplot()

ggplot(flights, aes(x = distance, y = arr_delay)) + geom_point() + geom_smooth(method = "lm")

lm_model <- lm(arr_delay ~ distance, data = flights)
summary(lm_model)

ggplot(flights, aes(x = distance, y = arr_delay)) + 
  geom_point(position = position_jitter(width = 50, height = 5), alpha = 0.5) + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Relationship between Flight Distance and Arrival Delay",
       x = "Distance",
       y = "Arrival Delay (minutes)") +
  theme_minimal()

ggplot(flights, aes(x = distance, y = arr_delay)) + 
  geom_hex(bins = 50) + 
  labs(title = "Density of Flight Distance vs. Arrival Delay",
       x = "Distance",
       y = "Arrival Delay (minutes)") +
  theme_minimal()

install.packages("viridis")
library(viridis)

library(ggplot2)
library(dplyr)
library(viridis)

# Reorder carriers based on median delay
flights <- flights %>%
  group_by(carrier) %>%
  mutate(median_delay = median(dep_delay, na.rm = TRUE)) %>%
  ungroup()

# Boxplot of departure delay by carrier
ggplot(flights, aes(x = reorder(carrier, -median_delay), y = dep_delay, fill = carrier)) +
  geom_boxplot(outlier.alpha = 0.2) +  # Reducing the visibility of outliers for a cleaner look
  labs(title = "Distribution of Departure Delays by Airline Carrier",
       x = "Carrier",
       y = "Departure Delay (minutes)") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position="none")  # Hide legend since colors are self-explanatory

library(ggplot2)

# Calculating average delay by destination
avg_delay_by_dest <- flights %>%
  group_by(dest) %>%
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE))

# Plot
ggplot(avg_delay_by_dest, aes(x = reorder(dest, avg_delay), y = avg_delay)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Airports with the Highest Delays",
       x = "Airport",
       y = "Average Delay (in minutes)")


ggplot(flights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Departure Delays",
       x = "Delay (in minutes)",
       y = "Number of Flights") +
  xlim(-60, 300)  # This limits the x-axis to show only delays from -60 to 300 minutes


library(ggplot2)

# Limit to top 20 airports with highest average delay
top_airports <- avg_delay_by_dest %>%
  arrange(-avg_delay) %>%
  head(20)

# Plot with improved features
ggplot(top_airports, aes(x = reorder(dest, avg_delay), y = avg_delay, fill = avg_delay)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "lightgray", high = "red") +
  labs(title = "Top 20 Airports with the Highest Average Delays",
       x = "Airport",
       y = "Average Delay (in minutes)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray90"))

install.packages("rmarkdown")