library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("E:/Study/STA32/Project/cause_of_deaths.csv")

# hiv deaths summary
hivSummary <- data %>%
  group_by(Year) %>%
  summarise(HIV_AIDS = sum(HIV.AIDS, na.rm = TRUE))

#highest point for hyphothesis for death drops
max_point <- hivSummary[which.max(hivSummary$HIV_AIDS),]


p <- ggplot(hivSummary, aes(x = Year, y = HIV_AIDS)) +
  geom_line(color = "steelblue") +
  geom_text(data = max_point, aes(label = HIV_AIDS, vjust = -1), size = 5, color = "red") +  # Label highest point
  scale_x_continuous(breaks = seq(min(hivSummary$Year), max(hivSummary$Year), by = 1)) +  # More years on x-axis
  labs(title = "HIV Deaths Over Years",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # x-axis text size
        axis.text.y = element_text(size = 14),  # y-axis text size
        axis.title = element_text(size = 16),  # axis title size
        plot.title = element_text(size = 20, hjust = 0.5),  #title size
        legend.title = element_text(size = 14),  #legend title size
        legend.text = element_text(size = 12))  # legend text size
windows()

print(p)





global_2019_data <- subset(data, Year == 2019)

global_deaths <- global_2019_data %>%
  select(4:ncol(global_2019_data)) %>% # cause col from 4 to end
  summarise_all(sum) # sum every col

#convert data to long
long <- pivot_longer(global_deaths, cols = everything(), names_to = "Cause", values_to = "Deaths")


windows()

ggplot(long, aes(x = Cause, y = Deaths)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = Deaths), position = position_nudge(y = 5), angle = 0, color = "black", size = 3.5, vjust = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16), ) + 
  labs(x = "Cause of Death", y = "Number of Deaths", title = "Global Number of Deaths by Cause, 2019")



usa_2019_data <- subset(data, Year == 2019 & Country.Territory == "United States")


deaths <- usa_2019_data[, 4:ncol(usa_2019_data)] #cause start from 4 to end


long_data <- pivot_longer(deaths, cols = everything(), names_to = "Cause", values_to = "Deaths")#convert to long


p <- ggplot(long_data, aes(x = Cause, y = Deaths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Deaths), position = position_nudge(y = 5), angle = 0, color = "black", size = 3.5, vjust = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),)+
  labs(x = "Cause of Death", y = "Number of Deaths", title = "Number of Deaths by Cause in the United States, 2019")

windows()
print(p)
