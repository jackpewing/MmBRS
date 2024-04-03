mmdata <- subset(mmdata, MmEffort == 1)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Aggregate data by hour to get the sum of mmpres_eff_adj for each hour
mmdata_sum <- mmdata %>%
  group_by(hour) %>%
  summarise(sum_mmpres= sum(MmPres, na.rm = TRUE))

# Plot the aggregated data
ggplot(mmdata_sum, aes(x = hour, y = sum_mmpres)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Hour", y = "Total Minutes of Narwhal Presence", title = "Total Minutes of Narwhal Presence by Hour - October") +
  theme_minimal()

# Aggregate data by hour to get the sum of mmpres_eff_adj for each hour
sdata_sum <- mmdata %>%
  group_by(hour) %>%
  summarise(sum_spres= sum(sPres, na.rm = TRUE))

# Plot the aggregated data
ggplot(sdata_sum, aes(x = hour, y = sum_spres)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Hour", y = "Total Minutes of Ship Presence", title = "Total Minutes of ship Presence by Hour - October") +
  theme_minimal()

