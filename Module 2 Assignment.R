# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)

# Load dataset
td <- read_csv('twitter_dataset.csv')

# View tables for missing or inconsistent data
table(td$Retweets)
table(td$Likes)
table(td$Timestamp)
table(td$Username)
table(td$Text)

# Convert Timestamp to Date format
td$Timestamp <- as.POSIXct(td$Timestamp, format ="%Y-%m-%d %H:%M:%S")
td$Date <- as.Date(td$Timestamp)

# Calculate engagement metrics
td <- td %>% mutate(Total_Engagement = Likes + Retweets)

# View dataset with calculated engagement
head(td)

# Aggregate engagement per date
engagement_trend <- td %>% 
  group_by(Date) %>% 
  summarise(Total_Engagement = sum(Total_Engagement, na.rm =TRUE))

# Plot engagement trend over time
ggplot(engagement_trend, aes(x = Date, y = Total_Engagement)) +
  geom_line(color = "pink", size = 1) +
  geom_point(color = "purple", size = 2) +
  labs(title = "Tweet Engagement Over Time",
       x = "Date",
       y = "Total Engagement")
        
# Calculate total engagement per user and get top 5 influencers
top_influencers <- td %>%
  group_by(Username) %>%
  summarise(Total_User_Engagement = sum(Total_Engagement, na.rm = TRUE)) %>%
  arrange(desc(Total_User_Engagement)) %>%
  head(5)
# View dataset with calculated engagement
head(top_influencers)

# Filter data for top 5 influencers
top_influencer_data <- td %>%
  filter(Username %in% top_influencers$Username)

# Create the visualization
ggplot(top_influencer_data,aes(x = Date, y = Total_Engagement, color = Username)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Top 5 Influencers Engagement Over Time",
       x = "Date",
       y = "Total_Engagement",
       color = "Influencer") +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"))




# First verify total engagement calculation
total_engagement_check <- td %>%
  summarise(
    Total_Likes = sum(Likes, na.rm = TRUE),
    Total_Retweets = sum(Retweets, na.rm = TRUE),
    Total_Combined = sum(Total_Engagement, na.rm = TRUE)
  )

# Recalculate top influencers with verified engagement
top_influencers <- td %>%
  group_by(Username) %>%
  summarise(
    Total_Likes = sum(Likes, na.rm = TRUE),
    Total_Retweets = sum(Retweets, na.rm = TRUE),
    Total_User_Engagement = sum(Total_Engagement, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_User_Engagement)) %>%
  head(5)



