# Install packages if not already installed
# install.packages("SnowballC")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("tm")
# install.packages("proxy")


# Load necessary libraries
library(tm)
library(SnowballC)
library(ggplot2)
library(proxy)
library(readr)


# Load data
youtube <- read.csv("most_subscribed_youtube_channels.csv")

# Sample data
channel_data <- data.frame(
  Channel = c("MrBeast", "T-Series", "Cocomelon"),
  Description = c(
    "MrBeast creates viral videos with challenges and giveaways.",
    "T-Series is the largest Indian music label and movie studio.",
    "Cocomelon offers educational and entertaining children's songs and videos."
  ),
  Tags = c(
    "challenges, giveaways, viral",
    "music, movies, Indian",
    "children, education, songs"
  ),
  stringsAsFactors = FALSE
)

# Create a corpus
corpus <- Corpus(VectorSource(paste(channel_data$Description, channel_data$Tags)))

# Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

# Clean subscribers column
youtube$subscribers <- as.numeric(gsub(",", "", youtube$subscribers))

# Get top 5 by category
top5_education <- subset(youtube, category == "Education")
top5_education <- head(top5_education[order(-top5_education$subscribers), ], 5)

top5_music <- subset(youtube, category == "Music")
top5_music <- head(top5_music[order(-top5_music$subscribers), ], 5)

top5_entertainment <- subset(youtube, category == "Entertainment")
top5_entertainment <- head(top5_entertainment[order(-top5_entertainment$subscribers), ], 5)

# Turn off scientific notation
options(scipen = 999)

# Show results
print("Top 5 Education Channels:")
print(top5_education[, c("Youtuber", "subscribers")])
print("Top 5 Music Channels:")
print(top5_music[, c("Youtuber", "subscribers")])
print("Top 5 Entertainment Channels:")
print(top5_entertainment[, c("Youtuber", "subscribers")])

# Combine the top 5 channels from each category
top15_channels <- rbind(
  top5_education[, c("Youtuber", "subscribers", "category")],
  top5_music[, c("Youtuber", "subscribers", "category")],
  top5_entertainment[, c("Youtuber", "subscribers", "category")]
)
print(top15_channels)

# Turn off scientific notation
options(scipen = 999)

# Create a bar plot of subscribers by channel, colored by category
ggplot(top15_channels, aes(x = reorder(Youtuber, subscribers), y = subscribers, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for horizontal bars
  scale_fill_manual(values = c("Education" = "blue", "Music" = "red", "Entertainment" = "green")) +
  labs(
    title = "Top 5 YouTube Channels in Education, Music, and Entertainment",
    x = "YouTube Channel",
    y = "Number of Subscribers",
    fill = "Category"
  ) +
  theme_minimal() +
  # Format y-axis to show numbers in millions
  scale_y_continuous(labels = function(x) paste0(x/1000000, "M"))


# Create a Document-Term Matrix using TF-IDF
dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))

# Convert to matrix
dtm_matrix <- as.matrix(dtm)

# Calculate cosine similarity (using simil instead of dist for proper cosine similarity)
cosine_sim <- as.matrix(simil(dtm_matrix, method = "cosine"))

# Format the similarity matrix
colnames(cosine_sim) <- channel_data$Channel
rownames(cosine_sim) <- channel_data$Channel

# Convert to a data frame for visualization
similarity_df <- as.data.frame(cosine_sim)


# Create a function to plot similarity for any channel
plot_channel_similarity <- function(channel_name) {
  # Extract similarities for the specified channel
  sim_values <- similarity_df[, channel_name]
  # Create a data frame for plotting
  plot_df <- data.frame(
    Channel = rownames(similarity_df),
    Similarity = sim_values
  )
  # Remove self-comparison
  plot_df <- plot_df[plot_df$Channel != channel_name, ]
  
    
  # Plot
  ggplot(plot_df, aes(x = reorder(Channel, Similarity), y = Similarity)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Cosine Similarity with", channel_name), 
         x = "Channel", 
         y = "Cosine Similarity") +
    theme_minimal()
}

# Plot similarity for MrBeast
mrbeast_plot <- plot_channel_similarity("MrBeast")
print(mrbeast_plot)

# Plot similarity for T-Series
tseries_plot <- plot_channel_similarity("T-Series")
print(tseries_plot)

# Plot similarity for Cocomelon
cocomelon_plot <- plot_channel_similarity("Cocomelon")
print(cocomelon_plot)

# Create a heatmap of all similarities
similarity_long <- reshape2::melt(as.matrix(similarity_df), varnames = c("Channel1", "Channel2"))

heatmap_plot <- ggplot(similarity_long, aes(x = Channel1, y = Channel2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "pink", high = "purple", name = "Similarity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Cosine Similarity Heatmap Between Channels")

print(heatmap_plot)
