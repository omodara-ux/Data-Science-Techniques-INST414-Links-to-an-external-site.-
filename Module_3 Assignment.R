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
#Convert scientific value off
options(scipen = 999)

# Load data
youtube <- read.csv("most_subscribed_youtube_channels.csv")

# Create a corpus from the category column
corpus <- Corpus(VectorSource(youtube$category))

# Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stemDocument)

# Create a Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Convert to matrix
dtm_matrix <- as.matrix(dtm)

# Function to find similar channels
find_similar_channels <- function(channel_name) {
  # Find channel index
  channel_index <- which(grepl(channel_name, youtube$Youtuber, ignore.case = TRUE))
  
  if(length(channel_index) > 0) {
    cat("\nAnalyzing channels similar to", youtube$Youtuber[channel_index[1]], "\n")
    
    # Calculate similarities manually
    similarities <- numeric(nrow(dtm_matrix))
    channel_vector <- dtm_matrix[channel_index[1], ]
    
    for(i in 1:nrow(dtm_matrix)) {
      # Calculate cosine similarity manually
      dot_product <- sum(dtm_matrix[i, ] * channel_vector)
      norm_a <- sqrt(sum(dtm_matrix[i, ]^2))
      norm_b <- sqrt(sum(channel_vector^2))
      
      if(norm_a > 0 && norm_b > 0) {
        similarities[i] <- dot_product / (norm_a * norm_b)
      } else {
        similarities[i] <- 0
      }
    }
    
    # Create results dataframe
    results <- data.frame(
      Youtuber = youtube$Youtuber,
      Category = youtube$category,
      Similarity = similarities
    )
    
    # Sort by similarity
    results <- results[order(-results$Similarity),]
    
    # Remove the channel itself
    results <- results[results$Youtuber != youtube$Youtuber[channel_index[1]],]
    
    # Get top 10
    top_10 <- head(results, 10)
    
    # Print results
    print(top_10)
    
    # Fix the plotting code
    plot_title <- paste("Top 10 Channels Similar to", youtube$Youtuber[channel_index[1]])
    
    # Make sure top_10$Youtuber is a factor with levels in the right order
    top_10$Youtuber <- factor(top_10$Youtuber, levels = top_10$Youtuber[order(top_10$Similarity)])
    
    p <- ggplot(data = top_10) + 
      geom_bar(aes(x = Youtuber, y = Similarity), stat = "identity") +
      coord_flip() +
      labs(title = plot_title, x = "Channel", y = "Similarity") +
      theme_minimal()
    
    print(p)
    
    return(top_10)
  } else {
    cat("\nChannel", channel_name, "not found in the dataset\n")
    return(NULL)
  }
}

# Find channels similar to T-Series
t_series_similar <- find_similar_channels("T-Series")

# Find channels similar to MrBeast
mrbeast_similar <- find_similar_channels("MrBeast")


# Find channels similar to T-Series
cocomelon_similar <- find_similar_channels("Cocomelon")


# Plot the similarity scores
ggplot(similarity_df, aes(x = rownames(similarity_df), y = MrBeast)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Cosine Similarity with MrBeast", x = "Channel", y = "Cosine Similarity") +
  theme_minimal()


