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

# Turn off scientific notation
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

# Create a Document-Term Matrix- TD-IDF
dtm <- DocumentTermMatrix(corpus)

# Convert to matrix
dtm_matrix <- as.matrix(dtm)

# Find Cocomelon's index
cocomelon_index <- which(grepl("Cocomelon", youtube$Youtuber, ignore.case = TRUE))

# If Cocomelon is found, calculate similarities
if(length(cocomelon_index) > 0) {
  # Calculate similarities manually to avoid the conformity error
  similarities <- numeric(nrow(dtm_matrix))
  cocomelon_vector <- dtm_matrix[cocomelon_index[1], ]
  
  for(i in 1:nrow(dtm_matrix)) {
    # Calculate cosine similarity manually if needed
    # Cosine similarity = (A·B) / (||A|| × ||B||)
    dot_product <- sum(dtm_matrix[i, ] * cocomelon_vector)
    norm_a <- sqrt(sum(dtm_matrix[i, ]^2))
    norm_b <- sqrt(sum(cocomelon_vector^2))
    
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
  
  # Remove Cocomelon itself
  results <- results[results$Youtuber != youtube$Youtuber[cocomelon_index[1]],]
  
  # Get top 10
  top_10 <- head(results, 10)
  
  # Print results
  print(top_10)
  
  # Plot results
  ggplot(top_10, aes(x = reorder(Youtuber, Similarity), y = Similarity)) +
    geom_bar(stat = "identity", fill = "pink") +
    coord_flip() +
    labs(title = "Top 10 Channels Similar to Cocomelon", x = "Channel", y = "Similarity") +
    theme_minimal()

  #making the subscribers a numerical value 
  youtube$subscribers <- as.numeric(gsub(",", "", youtube$subscribers))
  
  # Get top 10 by category
  top10_education <- subset(youtube, category == "Education")
  top10_education <- head(top5_education[order(-top5_education$subscribers), ], 10)
  
  
  top10_music <- subset(youtube, category == "Music")
  top10_music <- head(top5_music[order(-top5_music$subscribers), ], 10)
  
  top10_entertainment <- subset(youtube, category == "Entertainment")
  top10_entertainment <- head(top5_entertainment[order(-top5_entertainment$subscribers), ], 10)
  
  # Show results
  print("Top 10 Education Channels:")
  print(top10_education[, c("Youtuber", "subscribers")])
  print("Top 10 Music Channels:")
  print(top10_music[, c("Youtuber", "subscribers")])
  print("Top 10 Entertainment Channels:")
  print(top10_entertainment[, c("Youtuber", "subscribers")])
  

# Plot the similarity scores
ggplot(similarity_df, aes(x = rownames(similarity_df), y = MrBeast)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Cosine Similarity with MrBeast", x = "Channel", y = "Cosine Similarity") +
  theme_minimal()
