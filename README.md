# IJC437-project---Can-Acoustic-Features-be-used-to-Predict-Song-Success?-
This anlaysis evaluates whether the sonic profile of a song can be used to predict its success and popularity. 


# Music Analytics: PCA, Clustering & Success Prediction
# ------------------------------------------------------
# ------------------------------------------------------
#### Data analysis of song performance using dimensionality reduction, clustering, and regression modeling.

# Workflow

#  - Importing and cleaning data
#  - Subset merging
#  - Construction of a composite success metric
#  - Exploratory visualisations and analysis
#  - Correlation analysis
#  - Principal Component Analysis (PCA)
#  - Clustering in PCA space
#  - PCA-based linear regression modelling

# Libraries
# ---------
library(tidyverse)
library(ggplot2)
library(hexbin)
library(readr)
library(viridis)
library(reshape2)

# Importing Music0set Dataset
# ----------------------------

# Create song_rank dataframe from song_chart.csv
# Hot 100 charts table for songs.
song_rank <- read.csv("song_chart.csv")
song_rank <- data.frame(song_rank)

# Create acoustic features data frame from song_chart.csv
acoustic_features <- read.csv("acoustic_features.csv")
acoustic_features <- data.frame(acoustic_features)

# Cleaning Data
# -------------

# Clean song_rank
str(song_rank)
# clean data in the song_rank dataframe.
# It is a single string, and isn't separated by commas.
names(song_rank)
song_rank_clean <- song_rank %>%
  separate(col = 1, into = c("id", "position", "peak_position", "weeks_on_chart", "date"),
           sep = "\\s+", convert = TRUE)
View(song_rank_clean)

# Clean acoustic_features
str(acoustic_features)

acoustic_features_clean <- acoustic_features %>%
  separate(
    col = 1,
    into = c("song_id", "duration_ms", "key", "mode", "time_signature",
             "acousticness", "danceability", "energy", "instrumentalness",
             "liveness", "loudness", "speechiness", "valence", "tempo"),
    sep = "\\s+",
    convert = TRUE
  )

# Put songs into dataframe.  
# Data is tab separated.
# The data is to be separated by a column wherever a \t is found.
raw_lines <- readLines("songs.csv")
head(raw_lines)

# In this case, use read_tsv.
songs_clean <- read_tsv(
  "D:/DATA SCIENCE 2 MORE SPACE/Coursework/Data Visualisation/MUSIC DATASET COMPILATION/songs.csv",
  quote = ""
)

# Create new data frame, song_clean (contains song_info).
dim(songs_clean)
names(songs_clean)
head(songs_clean, 3)

View(songs_clean)

# Merging Tables
# --------------

# Merge song_rank_clean and acoustic_features_clean data frames together based on song ID.
song_rank_and_acoustic_features <- left_join(acoustic_features_clean, song_rank_clean, by = c("song_id" = "id"))
View(song_rank_and_acoustic_features)

# Merge main table (song_rank_and_acoustic_features) with songs_clean ( more information about the song) by song_id.
song_rank_and_acoustic_features_and_song_info <- left_join(song_rank_and_acoustic_features, songs_clean, by = c("song_id"))
View(song_rank_and_acoustic_features_and_song_info)

# Creating composite success score
#---------------------------------

# Using Z-scores, peak_position, position, and weeks on chart will be standardised before generating a composite success rating.

song_rank_and_acoustic_features_and_song_info <- song_rank_and_acoustic_features_and_song_info %>%
  mutate(
    # Standardizes raw metrics
    # scale() converts each variable into a z-score:
    z_position       = scale(position),
    z_peak_position  = scale(peak_position),
    z_weeks          = scale(weeks_on_chart),
    
  
    # Reverse position metrics (lower = better)
    # Standardization makes all variables comparable.
    
    # Each will have mean = 0 and SD = 1
    # Regression, clustering, PCA expect standardized inputs
    
    z_position_rev      = -z_position,
    z_peak_position_rev = -z_peak_position,
    
    
    
    # Compute the 3-variable composite success score (position + peak + weeks)
    success_3var = (z_position_rev + z_peak_position_rev + z_weeks) / 3)

    View(song_rank_and_acoustic_features_and_song_info)



# Exploratory Visualisations
# --------------------------
# --------------------------

# Each variable is compared to the composite success score.
# These visualisations display the correlation between variables.

# Hexbin Heatmap
# ---------------

# Shows the correlation between Danceability and Valence, coloured by the 3-variable composite success rating.

ggplot(song_rank_and_acoustic_features_and_song_info,
       aes(x = danceability, y = valence, z = success_3var)) +
  stat_summary_hex(fun = "mean") +
  scale_fill_viridis_c(option = "viridis") + # <- Viridis palette
  labs(title = "Hexbin Heatmap of Danceability against Valence",
       y = "Valence",
       x = "Danceabillity",
       fill = "Success") +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation Heatmap
# -------------------
    
# The correlation heatmap shows the correlation between song variables.
# It also shows the strength of those correlations
# The lighter the colour, the greater the correlation.
    
numeric_vars <- song_rank_and_acoustic_features_and_song_info %>%
  select(duration_ms, acousticness, danceability, energy, instrumentalness,
         liveness, loudness, speechiness, valence, tempo, popularity,
         position, peak_position, weeks_on_chart, success_3var)

cor_matrix <- round(cor(numeric_vars, use = "pairwise.complete.obs"), 2)

melted_cor <- melt(cor_matrix)

ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis") + # <- Function (scale_fill_brewer) uses Viridis to show the level of correlation 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  labs(title = "Correlation Heatmap of Music Dataset",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  theme(plot.title = element_text(hjust = 0.5))

# Scatter plot
# ------------
# Shows the correlation between Loudness and energy, coloured by the 3-variable composite success rating.

ggplot(song_rank_and_acoustic_features_and_song_info,
       aes(x = loudness, y = energy, colour = success_3var)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE, colour = "black") +
  scale_colour_viridis_c(option = "viridis") +   # <- Viridis palette
  labs(title = "Scatter Plot of Energy against Loudness",
       x = "Loudness (dB)",
       y = "Energy",
       colour = "Success") +
  theme(plot.title = element_text(hjust = 0.5))

# 2D Contour Density Plot
# -----------------------

# shows the correlation between tempo and overall success using the composite success rating.
# It also shows the density of songs at each level of success.
# The lighter the colour, the greater the concentration of songs.

ggplot(song_rank_and_acoustic_features_and_song_info,
       aes(x = tempo, y = success_3var)) +
  stat_density2d(aes(fill = after_stat(level)), geom = "polygon") +
  scale_fill_viridis_c(option = "viridis") +
  scale_x_continuous(breaks = seq(0, 250, by = 50), limits = c(0, 250)) + # x-axis ticks every 50
  labs(title = "2D Density Plot of Tempo against Average Position",
       y = "Success",
       x = "Tempo (BPM)",
       fill = "Density") +
  theme(plot.title = element_text(hjust = 0.5))

# Hexbin Plot
# -----------
# Shows the correlation between Liveness, Acousticness, and song success using the composite success rating. 
# Colour = success rating per hex

ggplot(song_rank_and_acoustic_features_and_song_info,
       aes(x = liveness,
           y = acousticness,
           z = success_3var)) +
  
  stat_summary_hex(fun = "mean") +  
  scale_fill_viridis_c(option = "viridis") +
  
  labs(title = "Hexbin: Liveness vs Acousticness",
       x = "Liveness",
       y = "Acousticness",
       fill = "Success") +
  theme(plot.title = element_text(hjust = 0.5))

# PCA Analysis
# ------------

# Select desired predictors of success.
predictors <- song_rank_and_acoustic_features_and_song_info %>%
  select(duration_ms, acousticness, danceability, energy,
         instrumentalness, liveness, loudness, speechiness,
         valence, tempo)

# Standardize predictors.
predictors_scaled <- scale(predictors)

# Perform PCA analysis
pca <- prcomp(predictors_scaled, center = TRUE, scale. = TRUE)

# Summary (variance explained)
summary(pca)


# Scree Plot
# ----------

# This scree plot visualises principal components and aids in their selection.
# The elbow begins at PC2, suggesting the first 2 principal components should be kept. 

# Calculate the proportion of total variance explained by each principal component for the scree plot
fv_prop <- pca$sdev^2 / sum(pca$sdev^2)

ggplot(data.frame(PC = 1:length(fv_prop), Variance = fv_prop),
       aes(x = PC, y = Variance)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:length(fv_prop)) +
  labs(title = "Scree Plot: Variance Explained by Each Principal Component",
       x = "Principal Component",
       y = "Proportion of Variance Explained") +
  theme(plot.title = element_text(hjust = 0.5))

# display PC loading values
round(pca$rotation[, 1:2], 2)

# PCA Analysis Visualisations
# ---------------------------
# ---------------------------

# Table of Loading Values for PC1
# -------------------------------
loadings_pc1 <- data.frame(
  Variable = rownames(pca$rotation),
  Loading  = pca$rotation[, 1]
) %>%
  arrange(desc(abs(Loading)))   # sort by importance

ggplot(loadings_pc1,
       aes(x = reorder(Variable, Loading), y = Loading, fill = Loading)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Loading, 3)),
            hjust = ifelse(loadings_pc1$Loading > 0, -0.1, 1.1),
            size = 3) +
  scale_fill_viridis(option = "viridis") +
  labs(title = "Ranked PCA Loadings (PC1)",
       x = "Variable",
       y = "Loading Value") +
  theme(plot.title = element_text(hjust = 0.5))

# Table of Loading Values for PC2
# -------------------------------

loadings_pc2 <- data.frame(
  Variable = rownames(pca$rotation),
  Loading  = pca$rotation[, 2]
) %>%
  arrange(desc(abs(Loading)))

ggplot(loadings_pc2,
       aes(x = reorder(Variable, Loading), y = Loading, fill = Loading)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Loading, 3)),
            hjust = ifelse(loadings_pc2$Loading > 0, -0.1, 1.1),
            size = 3) +
  scale_fill_viridis(option = "viridis") +
  labs(title = "Ranked PCA Loadings (PC2)",
       x = "Variable",
       y = "Loading Value") +
  theme(plot.title = element_text(hjust = 0.5))

# Clustering Principal Components 
# -------------------------------

# PCA scores
pc_scores <- as.data.frame(pca$x[, 1:2])

# K-means clustering
set.seed(123)
k <- 3
clusters <- kmeans(pc_scores, centers = k)
pc_scores$cluster <- factor(clusters$cluster)

# Scatter plot of clusters using Viridis colour palette
ggplot(pc_scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_viridis_d(option = "viridis") +  # Viridis colour for discrete clusters
  labs(title = "Clustered PCA Scatter Plot",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5))

# Cluster Plot with Centroids using Viridis Colour Palette
# ------------------------------------------------------
centers <- as.data.frame(clusters$centers)

ggplot(pc_scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_point(data = centers,
             aes(x = PC1, y = PC2),
             color = "red",      # colours centroids in red
             size = 4,
             shape = 4,
             stroke = 1.5)+
  scale_color_viridis_d(option = "viridis") +  # Viridis colour palette
  labs(title = "Clustered PCA Scatter Plot with Centroids",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5))

# Add success_3var column to pc_scores.
pc_scores$success_3var <- song_rank_and_acoustic_features_and_song_info$success_3var


# Summarise each cluster's average song success value
cluster_summary <- pc_scores %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    mean_success = mean(success_3var, na.rm = TRUE),
    median_success = median(success_3var, na.rm = TRUE),
    sd_success = sd(success_3var, na.rm = TRUE)
  )

cluster_summary


# Boxplot of Cluster Success 
# --------------------------
ggplot(pc_scores, aes(x = cluster, y = success_3var, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d(option = "viridis") +
  labs(title = "Distribution of Song Success by Cluster",
       x = "Cluster",
       y = "Success") +
  theme(plot.title = element_text(hjust = 0.5))


# Linear Regression Model
# -----------------------

set.seed(123)  # Set.seed for reproducibility

# Split data into 90% train, 10% test
train_indices <- sample(nrow(song_rank_and_acoustic_features_and_song_info), 
                        size = 0.9 * nrow(song_rank_and_acoustic_features_and_song_info))

train_data <- song_rank_and_acoustic_features_and_song_info[train_indices, ]
test_data  <- song_rank_and_acoustic_features_and_song_info[-train_indices, ]


# Select predictors for PCA (Numeric).

predictors_train <- train_data %>%
  select(duration_ms, acousticness, danceability, energy,
         instrumentalness, liveness, loudness, speechiness,
         valence, tempo)

predictors_test <- test_data %>%
  select(duration_ms, acousticness, danceability, energy,
         instrumentalness, liveness, loudness, speechiness,
         valence, tempo)


# Standardize training predictors
predictors_train_scaled <- scale(predictors_train)


# Fit PCA onto training data
pca_train <- prcomp(predictors_train_scaled, center = TRUE, scale. = TRUE)


# Transform training and testing datasets using PCA rotation
train_pcs <- as.data.frame(predict(pca_train, newdata = predictors_train_scaled))
test_pcs  <- as.data.frame(predict(pca_train, newdata = scale(predictors_test, 
                                                              center = attr(predictors_train_scaled, "scaled:center"),
                                                              scale  = attr(predictors_train_scaled, "scaled:scale"))))

# Add success variable
train_pcs$success_3var <- train_data$success_3var
test_pcs$success_3var  <- test_data$success_3var


# Fit linear regression using top 2 PCs
lm_model <- lm(success_3var ~ PC1 + PC2, data = train_pcs)
summary(lm_model)

# Predict using test set
predictions <- predict(lm_model, newdata = test_pcs)

# Evaluate performance
rmse <- sqrt(mean((predictions - test_pcs$success_3var)^2))
cat("RMSE on test set:", round(rmse, 4), "\n")

# Visualize Actual vs Predicted Success
plot_data <- data.frame(
  Actual = test_pcs$success_3var,
  Predicted = predictions
)


# Hexbin Plot of Regression Model Results
# ---------------------------------------

# Visualise actual vs predicted success

# red dashed line = ideal predictions
# if the model was perfect, all results would lie on this line. 

# Blue line = smoothed relationship between actual success and the model's predicted success
# Shows the model’s average predicted value for a given actual outcome

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  stat_binhex(bins = 50) +
  scale_fill_viridis_c(option = "viridis") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1) +
  labs(title = "Actual vs Predicted Song Success",
       x = "Actual Success",
       y = "Predicted Success",
       fill = "Number of Songs") +
  theme(plot.title = element_text(hjust = 0.5))
