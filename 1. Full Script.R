# =========================================
# SECTION 1: LOAD LIBRARIES
# =========================================

library(readxl)
library(writexl)
library(mclust)
library(dplyr)
library(tidyr)
library(caret)
library(factoextra)
library(cluster)
library(xray) #for anomalies function
library(corrplot) # visualise correlations
library(fclust)
library(UniversalCVI)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(data.table)
library(xgboost)
library(ggplot2)
library(foreach)
library(doParallel)
library(patchwork)
library(reshape2)
library(grid)
library(gridExtra)
library(umap)
library(ggridges)
library(foreach)
library(doParallel)

# =========================================
# SECTION 2: DATA LOADING & INITIAL SETUP
# =========================================

# Load the data
dataO <- read_excel("raw_players.xlsx", sheet = 1)
dataD <- read_excel("raw_players.xlsx", sheet = 2)
lineup <- read_excel("raw_players.xlsx", sheet = 3)
teamRating <- read_excel("raw_players.xlsx", sheet = 4)

# Extract player bio data
bioO <- dplyr::select(dataO, c(Player, Season, G, MP, "MP/G", Age, Team, "CombinedO/10", "O-EPM", "O-LEBRON"))
bioD <- dplyr::select(dataD, c(Player, Season, G, MP, "MP/G", Age, Team, "CombinedD/10", "D-EPM", "D-LEBRON"))

# Prep data for clustering removing non-desired features
# Min max 0-1 normalization for offensive data
clusterO <- dataO[, setdiff(names(dataO), names(bioO))]
process <- preProcess(as.data.frame(clusterO), method = c("range"))
clusterO <- predict(process, as.data.frame(clusterO))

# Min max 0-1 normalization for defensive data
clusterD <- dataD[, setdiff(names(dataD), names(bioD))]
process <- preProcess(as.data.frame(clusterD), method = c("range"))
clusterD <- predict(process, as.data.frame(clusterD))

rm(process)
rdpu_palette <- scales::brewer_pal(palette = "RdPu")(5)  # Extracts 9 colors from the "RdPu" palette

# =========================================
# SECTION 3: EDA
# =========================================

#anomalies(dataO) # check dataset is complete and no unexpected values

clusterO %>% 
  cor() %>% 
  corrplot(method = "color", type = "upper", order = "hclust",
           col = colorRampPalette(c("deeppink3", "white", "deepskyblue3"))(200),
           tl.col = "black", tl.srt = 45, tl.cex = 0.7, # Text label color and rotation
           addCoef.col = "black",
           number.cex = 0.7)

clusterD %>% 
  cor() %>% 
  corrplot(method = "color", type = "upper", order = "hclust",
           col = colorRampPalette(c("deeppink3", "white", "deepskyblue3"))(200),
           tl.col = "black", tl.srt = 45, tl.cex = 0.7, # Text label color and rotation
           addCoef.col = "black",
           number.cex = 0.7)

# Hopkins statistic -> both >0.8, data is significiantly clusterable
#get_clust_tendency(clusterO, 100)
#get_clust_tendency(clusterD, 100)

# =========================================
# SECTION 4: CVI Plots (via UniversalCVI)
# =========================================

# Custom theme to remove gridlines but keep the black border
custom_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 9),  # Adjust axis title font size
        axis.text = element_text(size = 9),    # Adjust axis text font size
        plot.title = element_text(size = 10))  # Adjust plot title font size

OffensiveCVI <- FzzyCVIs(clusterO, cmax = 14, cmin = 7, indexlist = c("XB", "KWON2", "WP", "HF"),
                         corr = 'pearson', method = 'FCM', fzm = 1.33, iter = 100, nstart = 30, NCstart = TRUE)

DefensiveCVI <- FzzyCVIs(clusterD, cmax = 9, cmin = 4, indexlist = c("XB", "KWON2", "WP", "HF"),
                         corr = 'pearson', method = 'FCM', fzm = 1.33, iter = 100, nstart = 30, NCstart = TRUE)

plot_XB <- ggplot(OffensiveCVI$XB, aes(x = c, y = XB)) +
  geom_line() +
  geom_point() +
  geom_point(data = subset(OffensiveCVI$XB, c == 10), color = "red", size = 3) +
  geom_vline(xintercept = 8, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 13, linetype = "dashed", color = "red") +
  labs(x = "Number of Clusters", y = "XB", title = "Xie-Beni Plot") +
  custom_theme

# Plot for KWON2
plot_KWON2 <- ggplot(OffensiveCVI$KWON2, aes(x = c, y = KWON2)) +
  geom_line() +
  geom_point() +
  geom_point(data = subset(OffensiveCVI$KWON2, c == 10), color = "red", size = 3) +
  geom_vline(xintercept = 8, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 13, linetype = "dashed", color = "red") +
  labs(x = "Number of Clusters", y = "KWON2", title = "KWON-2 Plot") +
  custom_theme

# Plot for WPI
plot_WP <- ggplot(OffensiveCVI$WP, aes(x = c, y = WPI)) +
  geom_line() +
  geom_point() +
  geom_point(data = subset(OffensiveCVI$WP, c == 10), color = "red", size = 3) +
  geom_vline(xintercept = 8, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 13, linetype = "dashed", color = "red") +
  labs(x = "Number of Clusters", y = "WPI", title = "WPI Plot") +
  custom_theme

# Plot for HF
plot_HF <- ggplot(OffensiveCVI$HF, aes(x = c, y = HF)) +
  geom_line() +
  geom_point() +
  geom_point(data = subset(OffensiveCVI$HF, c == 10), color = "red", size = 3) +
  geom_vline(xintercept = 8, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 13, linetype = "dashed", color = "red") +
  labs(x = "Number of Clusters", y = "HF", title = "HF Plot") +
  custom_theme

# Arrange all plots next to each other using patchwork
combined_plot <- plot_XB + plot_KWON2 + plot_WP + plot_HF +
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Cluster Validation Indices for Offence") 

# Print the combined plot
print(combined_plot)

str(DefensiveCVI$XB)
str(DefensiveCVI$KWON2)
str(DefensiveCVI$WP)
str(DefensiveCVI$HF)

# Plot for XB
plot_XB <- ggplot(DefensiveCVI$XB, aes(x = c, y = XB)) +
  geom_line() +
  geom_point() +
  geom_point(data = subset(DefensiveCVI$XB, c == 6), color = "red", size = 3) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "red") +
  labs(x = "Number of Clusters", y = "XB", title = "Xie-Beni Plot") +
  custom_theme

# Plot for KWON2
plot_KWON2 <- ggplot(DefensiveCVI$KWON2, aes(x = c, y = KWON2)) +
  geom_line() +
  geom_point() +
  geom_point(data = subset(DefensiveCVI$KWON2, c == 6), color = "red", size = 3) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "red") +
  labs(x = "Number of Clusters", y = "KWON2", title = "KWON-2 Plot") +
  custom_theme

# Plot for WPI
plot_WP <- ggplot(DefensiveCVI$WP, aes(x = c, y = WPI)) +
  geom_line() +
  geom_point() +
  geom_point(data = subset(DefensiveCVI$WP, c == 6), color = "red", size = 3) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "red") +
  labs(x = "Number of Clusters", y = "WPI", title = "WPI Plot") +
  custom_theme

# Plot for HF
plot_HF <- ggplot(DefensiveCVI$HF, aes(x = c, y = HF)) +
  geom_line() +
  geom_point() +
  geom_point(data = subset(DefensiveCVI$HF, c == 6), color = "red", size = 3) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 8, linetype = "dashed", color = "red") +
  labs(x = "Number of Clusters", y = "HF", title = "HF Plot") +
  custom_theme

# Arrange all plots next to each other using patchwork
combined_plot <- plot_XB + plot_KWON2 + plot_WP + plot_HF +
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Cluster Validation Indices for Defence")  

# Print the combined plot
print(combined_plot)

# =========================================
# SECTION 5: Role Clustering
# =========================================

# Perform fuzzy c-means clustering for offensive data
set.seed(133)
numClustersO <- 10
fcmResultsO <- e1071::cmeans(clusterO, centers = numClustersO, m = 1.33, iter.max = 100, verbose = FALSE)

dataO$Cluster <- fcmResultsO$cluster
clustProbsO <- fcmResultsO$membership

# Perform fuzzy c-means clustering for defensive data
set.seed(133)
numClustersD <- 6
fcmResultsD <- e1071::cmeans(clusterD, centers = numClustersD, m = 1.33, iter.max = 100, verbose = FALSE)

dataD$Cluster <- fcmResultsD$cluster
clustProbsD <- fcmResultsD$membership

# Function to adjust probabilities
adjust_probabilities <- function(probs) {
  
  adjustedProbs <- round(probs / 0.25) * 0.25 # round to 0.25
  sumDiff <- 1 - sum(adjustedProbs) #calculate lineup sum
  recalcOrder <- order(abs(probs - adjustedProbs), decreasing = TRUE)   # prioritise on initial rounding error
  
  # Continue adjusting until the total difference is zero
  for (i in recalcOrder) {
    if (sumDiff == 0) {
      break
    }
    
    # Decide increase or decrease
    if (sumDiff > 0) {
      increment <- 0.25
    } else {
      increment <- -0.25
    }
    
    # Check adjustment is valid (probability remains within [0, 1])
    if (adjustedProbs[i] + increment >= 0 && adjustedProbs[i] + increment <= 1) {
      adjustedProbs[i] <- adjustedProbs[i] + increment
      sumDiff <- sumDiff - increment
    }
  }
  
  # Final adjustment to ensure sum is exactly 1
  while (abs(sumDiff) >= 0.01) {
    # Find the closest probability to adjust
    recalcIDX <- which.min(abs(adjustedProbs + sumDiff / length(adjustedProbs) - adjustedProbs))
    # Apply the smallest possible adjustment
    adjustedProbs[recalcIDX] <- adjustedProbs[recalcIDX] + sumDiff
    sumDiff <- 1 - sum(adjustedProbs)
  }
  return(adjustedProbs)
}

# Apply the adjustment to each player's probabilities for offensive data
adjustedO <- t(apply(clustProbsO, 1, adjust_probabilities))

# Add adjusted probabilities to the dataO dataframe
for (i in 1:numClustersO) {
  dataO[[paste0('Cluster_', i, '_prob')]] <- adjustedO[, i]
}

# Apply the adjustment to each player's probabilities for defensive data
adjustedD <- t(apply(clustProbsD, 1, adjust_probabilities))

# Add adjusted probabilities to the dataD dataframe
for (i in 1:numClustersD) {
  dataD[[paste0('Cluster_', i, '_prob')]] <- adjustedD[, i]
}

# Count number of players in each cluster for offensive data
clustNumO <- dataO %>%
  dplyr::group_by(Cluster) %>%
  dplyr::summarize(Count = n()) %>%
  dplyr::arrange(desc(Count)) %>% 
  print()

# Average clusters for offensive data
clusterAvO <- dataO %>%
  dplyr::group_by(Cluster) %>%
  dplyr::summarize(across(8:33, mean, na.rm = TRUE)) %>% 
  print()

# Count number of players in each cluster for defensive data
clustNumD <- dataD %>%
  dplyr::group_by(Cluster) %>%
  dplyr::summarize(Count = n()) %>%
  dplyr::arrange(desc(Count)) %>% 
  print()

# Average clusters for defensive data
clusterAvD <- dataD %>%
  dplyr::group_by(Cluster) %>%
  dplyr::summarize(across(8:23, mean, na.rm = TRUE)) %>% 
  print()

export <- list(
  avO = clusterAvO,
  avD = clusterAvD
)

#write_xlsx(export, path = "clusterAv.xlsx")

# =========================================
# SECTION 6: UMAP Plot
# =========================================

# Set seed for reproducibility
set.seed(133)

# Calculate UMAP for offensive data
umapO <- umap(dataO[, 8:33], n_components = 2, n_neighbors = 30, metric = "euclidean", min_dist = 0.05)
cluster_labels <- c("Offensive Superstar", "Mobile Big", "Rim Crusher", "Facilitator", 
                    "Skilled Big Man", "Glue Guy", "Dynamic Shooter", 
                    "Static 3PT Shooter", "Defensive Specialist", "Secondary Generator")

# Add UMAP projections to dataO dataframe
testO <- dataO
testO$Cluster <- cluster_labels[testO$Cluster]
testO$umapX <- umapO$layout[,1]
testO$umapY <- umapO$layout[,2]
testO <- testO %>%
  mutate(Cluster = factor(Cluster, levels = cluster_labels))

# Plot UMAP for offensive data
ggplot(testO, aes(x = umapX, y = umapY, color = as.factor(Cluster))) +
  geom_point(size = 2) +
  labs(title = "UMAP Plot for Offensive Roles", color = "Role") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),  # Adjust size for axis markings
        axis.title = element_text(size = 11))  # Adjust size for axis labels

# Calculate UMAP for defensive data
umapD <- umap(dataD[, 8:23], n_components = 2, n_neighbors = 50, metric = "euclidean")
cluster_labels <- c("Chaser", "Big Helper", "Anchor Big", "Point of Attack", "Wing Stopper", "Off-Ball Defender")

# Add UMAP projections to dataD dataframe
testD <- dataD
testD$Cluster <- cluster_labels[testD$Cluster]
testD$umapX <- umapD$layout[,1]
testD$umapY <- umapD$layout[,2]
testD <- testD %>%
  mutate(Cluster = factor(Cluster, levels = cluster_labels))

# Plot UMAP for defensive data
ggplot(testD, aes(x = umapX, y = umapY, color = as.factor(Cluster))) +
  geom_point(size = 2) +
  labs(title = "UMAP Plot for Defensive Roles", color = "Role") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),  # Adjust size for axis markings
        axis.title = element_text(size = 11))  # Adjust size for axis labels

# =========================================
# SECTION 7: Offensive Analysis
# =========================================

cluster_labels <- c("Offensive Superstar", "Mobile Big", "Rim Crusher", "Facilitator", 
                    "Skilled Big Man", "Glue Guy", "Dynamic Shooter", 
                    "Static 3PT Shooter", "Defensive Specialist", "Secondary Generator")

cluster_probsO <- dataO[, 35:44]

# 1. Offensive data correlations
cluster_probsO %>% 
  cor() %>% 
  corrplot(method = "color", type = "upper", order = "hclust",
           col = colorRampPalette(c("deeppink3", "white", "deepskyblue3"))(200),
           tl.col = "black", tl.srt = 45, tl.cex = 0.7, # Text label color and rotation
           addCoef.col = "black",
           number.cex = 0.7)

# 2. Grouping role allocations into stacked bar chart
bin_counts <- melt(cluster_probsO, variable.name = "Cluster", value.name = "Probability")%>%
  filter(Probability > 0)  %>% 
  mutate(Probability_Bin = cut(Probability, 
                               breaks = c(0, 0.25, 0.5, 0.75, 1), 
                               include.lowest = TRUE, 
                               labels = c("0 - 0.25", "0.25 - 0.50", "0.50 - 0.75", "0.75 - 1"))) %>% 
  group_by(Cluster, Probability_Bin) %>%
  summarise(Frequency = n()) %>%
  ungroup() 

# Assuming clusters are numbered 1 to 10
names(cluster_labels) <- as.character(1:10)

# Modify the bin_counts dataframe to replace Cluster names with the new labels
bin_counts$Cluster <- cluster_labels[bin_counts$Cluster] 
bin_counts <- bin_counts %>%
  mutate(Cluster = factor(Cluster, levels = cluster_labels))

# Create the stacked bar plot
ggplot(bin_counts, aes(x = Cluster, y = Frequency, fill = Probability_Bin)) +
  geom_bar(stat = "identity") +
  labs(title = "Allocation of Players Across Offensive Roles", 
       x = "Offensive Role", 
       y = "Number of Players") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),  # Adjust size for axis markings
        axis.title = element_text(size = 12))+  # Adjust size for axis labels
  scale_fill_brewer(palette = "RdPu", name = "Membership Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Plotting offensive role combinations 
filtered_data <- cluster_probsO[!apply(cluster_probsO == 1, 1, any), ]
cluster_combinations <- apply(filtered_data, 1, function(row) {
  clusters_present <- which(row > 0)
  paste(clusters_present, collapse = "-")
})

combination_freq <- table(cluster_combinations)
combination_df <- as.data.frame(combination_freq)
combination_df <- combination_df[order(-combination_df$Freq), ] 
print(combination_df)

# plot the top 10 role combinations
plot <- ggplot(head(combination_df, 10), aes(x = reorder(cluster_combinations, -Freq), y = Freq, fill = Freq)) +
  geom_bar(stat = "identity", fill = rdpu_palette[2]) +
  labs(title = "10 Most Common Offensive Role Combinations",
       x = "Role Pairings",
       y = "Number of Players") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),   # Adjust size for axis markings
        axis.title = element_text(size = 13))  # Adjust size for axis labels

# Add custom annotation (legend)
legend_text <- paste("Legend:",
                     "1 - Offensive Superstar",
                     "2 - Mobile Big",
                     "3 - Rim Crusher",
                     "4 - Facilitator",
                     "5 - Skilled Big Man",
                     "6 - Glue Guy",
                     "7 - Dynamic Shooter",
                     "8 - Static 3PT Shooter",
                     "9 - Defensive Specialist",
                     "10 - Secondary Generator",
                     sep = "\n")

plot + annotation_custom(
  grob = textGrob(legend_text, x = 0.71, y = 0.97, just = c("left", "top"), gp = gpar(fontsize = 11)),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)

# 4. Role ridgeplots for distribution of O-BPI
dataO$Cluster <- as.factor(dataO$Cluster) 
rdpu_palette2 <- scales::brewer_pal(palette = "RdPu")(9)
rdpu_palette3 <- c("#FDE4EF",rdpu_palette2[2:9], "#30075e")

# Create the ridge plot with the specified changes
ggplot(dataO, aes(x = `CombinedO/10`, y = reorder(Cluster, desc(Cluster)), fill = as.factor(Cluster), group = Cluster)) +
  geom_density_ridges(scale = 1.5, alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +  # Set x-axis breaks at 0, 2, 4, 6, 8, 10
  labs(title = "Distribution of O-BPI Ratings by Offensive Role",
       x = "O-BPI",
       y = "Role",  # Add y-axis title "Cluster"
       fill = "Role") +  # Legend title
  scale_fill_manual(values = c(rdpu_palette3),
                    labels = cluster_labels) +  # Use custom labels for the legend
  theme_ridges() +
  theme_bw() +
  theme(
    legend.position = "right",         # Position the legend to the right of the plot
    axis.title.x = element_text(hjust = 0.5, size = 12),  # Center x-axis title
    axis.title.y = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(size=12),
    axis.text.y = element_blank(),      # Remove y-axis text (numbers)
    plot.title = element_text(face = "plain", size = 12),  # Make the title normal font
    legend.title = element_text(face = "plain", size = 12),  # Make the legend title normal font
    legend.text = element_text(size = 11)  # Set the size of legend text
  ) 

# =========================================
# SECTION 8: Defensive Analysis
# =========================================

cluster_labels <- c("Chaser", "Big Helper", "Anchor Big", "Point of Attack", "Wing Stopper", "Off-Ball Defender")

cluster_probsD<- dataD[,25:30]

# 1. Defensive data correlations
cluster_probsD%>% 
  cor() %>% 
  corrplot(method = "color", type = "upper", order = "hclust",
           col = colorRampPalette(c("deeppink3", "white", "deepskyblue3"))(200),
           tl.col = "black", tl.srt = 45, tl.cex = 0.7, # Text label color and rotation
           addCoef.col = "black",
           number.cex = 0.7)

# 2. Grouping role allocations into stacked bar chart
bin_countsD <- melt(cluster_probsD, variable.name = "Cluster", value.name = "Probability")%>%
  filter(Probability > 0)  %>% 
  mutate(Probability_Bin = cut(Probability, 
                               breaks = c(0, 0.25, 0.5, 0.75, 1), 
                               include.lowest = TRUE, 
                               labels = c("0 - 0.25", "0.25 - 0.50", "0.50 - 0.75", "0.75 - 1"))) %>% 
  group_by(Cluster, Probability_Bin) %>%
  summarise(Frequency = n()) %>%
  ungroup()

# Assuming clusters are numbered 1 to 10
names(cluster_labels) <- as.character(1:6)

# Modify the bin_counts dataframe to replace Cluster names with the new labels
bin_countsD$Cluster <- cluster_labels[bin_countsD$Cluster]
bin_countsD <- bin_countsD %>%
  mutate(Cluster = factor(Cluster, levels = cluster_labels))

# Create the stacked bar plot
ggplot(bin_countsD, aes(x = Cluster, y = Frequency, fill = Probability_Bin)) +
  geom_bar(stat = "identity") +
  labs(title = "Allocation of Players Across Defensive Roles", 
       x = "Defensive Role", 
       y = "Number of Players") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),  # Adjust size for axis markings
        axis.title = element_text(size = 12))+  # Adjust size for axis labels
  scale_fill_brewer(palette = "RdPu", name = "Membership Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Plotting defensive role combinations 
filtered_data <- cluster_probsD[!apply(cluster_probsD == 1, 1, any), ]
cluster_combinations <- apply(filtered_data, 1, function(row) {
  clusters_present <- which(row > 0)
  paste(clusters_present, collapse = "-")
})

combination_freq <- table(cluster_combinations)
combination_df <- as.data.frame(combination_freq)
combination_df <- combination_df[order(-combination_df$Freq), ] 
print(combination_df)

# plot the top 10 role combinations
plot <- ggplot(head(combination_df, 10), aes(x = reorder(cluster_combinations, -Freq), y = Freq, fill = Freq)) +
  geom_bar(stat = "identity", fill = rdpu_palette[4]) +
  labs(title = "10 Most Common Defensive Role Combinations",
       x = "Role Pairings",
       y = "Number of Players") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),   # Adjust size for axis markings
        axis.title = element_text(size = 13))  # Adjust size for axis labels

# Add custom annotation (legend)
legend_text <- paste("Legend:",
                     "1 - Chaser",
                     "2 - Big Helper",
                     "3 - Anchor Big",
                     "4 - Point of Attack",
                     "5 - Wing Stopper",
                     "6 - Off-Ball Defender",
                     sep = "\n")

plot + annotation_custom(
  grob = textGrob(legend_text, x = 0.75, y = 0.96, just = c("left", "top"), gp = gpar(fontsize = 11)),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
) 

# 4. Role ridgeplots for distribution of D-BPI
ggplot(dataD, aes(x = `CombinedD/10`, y = reorder(Cluster, desc(Cluster)), fill = as.factor(Cluster), group = Cluster)) +
  geom_density_ridges(scale = 1.5, alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +  # Set x-axis breaks at 0, 2, 4, 6, 8, 10
  labs(title = "Distribution of D-BPI Ratings by Defenive Role",
       x = "D-BPI",
       y = "Role",  # Add y-axis title "Cluster"
       fill = "Role") +  # Legend title
  scale_fill_manual(values = c(rdpu_palette2[3:9]),
                    labels = cluster_labels) +  # Use custom labels for the legend
  theme_ridges() +
  theme_bw() +
  theme(
    legend.position = "right",         # Position the legend to the right of the plot
    axis.title.x = element_text(hjust = 0.5, size = 12),  # Center x-axis title
    axis.title.y = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(size=12),
    axis.text.y = element_blank(),      # Remove y-axis text (numbers)
    plot.title = element_text(face = "plain", size = 12),  # Make the title normal font
    legend.title = element_text(face = "plain", size = 12),  # Make the legend title normal font
    legend.text = element_text(size = 11)  # Set the size of legend text
  )

# =========================================
# SECTION 9: Clustering Evaluation - Assignment Distribution
# =========================================

# # Simple CVIs:
# fclust::PE(clustProbsO) # partition entropy
# fclust::PC(clustProbsO) # partition coefficient
# fclust::MPC(clustProbsO) # modified partition coefficient

# fuzzy silhouette index
fclust::SIL.F (clusterO, clustProbsO) # fuzzy silhouette coefficient
fclust::SIL.F (clusterD, clustProbsD) # fuzzy silhouette coefficient

# Plot cluster assignment distribution
clustersCountO <- dataO
clustersCountO$NumClustersAssigned <- apply(adjustedO, 1, function(x) sum(x > 0))

clusterTallyO <- clustersCountO %>%
  dplyr::group_by(NumClustersAssigned) %>%
  dplyr::summarize(Count = n()) %>%
  dplyr::arrange(NumClustersAssigned)

clustersCountD <- dataD
clustersCountD$NumClustersAssigned <- apply(adjustedD, 1, function(x) sum(x > 0))

clusterTallyD <- clustersCountD %>%
  dplyr::group_by(NumClustersAssigned) %>%
  dplyr::summarize(Count = n()) %>%
  dplyr::arrange(NumClustersAssigned)

max_y_value <- max(clusterTallyO$Count, clusterTallyD$Count)

# Plot graphs side by side for offence and defence
plotO <- ggplot(clusterTallyO, aes(x = NumClustersAssigned, y = Count)) +
  geom_bar(stat = "identity", fill = rdpu_palette[2]) +  # Use a middle shade from the RdPu palette
  labs(title = "Distribution of Offensive Versatility",
       x = "Clusters Assigned",
       y = "Number of Players") +
  ylim(0, max_y_value) +  # Set the y-axis limit
  theme_bw()
plotD <- ggplot(clusterTallyD, aes(x = NumClustersAssigned, y = Count)) +
  geom_bar(stat = "identity", fill = rdpu_palette[4]) +  # Use a darker shade from the RdPu palette
  labs(title = "Distribution of Defensive Versatility",
       x = "Clusters Assigned",
       y = "Number of Players") +
  ylim(0, max_y_value) +  # Set the y-axis limit
  theme_bw()
grid.arrange(plotO, plotD, ncol = 2)

rm (plotD, plotO, clusterTallyD, clusterTallyO,clustersCountD, clustersCountO, max_y_value)

# =========================================
# SECTION 10: Lineup Data Preparation
# =========================================

# Update lineups with cluster membership and BPIs
update_cluster_probs_and_rating <- function(player, season, all_data, num_clusters) {
  match_row <- all_data %>%
    filter(Player == player, Season == season)
  
  if (nrow(match_row) == 1) {
    return(as.list(match_row %>%
                     select(starts_with("Cluster_"), starts_with("Combined"))))
  } else {
    cluster_probs <- as.list(setNames(rep(NA, num_clusters), paste0("Cluster_", 1:num_clusters, "_prob")))
    cluster_probs$CombinedO_10 <- NA
    cluster_probs$CombinedD_10 <- NA
    return(cluster_probs)
  }
}

# Iterate through the lineups data for offensive and defensive clusters
for (i in 1:nrow(lineup)) {
  for (p in 1:5) {
    player_col <- paste0("P", p)
    rating_col_O <- paste0("O", p, "_rating")
    rating_col_D <- paste0("D", p, "_rating")
    player <- lineup[[player_col]][i]
    season <- lineup$Season[i]
    
    player_data_O <- update_cluster_probs_and_rating(player, season, dataO, numClustersO)
    player_data_D <- update_cluster_probs_and_rating(player, season, dataD, numClustersD)
    
    for (j in 1:numClustersO) {
      colname <- paste0("O", p, "_", j)
      cluster_col <- paste0("Cluster_", j, "_prob")
      
      if (!is.null(player_data_O[[cluster_col]])) {
        lineup[[colname]][i] <- player_data_O[[cluster_col]]
      }
    }
    for (j in 1:numClustersD) {
      colname <- paste0("D", p, "_", j)
      cluster_col <- paste0("Cluster_", j, "_prob")
      
      if (!is.null(player_data_D[[cluster_col]])) {
        lineup[[colname]][i] <- player_data_D[[cluster_col]]
      }
    }
    
    # add ratings to lineup
    lineup[[rating_col_O]][i] <- player_data_O$"CombinedO/10"
    lineup[[rating_col_D]][i] <- player_data_D$"CombinedD/10"
  }
}

# Sum up the values for each offensive cluster
for (j in 1:numClustersO) {
  cluster_sum_col <- paste0("O_", j)
  lineup[[cluster_sum_col]] <- rowSums(lineup[, paste0("O", 1:5, "_", j)], na.rm = TRUE)
}

# Sum up the values for each defensive cluster
for (j in 1:numClustersD) {
  cluster_sum_col <- paste0("D_", j)
  lineup[[cluster_sum_col]] <- rowSums(lineup[, paste0("D", 1:5, "_", j)], na.rm = TRUE)
}

# =========================================
# SECTION 11: Bayesian Adjustment
# =========================================

lookup_team_rating <- function(season, team, teamRating, type) {
  rating <- teamRating %>%
    filter(Season == season, Team == team) %>%
    select(type) %>%
    pull()
  
  if (length(rating) == 1) {
    return(rating)
  } else {
    return(NA)
  }
}

# Apply Bayesian adjustment for lineups with fewer than 550 possessions
lineup <- lineup %>%
  rowwise() %>%
  mutate(
    team_off_rating = lookup_team_rating(Season, Team, teamRating, "Off:pts/poss"),
    team_def_rating = lookup_team_rating(Season, Team, teamRating, "Def: pts/poss"),
    adjustedOFFRTG = ifelse(
      Poss >= 550,
      `OFFENSE: Pts/Poss`,
      `OFFENSE: Pts/Poss` * (Poss / 550) + team_off_rating * (1 - Poss / 550)
    ),
    adjustedDEFRTG = ifelse(
      Poss >= 550,
      `DEFENSE: Pts/Poss`,
      `DEFENSE: Pts/Poss` * (Poss / 550) + team_def_rating * (1 - Poss / 550)
    )
  ) %>%
  ungroup() %>%
  select(-team_off_rating, -team_def_rating) # Remove temporary columns if needed

lineup$adjustedDIFF <- lineup$adjustedOFFRTG - lineup$adjustedDEFRTG

# Aggregate the cluster weightings to form soft lineups
for (j in 1:numClustersO) {
  cluster_rating_col_O <- paste0("O", j, "_net")
  lineup[[cluster_rating_col_O]] <- rowSums(lineup[, paste0("O", 1:5, "_", j)] * lineup[, paste0("O", 1:5, "_rating")], na.rm = TRUE)
}

for (j in 1:numClustersD) {
  cluster_rating_col_D <- paste0("D", j, "_net")
  lineup[[cluster_rating_col_D]] <- rowSums(lineup[, paste0("D", 1:5, "_", j)] * lineup[, paste0("D", 1:5, "_rating")], na.rm = TRUE)
}

# Extract cleaned offensive and defensive lineup data
lineupCleanOff <- lineup %>%
  select(1:8,102:111,121:130, 118)
lineupCleanDef <- lineup %>%
  select(1:8, 112:117, 131:136, 119)

export <- list(
  dataO = dataO,
  dataD = dataD,
  lineupO = lineupCleanOff,
  lineupD = lineupCleanDef
)
#write_xlsx(export, path = "populatedV10.xlsx")

# Clean up workspace
rm(cluster_col, cluster_sum_col, colname, i, j, p, player, player_col, cluster_rating_col_D,
   cluster_rating_col_O, rating_col_D, rating_col_O, season, numClustersD, numClustersO, 
   adjust_probabilities, lookup_team_rating, update_cluster_probs_and_rating, export)

# =========================================
# SECTION 12: Prepare Predictive Modelling
# =========================================

# Extract lineup features for modelling
lineupBio <- select(lineupCleanOff,c("P1","P2", "P3", "P4", "P5", "Team", "Season", "Poss"))

lineupCleanOff$sumORTG <- rowSums(lineupCleanOff[,19:28 ])
lineupOffensive <- lineupCleanOff[,setdiff(names(lineupCleanOff), names(lineupBio))] %>%
  as.data.table() %>%
  select(-c(11:20))

lineupCleanDef$sumDRTG <- rowSums(lineupCleanDef[,15:20 ])
lineupDefensive <- lineupCleanDef[,setdiff(names(lineupCleanDef), names(lineupBio))] %>%
  as.data.table() %>%
  select(-c(7:12))

# =========================================
# SECTION 12: Perform Offensive Modelling - XGBoost
# =========================================

# Tune model
taskO <- TaskRegr$new(id = "lineupO", backend = lineupOffensive, target = "adjustedOFFRTG")
learnerO <- lrn("regr.xgboost", nrounds = 200, eta = 0.1)
measures <- msrs(c('regr.rmse', 'regr.mape', 'regr.rae'))
cv5<- rsmp("cv", folds = 5)

param_set <- ParamSet$new(list(
  ParamDbl$new("eta", lower = 0.01, upper = 0.3),
  ParamInt$new("max_depth", lower = 3, upper = 10),
  ParamDbl$new("subsample", lower = 0.5, upper = 1),
  ParamInt$new("nrounds", lower = 50, upper = 150),
  ParamDbl$new("colsample_bytree", lower = 0.5, upper = 1)
))

# Define the tuning instance
instance <- TuningInstanceSingleCrit$new(
  task = taskO,
  learner = learnerO,
  resampling = rsmp("repeated_cv", folds = 5, repeats = 5),
  measure = msr("regr.mape"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 150)
)

tuner <- tnr("grid_search", resolution = 5)
tuner$optimize(instance)

# Update learner with optimal params
best_params <- tuner$optimize(instance)$learner_param_vals[[1]]
learnerO$param_set$values <- best_params

# Benchmarking

learners = c(lrns(c("regr.ranger", "regr.lm", "regr.rpart", "regr.svm")), learnerO)
design = benchmark_grid(taskO, learners, cv5)
bmrO = benchmark(design) %>% 
  print()
bmr_resultsO <- bmrO$aggregate(measures) %>% 
  print()

# Run Model
set.seed(100)
split <- partition(taskO, 0.7)
learnerO$train(taskO, split$train)
predictionsO <- learnerO$predict(taskO, split$test)
performanceO <- predictionsO$score(measures) %>% 
  print()

# Plot feature importance
importance <- xgboost::xgb.importance(model = learnerO$model)
xgboost::xgb.plot.importance(importance)

cluster_labels <- c("Offensive Superstar", "Mobile Big", "Rim Crusher", "Facilitator", 
                    "Skilled Big Man", "Glue Guy", "Dynamic Shooter", 
                    "Static 3PT Shooter", "Defensive Specialist", "Secondary Generator")

# Plot feature importance
# Assuming clusters are numbered 1 to 10
names(cluster_labels) <- paste0("O_", 1:10)
importance$Feature <- ifelse(importance$Feature %in% names(cluster_labels), 
                             cluster_labels[importance$Feature], 
                             importance$Feature)
importance$Feature <- gsub("sumORTG", "sum O-BPI", importance$Feature)

ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = rdpu_palette[2]) +  # 
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Feature Importance for Offence Model",
       x = "Feature",
       y = "Importance") +
  scale_y_continuous(breaks = seq(0, max(importance$Importance), by = 0.1)) +  # Increase the number of axis marks
  theme_bw()  # Use the theme_bw for a clean appearance


# =========================================
# SECTION 13: Perform Defensive Modelling - XGBoost
# =========================================

# Tune model
taskD <- TaskRegr$new(id = "lineupD", backend = lineupDefensive, target = "adjustedDEFRTG")
learnerD <- lrn("regr.xgboost", nrounds = 200, eta = 0.1)

param_set <- ParamSet$new(list(
  ParamDbl$new("eta", lower = 0.01, upper = 0.3),
  ParamInt$new("max_depth", lower = 3, upper = 10),
  ParamDbl$new("subsample", lower = 0.5, upper = 1),
  ParamInt$new("nrounds", lower = 50, upper = 150),
  ParamDbl$new("colsample_bytree", lower = 0.5, upper = 1)
))

# Define the tuning instance
instance <- TuningInstanceSingleCrit$new(
  task = taskD,
  learner = learnerD,
  resampling = rsmp("repeated_cv", folds = 5, repeats = 5),
  measure = msr("regr.mape"),
  search_space = param_set,
  terminator = trm("evals", n_evals = 150)
)

tuner <- tnr("grid_search", resolution = 5)
tuner$optimize(instance)

best_params <- tuner$optimize(instance)$learner_param_vals[[1]]
learnerD$param_set$values <- best_params

# Benchmarking
learners = c(lrns(c("regr.ranger", "regr.lm", "regr.rpart", "regr.svm")), learnerD)
design = benchmark_grid(taskD, learners, cv5)
bmrD = benchmark(design) %>% 
  print()
bmr_resultsD <- bmrD$aggregate(measures) %>% 
  print()

# Run model
set.seed(101)
split <- partition(taskD, 0.7)
learnerD$train(taskD, split$train)
predictionsD <- learnerD$predict(taskD, split$test)
performanceD <- predictionsD$score(measures) %>% 
  print()

# Plot feature importance
importance <- xgboost::xgb.importance(model = learnerD$model)
xgboost::xgb.plot.importance(importance)

cluster_labels <- c("Chaser", "Big Helper", "Anchor Big", "Point of Attack", "Wing Stopper", "Off-Ball Defender")

# Assuming clusters are numbered 1 to 10
names(cluster_labels) <- paste0("D_", 1:6)
importance$Feature <- ifelse(importance$Feature %in% names(cluster_labels), 
                             cluster_labels[importance$Feature], 
                             importance$Feature)
importance$Feature <- gsub("sumDRTG", "sum D-BPI", importance$Feature)

ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = rdpu_palette[4]) +  # 
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Feature Importance for Defence Model",
       x = "Feature",
       y = "Importance") +
  scale_y_continuous(breaks = seq(0, max(importance$Importance), by = 0.1)) +  # Increase the number of axis marks
  theme_bw()  # Use the theme_bw for a clean appearance

# Resample
learnerO$importance()
learnerD$importance()
# Use the 5-fold V, calculating the macro average rmse across samples
cv5 <- rsmp("repeated_cv", folds = 5, repeats = 5)
rrO = resample(taskO, learnerO, cv5) %>%
  print()
rrO$aggregate(measures) # aggregate root mean squared error

rrD = resample(taskD, learnerD, cv5) %>%
  print()
rrD$aggregate(measures) # aggregate root mean squared error

rm(cv5, measures, instance, param_set, taskO, taskD, design,
   clustNumD, clustNumO, best_params, tuner, split, rrO, rrD)

# =========================================
# SECTION 14: Residual Analysis
# =========================================

# Prepare residuals analysis
residuals <- predictionsO$truth - predictionsO$response
residuals_df <- data.frame(
  Predicted = predictionsO$response,
  Actual = predictionsO$truth,
  Residuals = residuals
)

# Residuals vs. Predicted Values
ggplot(residuals_df, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted Offensive Lineup Rating ",
       x = "Predicted ORtg*",
       y = "Residuals")+
  theme_bw()

# Compare test vs training
train_predictions <- learnerD$predict(taskD, split$train)
train_performance <- train_predictions$score(measures)
test_performance <- predictionsD$score(measures)

print(train_performance)
print(test_performance)

qqnorm(residuals)
qqline(residuals, col = "red")


# =========================================
#       Synthetic Lineup Prediction

# =========================================
# SECTION 1: Generate Offensive Matrix - 10x5x0.25 + sumORTG
# =========================================

# Function to generate valid rows in parallel
generate_valid_rows_parallel <- function(num_columns = 10, target_sum = 5, step = 0.25) {
  values <- seq(0, 5, by = step)
  valid_rows <- list()
  
  generate_combinations <- function(current_row, remaining_sum, current_column) {
    if (current_column == num_columns + 1) {
      if (remaining_sum == 0) {
        return(list(current_row))
      }
      return(NULL)
    }
    
    result <- list()
    for (value in values) {
      if (value <= remaining_sum) {
        sub_result <- generate_combinations(c(current_row, value), remaining_sum - value, current_column + 1)
        if (!is.null(sub_result)) {
          result <- c(result, sub_result)
        }
      }
    }
    return(result)
  }
  
  numCores <- detectCores()
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Distribute the task across cores
  valid_rows <- foreach(i = seq_along(values), .combine = 'c', .packages = 'foreach') %dopar% {
    result <- list()
    if (values[i] <= target_sum) {
      sub_result <- generate_combinations(c(values[i]), target_sum - values[i], 2)
      if (!is.null(sub_result)) {
        result <- c(result, sub_result)
      }
    }
    result
  }
  
  valid_matrix <- unique(do.call(rbind, valid_rows))
  colnames(valid_matrix) <- paste0("O_", 1:num_columns)
  
  # testing the target variable and datatable parts here
  valid_matrix <- cbind(valid_matrix, pred_adjustedOFFRTG = as.numeric(NA))
  
  return(as.data.table(valid_matrix))
}

# Generate the base matrix A
A <- generate_valid_rows_parallel()

# Values for the new column
new_col_values <- seq(10, 30, by = 5)

# Function to expand matrix A with new column values
expand_matrix_with_new_column <- function(matrix_A, new_col_values) {
  expanded_matrix <- do.call(rbind, lapply(new_col_values, function(val) {
    cbind(matrix_A, val)
  }))
  colnames(expanded_matrix)[ncol(expanded_matrix)] <- "sumORTG"
  return(expanded_matrix)
}

# Expand the matrix
validCombinationsO_DT <- expand_matrix_with_new_column(A, new_col_values)

#write.csv(expanded_matrix, "expanded_matrix.csv", row.names = FALSE)
#rm(generate_valid_rows_parallel, A, new_col_values, expand_matrix_with_new_column)

# =========================================
# SECTION 2: Generate Defensive Matrix - 6x5x0.25 + sumDRTG
# =========================================

# Function to generate valid rows in parallel
generate_valid_rows_parallel <- function(num_columns = 6, target_sum = 5, step = 0.25) {
  values <- seq(0, 5, by = step)
  valid_rows <- list()
  
  generate_combinations <- function(current_row, remaining_sum, current_column) {
    if (current_column == num_columns + 1) {
      if (remaining_sum == 0) {
        return(list(current_row))
      }
      return(NULL)
    }
    
    result <- list()
    for (value in values) {
      if (value <= remaining_sum) {
        sub_result <- generate_combinations(c(current_row, value), remaining_sum - value, current_column + 1)
        if (!is.null(sub_result)) {
          result <- c(result, sub_result)
        }
      }
    }
    return(result)
  }

  # Distribute the task across cores
  valid_rows <- foreach(i = seq_along(values), .combine = 'c', .packages = 'foreach') %dopar% {
    result <- list()
    if (values[i] <= target_sum) {
      sub_result <- generate_combinations(c(values[i]), target_sum - values[i], 2)
      if (!is.null(sub_result)) {
        result <- c(result, sub_result)
      }
    }
    result
  }
  
  stopCluster(cl)
  valid_matrix <- unique(do.call(rbind, valid_rows))
  colnames(valid_matrix) <- paste0("D_", 1:num_columns)
  
  # testing the target variable and datatable parts here
  valid_matrix <- cbind(valid_matrix, pred_adjustedDEFRTG = as.numeric(NA))
  
  return(as.data.table(valid_matrix))
}

# Generate the base matrix A
A <- generate_valid_rows_parallel()

# Values for the new column
new_col_values <- seq(10, 35, by = 5)

# Function to expand matrix A with new column values
expand_matrix_with_new_column <- function(matrix_A, new_col_values) {
  expanded_matrix <- do.call(rbind, lapply(new_col_values, function(val) {
    cbind(matrix_A, val)
  }))
  colnames(expanded_matrix)[ncol(expanded_matrix)] <- "sumDRTG"
  return(expanded_matrix)
}

# Expand the matrix
validCombinationsD_DT <- expand_matrix_with_new_column(A, new_col_values)
#write.csv(expanded_matrix, "expanded_matrix.csv", row.names = FALSE)
#rm(generate_valid_rows_parallel, A, new_col_values, expand_matrix_with_new_column, A, expand_matrix_with_new_column, generate_valid_rows_parallel)

# =========================================
# SECTION 3: Predict Synthetic Data - Offence
# =========================================

syntheticTaskO <- TaskRegr$new(id = "syntheticTaskO", backend = validCombinationsO_DT, target = "pred_adjustedOFFRTG")

# Check if the synthetic task is empty
if (syntheticTaskO$nrow == 0) {
  cat("Synthetic task is empty\n")
} else {
  # Make predictions on the synthetic data
  syntheticPredictions <- learnerO$predict(syntheticTaskO)
  
  # Check for missing predictions
  if (any(is.na(syntheticPredictions$data$response))) {
    cat("Missing predictions detected\n")
  } else {
    # Extract the predictions and add them to the synthetic data grid
    validCombinationsO_DT <- validCombinationsO_DT[, pred_adjustedOFFRTG := syntheticPredictions$data$response] %>% 
      arrange(desc(pred_adjustedOFFRTG))
    
    # Display the first few rows of the combined predictions
    head(validCombinationsO_DT)
  }
}

# =========================================
# SECTION 4: Predict Synthetic Data - Defence
# =========================================

syntheticTaskD <- TaskRegr$new(id = "syntheticTaskD", backend = validCombinationsD_DT, target = "pred_adjustedDEFRTG")

# Check if the synthetic task is empty
if (syntheticTaskD$nrow == 0) {
  cat("Synthetic task is empty\n")
} else {
  # Make predictions on the synthetic data
  syntheticPredictions <- learnerD$predict(syntheticTaskD)
  
  # Check for missing predictions
  if (any(is.na(syntheticPredictions$data$response))) {
    cat("Missing predictions detected\n")
  } else {
    # Extract the predictions and add them to the synthetic data grid
    validCombinationsD_DT <- validCombinationsD_DT[, pred_adjustedDEFRTG := syntheticPredictions$data$response] %>% 
      arrange(pred_adjustedDEFRTG)
    
    # Display the first few rows of the combined predictions
    head(validCombinationsD_DT)
  }
}

rm(syntheticPredictions, syntheticTaskD, syntheticTaskO)

# write.csv(validCombinationsO_DT, "Offence_output.csv")
# write.csv(validCombinationsD_DT, "Defence_output.csv")