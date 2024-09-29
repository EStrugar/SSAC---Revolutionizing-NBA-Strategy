numCores <- detectCores()
cl <- makeCluster(numCores - 1)
registerDoParallel(cl)


# =========================================
# SECTION 1: Rating distributions across sumRTGs
# =========================================

ggplot(validCombinationsD_DT, aes(x = factor(sumDRTG), y = pred_adjustedDEFRTG)) +
  geom_boxplot(fill = rdpu_palette[4], outlier.shape = NA) +
  labs(title = "Distribution of Predicted Defensive Lineup Ratings by Sum D-BPI",
       x = "Total Defensive Impact Score (Sum D-BPI)",
       y = "Predicted Adjusted Defensive Rating (DRtg*)") +
  theme_bw()  +
  theme(axis.text = element_text(size = 11),  # Adjust size for axis markings
        axis.title = element_text(size = 13))  # Adjust size for axis labels

summary_stats <- validCombinationsD_DT %>%
  group_by(sumDRTG) %>%
  summarise(
    count = n(),
    min = min(pred_adjustedDEFRTG),
    q1 = quantile(pred_adjustedDEFRTG, 0.25),
    median = median(pred_adjustedDEFRTG),
    q3 = quantile(pred_adjustedDEFRTG, 0.75),
    max = max(pred_adjustedDEFRTG),
    iqr = IQR(pred_adjustedDEFRTG),
    range = max(pred_adjustedDEFRTG) - min(pred_adjustedDEFRTG),
    outliers = list(boxplot.stats(pred_adjustedDEFRTG)$out)
  )

# Display the summary
summary_stats
rm(summary_stats)

# =========================================
# SECTION 2: Top and bottom 500 lineups for elite teams
# =========================================

cluster_labelsD <- c("Chaser", "Big Helper", "Anchor Big", "Point of Attack", "Wing Stopper", "Off-Ball Defender")
names(cluster_labelsD) <- paste0("D_", 1:6)

lineups_over_27D <- validCombinationsD_DT %>%  filter(sumDRTG > 27)
top_lineupsD <- lineups_over_27D %>% filter(sumDRTG > 32) %>% head(500)  
bottom_lineupsD <- lineups_over_27D %>% filter(sumDRTG > 32) %>% tail(500)  

top_lineupsD$Performance <- "Top 500"
bottom_lineupsD$Performance <- "Bottom 500"

cluster_distributionD <- bind_rows(top_lineupsD, bottom_lineupsD) %>%
  pivot_longer(cols = starts_with("D_"), names_to = "Cluster", values_to = "Membership") %>% 
  mutate(Cluster = recode(Cluster, !!!cluster_labelsD))%>%
  mutate(Cluster = factor(Cluster, levels = cluster_labelsD))

# Plot the combined data
combinedPlotD <- ggplot(cluster_distributionD, aes(x = Cluster, y = Membership, fill = Performance)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Distribution of Roles Across Top and Bottom Defensive Lineups",
       x = "Role",
       y = "Membership Value") +
  ylim(0, 5) +
  scale_fill_manual(values = c("salmon", "lightgreen")) +
  theme_bw() +
  theme(axis.text = element_text(size = 11),  
        axis.title = element_text(size = 13))  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(combinedPlotD)

# =========================================
# SECTION 3: cluster relationship with overall performance
# =========================================

cluster_combinations <- head(validCombinationsD_DT, 25000) %>%
  group_by(D_2, D_3) %>%  
  #filter(sumDRTG > 32) %>% 
  filter(D_2 + D_3 != 5.0) %>% 
  summarise(avg_DEFRTG = mean(pred_adjustedDEFRTG))

# Visualize with a heatmap
ggplot(cluster_combinations, aes(x = D_2, y = D_3, fill = avg_DEFRTG)) +
  geom_tile() +
  labs(title = "Heatmap of Defensive Ratings by Cluster Combinations",
       x = "Cluster 1 Membership",
       y = "Cluster 2 Membership") +
  scale_fill_distiller(palette = "RdPu")+
  
  theme_bw()

stopCluster(cl)

#rm(cl, numCores, topPerformers, bottomPerformers, combined_y_limits, top_lineups,
   bottom_lineups, cluster_distribution, combinedPlot, cluster_combinations)