numCores <- detectCores()
cl <- makeCluster(numCores - 1)
registerDoParallel(cl)


# =========================================
# SECTION 1: Rating distributions across sumRTGs
# =========================================

# Plot the distribution of pred_adjustedDEFRTG across sumDRTG
ggplot(validCombinationsO_DT, aes(x = factor(sumORTG), y = pred_adjustedOFFRTG)) +
  geom_boxplot(fill = rdpu_palette[2], outlier.shape = NA) +
  labs(title = "Distribution of Predicted Offensive Lineup Ratings by Sum O-BPI",
       x = "Total Offensive Impact Score (Sum O-BPI)",
       y = "Predicted Adjusted Offensive Rating (ORtg*)") +
  theme_bw()  +
  theme(axis.text = element_text(size = 11),  # Adjust size for axis markings
        axis.title = element_text(size = 13))  # Adjust size for axis labels

summary_stats <- validCombinationsO_DT %>%
  group_by(sumORTG) %>%
  summarise(
    count = n(),
    min = min(pred_adjustedOFFRTG),
    q1 = quantile(pred_adjustedOFFRTG, 0.25),
    median = median(pred_adjustedOFFRTG),
    q3 = quantile(pred_adjustedOFFRTG, 0.75),
    max = max(pred_adjustedOFFRTG),
    iqr = IQR(pred_adjustedOFFRTG),
    range = max(pred_adjustedOFFRTG) - min(pred_adjustedOFFRTG),
    outliers = list(boxplot.stats(pred_adjustedOFFRTG)$out)
  )

# Display the summary
summary_stats
rm(summary_stats)

# =========================================
# SECTION 2: Top and bottom 500 lineups for elite teams
# =========================================

cluster_labels <- c("Offensive Superstar", "Mobile Big", "Rim Crusher", "Facilitator", 
                    "Skilled Big Man", "Glue Guy", "Dynamic Shooter", 
                    "Static 3PT Shooter", "Defensive Specialist", "Secondary Generator")

# Assuming clusters are numbered 1 to 10
names(cluster_labels) <- paste0("O_", 1:10)

lineups_over_24 <- validCombinationsO_DT %>%  filter(sumORTG > 24)
top_lineupsO <- lineups_over_24 %>% filter(sumORTG > 27) %>% head(500)  
bottom_lineupsO <- lineups_over_24 %>% filter(sumORTG > 27) %>% tail(500)  

top_lineupsO$Performance <- "Top 500"
bottom_lineupsO$Performance <- "Bottom 500"

cluster_distributionO <- bind_rows(top_lineupsO, bottom_lineupsO) %>%
  pivot_longer(cols = starts_with("O_"), names_to = "Cluster", values_to = "Membership") %>% 
  mutate(Cluster = recode(Cluster, !!!cluster_labels))%>%
  mutate(Cluster = factor(Cluster, levels = cluster_labels))

# Plot the combined data
combinedPlotO <- ggplot(cluster_distributionO, aes(x = Cluster, y = Membership, fill = Performance)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Distribution of Roles Across Top and Bottom Offensive Lineups",
       x = "Role",
       y = "Membership Value") +
  ylim(0, 3) +
  scale_fill_manual(values = c("salmon", "lightgreen")) +
  theme_bw() +
  theme(axis.text = element_text(size = 11),  
        axis.title = element_text(size = 13))  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(combinedPlotO)

# =========================================
# SECTION 3: cluster relationship with overall performance
# =========================================

cluster_combinationsO <- lineups_over_24 %>%
  filter(sumORTG > 27) %>% 
  group_by(O_10, O_2) %>%
  filter(O_10 + O_2 != 5.0) %>% 
  summarise(avg_OFFRTG = mean(pred_adjustedOFFRTG))

# Visualize with a heatmap
ggplot(cluster_combinationsO, aes(x = O_10, y = O_2, fill = avg_OFFRTG)) +
  geom_tile() +
  labs(title = "Heatmap of Lineup Ratings by Cluster Combinations",
       x = "Superstar ",
       y = "Glue Guy Membership") +
  scale_fill_distiller(palette = "RdPu", direction = 1)+
  theme_bw()

stopCluster(cl)

#rm(cl, numCores, topPerformers, bottomPerformers, combined_y_limits, top_lineups,
#   bottom_lineups, cluster_distribution, combinedPlot, cluster_combinations)