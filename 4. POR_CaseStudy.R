# =========================================
# SECTION 1: Hardcode Lillard, McCollum and Nurkic 2022 Contributions
# =========================================

# Extract lineups featuring the offensive contributions (Sum O-BPI and clusters)
O_25 <- validCombinationsO_DT %>%  filter(sumORTG == 25,
                                          O_1 >= 0.75,
                                          O_10 >= 1.25,
                                          O_5 >= 1.0)

# Extract lineups featuring the defensive contributions (Sum O-BPI and clusters)
D_20 <- validCombinationsD_DT %>%  filter(sumDRTG == 20,
                                          D_1 >= 1.0,
                                          D_6 >= 1.0,
                                          D_2 >= 0.25, 
                                          D_3 >= 0.75)
export <- list(
  O = O_25,
  D = D_20,
  PlayerO = dataO,
  PlayerD = dataD
)

#write_xlsx(export, path = "possibleLineups.xlsx")
rm(export)

# =========================================
# SECTION 2: Remove Core Trio's Representation From Possible Lineups
# =========================================

#Subtract Offensive Contribution
adjusted_O <- O_25 %>%
  mutate(
    O_1 = O_1 - 0.75,
    O_10 = O_10 - 1.25,
    O_5 = O_5 - 1.0
  )

#Subtract Defensive Contribution
adjusted_D <- D_20 %>%
  mutate(
    D_1 = D_1 - 1.0,
    D_6 = D_6 - 1.0,
    D_2 = D_2 - 0.25,
    D_3 = D_3 - 0.75
  )

adjusted_O
adjusted_D

# =========================================
# SECTION 3: Extract Combined Clusters Memberships/BPIs For All Possible Players
# =========================================

combinedPlayer <- dataD %>% 
  select(Season, Player, 25:30,8) %>% 
  rename(
    D_1 = 3,
    D_2 = 4,
    D_3 = 5,
    D_4 = 6,
    D_5 = 7,
    D_6 = 8,
    D_BPI = 9
  ) 

new_columns <- dataO %>%
  select(35:44,8) %>% 
  rename(
    O_1 = 1,  
    O_2 = 2,
    O_3 = 3,
    O_4 = 4,
    O_5 = 5,
    O_6 = 6,
    O_7 = 7,
    O_8 = 8,
    O_9 = 9,
    O_10 = 10,
    O_BPI = 11
  )

combinedPlayer <- bind_cols(combinedPlayer, new_columns)
player_combinations <- expand_grid(player1 = combinedPlayer, player2 = combinedPlayer) %>%
  filter(player1$Player != player2$Player ) %>% 
  mutate(
    combined_O_1 = player1$O_1 + player2$O_1,
    combined_O_2 = player1$O_2 + player2$O_2,
    combined_O_3 = player1$O_3 + player2$O_3,
    combined_O_4 = player1$O_4 + player2$O_4,
    combined_O_5 = player1$O_5 + player2$O_5,
    combined_O_6 = player1$O_6 + player2$O_6,
    combined_O_7 = player1$O_7 + player2$O_7,
    combined_O_8 = player1$O_8 + player2$O_8,
    combined_O_9 = player1$O_9 + player2$O_9,
    combined_O_10 = player1$O_10 + player2$O_10,
    combined_D_1 = player1$D_1 + player2$D_1,
    combined_D_2 = player1$D_2 + player2$D_2,
    combined_D_3 = player1$D_3 + player2$D_3,
    combined_D_4 = player1$D_4 + player2$D_4,
    combined_D_5 = player1$D_5 + player2$D_5,
    combined_D_6 = player1$D_6 + player2$D_6,
    OBPI = player1$O_BPI + player2$O_BPI,
    DBPI = player1$D_BPI + player2$D_BPI,
    NetBPI = player1$O_BPI + player2$O_BPI + player1$D_BPI + player2$D_BPI
    #combination_id = pmin(player1$Player, player2$Player) %>% paste0("-", pmax(player1$Player, player2$Player))
  ) 

matching_combinations <- player_combinations %>%
  filter(
    combined_O_1 >= 0,
    combined_O_2 >= 0,
    combined_O_3 >= 0,
    combined_O_4 >= 0,
    combined_O_5 >= 0,
    combined_O_6 >= 0,
    combined_O_7 >= 0,
    combined_O_8 >= 0,
    combined_O_9 >= 0,
    combined_O_10 >= 0,
    combined_D_1 >= 0,
    combined_D_2 >= 0,
    combined_D_3 >= 0,
    combined_D_4 >= 0,
    combined_D_5 >= 0,
    combined_D_6 >= 0
  ) %>% 
  #distinct(combination_id, .keep_all = TRUE) %>%  
  mutate(
    player1_Season = player1$Season,
    player1_Player = player1$Player,
    player2_Season = player2$Season,
    player2_Player = player2$Player
  ) %>%
  filter(player1_Season == 2022,
         player2_Season == 2022) %>% 
  select(
    player1_Season, 
    player1_Player, 
    player2_Season, 
    player2_Player, OBPI, DBPI, NetBPI,
    combined_O_1,combined_O_2,combined_O_3,combined_O_4,combined_O_5,combined_O_6,combined_O_7,combined_O_8,combined_O_9,combined_O_10, combined_D_1,combined_D_2,combined_D_3,combined_D_4,combined_D_5,combined_D_6
  ) %>% 
  print()

# =========================================
# SECTION 4: Explore Impact on Portland's 2022 Starting Lineup
# =========================================

# Extract the predicted ORtg and DRtg for the lineups featuring Lillard, McCollum, Nurkic and each player pairing
combined_with_ratings <- matching_combinations %>%
  left_join(adjusted_O, by = c("combined_O_1" = "O_1","combined_O_2" = "O_2","combined_O_3" = "O_3","combined_O_4" = "O_4",
                               "combined_O_5" = "O_5","combined_O_6" = "O_6","combined_O_7" = "O_7","combined_O_8" = "O_8",
                               "combined_O_9" = "O_9","combined_O_10" = "O_10")) %>%
  left_join(adjusted_D, by = c("combined_D_1" = "D_1", "combined_D_2" = "D_2", "combined_D_3" = "D_3",
                               "combined_D_4" = "D_4", "combined_D_5" = "D_5", "combined_D_6" = "D_6"))

# Calculate the net impact of each pairing and rank the best duos by NetRtg 
final_table <- combined_with_ratings %>%
  mutate(RTG_Difference = pred_adjustedOFFRTG - pred_adjustedDEFRTG) %>%
  arrange(desc(RTG_Difference)) %>% 
  select(player1_Player, player2_Player, OBPI, DBPI, RTG_Difference, pred_adjustedOFFRTG, pred_adjustedDEFRTG, NetBPI) %>%
  print()
