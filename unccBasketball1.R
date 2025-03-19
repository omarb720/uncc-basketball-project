library(dplyr)
library(tidyr)
library(caret)
library(readr)
library(devtools)
library(readxl)
library(ggplot2)
library(forcats)  
library(viridis)

uncc_basketball <- data.frame (
  ID = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
  date = c('01/04/2025', '01/08/2025', '01/12/2025', '01/14/2025', '01/19/2025', '01/22/2025', '01/29/2025', '02/1/2025', '02/4/2025', '02/08/2025', '02/10/2025', '02/15/2025', '02/19/2025', '02/26/2025', '03/02/2025', '03/06/2025', '03/09/2025'),
  hometeam = c("Rice", "Charlotte", "Tulsa", "Wichita St", "Charlotte", "Charlotte", "Temple", "UAB", "Charlotte", "Charlotte", "FAU", "Charlotte", "Charlotte", "Tulane", "East Carolina", "North Texas", "Charlotte"),
  awayteam = c("Charlotte", "FAU", "Charlotte", "Charlotte", "Memphis", "South Florida", "Charlotte", "Charlotte", "Wichita St", "Rice", "Charlotte", "East Carolina", "Temple", "Charlotte", "Charlotte", "Charlotte", "UTSA"),
  spread = c(-13, -11, -6, -9, -9, 8, -1, -18, -8, 3, -8, -16, 6, -14, -2, -11, -3),
  cltOutcome = c("L", "L", "L", "L", "L", "W", "L", "L", "L", "W", "L", "L", "W", "L", "L", "L", "L"),
  h1Passes_u16 = c(30, 36, 31,38,52,42,22,32,34,52, 41,54,43,23,26,35,21), #all plays from start of game until under 16 (inlcuding play if during) 
  h1Paint_touches_u16 = c(1, 5, 1,3,6,7,5,5,6,7,8,10,7,1,5,12,5),
  h1Points_u16 = c(6,8,4,5,8,8,5,2,12,6,15,13,11,2,7,10,7),
  h1Reversals_u16 = c(1, 0, 1,0,2,2,0,0,0,0,0,1,0,1,0,0,0),
  h1Trans_passes_u16 = c(0, 2, 1,2,3,0,0,0,1,0,0,0,0,0,1,0,1),
  h1Trans_points_u16 = c(0, 0, 0,0,3,0,0,0,0,0,0,0,0,1,0,0,2),
  h1Passes_u12 = c(40,25, 31,44,11,38,38,25,30,14,38,12,13,  45,26,8,18), #all plays after under 16 aup until under 12 (inlcuding play if during) 
  h1Paint_touches_u12 = c(7,4,2,6,3,6,7,7,4,2,5,1,1,9,5,2,3), 
  h1Points_u12 = c(6,4,2,3,0,6,7,8,8,5,6,2,3,3,7,2,5), 
  h1Reversals_u12 = c(0,1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0),
  h1Trans_passes_u12 = c(1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0),
  h1Trans_points_u12 = c(2,0,2,0,0,0,0,0,0,0,3,0,2,0,0,0,0),
  h1Passes_u8 = c(41,16,17,17,22,28,14,26,30,39,31,47,28,38,20,20,37), #all plays after under 12 aup until under 8 (inlcuding play if during) 
  h1Paint_touches_u8 = c(8,2,4,8,3,5,4,3,3,7,3,5,7,6,4,2,9),
  h1Points_u8 = c(5,4,6,5,3,4,4,5,2,4,9,10,9,10,1,5,9), 
  h1Reversals_u8 = c(1,0,0,0,1,0,0,0,1,0,0,1,0,1,0,0,1),
  h1Trans_passes_u8 = c(0,2,0,0,0,7,0,0,0,0,0,0,2,1,0,0,0),
  h1Trans_points_u8 = c(0,3,0,0,0,3,0,2,2,0,0,0,2,2,0,0,0),
  h1Passes_u4 = c(47 , 14, 22,21,36,37,24,20,48,27,11,17,24,23,31,35,33), #all plays after under 8 up until under 4 
  h1Paint_touches_u4 = c(8, 3,1,9,5,6,1,4,10,5,1,2,4,3,7,5,3),
  h1Points_u4 = c(4, 8, 7,8,4,9,6,6,4,6,0,2,6,4,4,12,5), 
  h1Reversals_u4 = c(2, 0,1,0,1,0,0,0,1,0,0,0,1,0,1,0,0),
  h1Trans_passes_u4 = c(0, 1,0,1,2,0,0,0,3,4,0,2,0,0,1,0,0),
  h1Trans_points_u4 = c(2, 0,2,0,0,0,0,0,3,6,0,0,0,0,0,0,0),
  h1Passes_rest = c(8,23,33,41,23,21,40,23,0,31,12,28,28,24,19,16,16),
  h1Paint_touches_rest = c(2, 7, 4,6,5,3,5,6,0,5,2,4,7,1,4,3,4),
  h1Points_rest = c(0 , 3, 8,6,7,5,14,7,0,5,4,8,5,7,6,7,5), 
  h1Reversals_rest = c(0 , 0, 0,1,0,0,1,0,0,0,0,0,0,0,0,0,0),
  h1Trans_passes_rest = c(0, 0,1,0,0,2,0,0,0,2,0,0,0,0,1,0,2),
  h1Trans_points_rest = c(0, 0,0,0,0,2,0,0,0,3,0,0,0,0,2,0,5),
  h2Passes_u16 = c(28,30,34,29, 35,33,34,36,48,24,22,38,36,32,35,17,26), 
  h2Paint_touches_u16 = c(3,6,7,3,5,3,2,7,4,7,4,8,4,6,6,3,8),
  h2Points_u16 = c(5,8,3,5,11,7,7,11,5,8,4,6,9,5,9,3,8), 
  h2Reversals_u16 = c(0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,0,0),
  h2Trans_passes_u16 = c(0,1,0,0,1,0,0,0,2,4,3,0,0,0,0,0,1),
  h2Trans_points_u16 = c(0,0,0,1,2,0,0,0,0,6,0,0,0,0,0,0,0),
  h2Passes_u12 = c(47,18,36,30,26,27,37,24,23,58,35,42,28,30,28,36,28),
  h2Paint_touches_u12 = c(2, 8,5,3,3,7,8,6,3,6,7,10,11,4,4,4,5),
  h2PointsU12 = c(3,4,14,2,2,4,8,7,6,4,2,2,2,6,3,7,7), 
  h2Reversals_u12 = c(2,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0),
  h2Trans_passes_u12 = c(0,2,3,1,0,0,1,4,0,0,0,0,2,0,0,0,0),
  h2Trans_points_u12 = c(0,0,3,2,3,0,0,5,0,0,4,4,3,0,0,0,0),
  h2Passes_u8 = c(30, 20,16,37,21,33,28,34,31,29,31,15,31,38,29,13,21),
  h2Paint_touches_u8 = c(7,4,2,4,4,7,3,9,5,5,7,3,7,9,7,1,5),
  h2Points_u8 = c(9, 6,0,7,2,8,7,5,3,7,6,2,5,8,10,0,6), 
  h2Reversals_u8 = c(1, 0,0,0,0,0,0,0,1,2,0,0,0,3,1,0,0),
  h2Trans_passes_u8 = c(0,2,2,2,0,0,4,1,0,2,1,1,0,1,5,2,2),
  h2Trans_points_u8 = c(0,0,3,0,0,0,2,0,0,3,2,2,0,0,2,3,3),
  h2Passes_u4 = c(24, 27,20,15,23,23,9,30,34,34,22,29,20,30,20,35,27),
  h2Paint_touches_u4 = c(4, 7, 4,2,3,4,2,8,5,2,8,5,2,3,8,11,4),
  h2Points_u4 = c(9, 7,3,5,7,7,2,12,4,5,10,7,5,3,6,10,12), 
  h2Reversals_u4 = c(0, 0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0),
  h2Trans_passes_u4 = c(0, 0,2,0,1,1,2,0,2,6,1,0,0,0,0,0,0),
  h2Trans_points_u4 = c(0,0,3,0,3,1,2,2,0,3,3,0,0,0,0,0,0),
  h2Passes_rest = c(23, 5,16,33,11,16,92,12,19,20,24,6,45,23,24,12,11), #all plays after under 4 including overtime
  h2Paint_touches_rest = c(3, 3,3,10,3,1,20,3,3,6,5,0,8,7,2,6,4),
  h2Points_rest = c(4, 6,5,10,11,5,22,4,6,7,7,0,14,10,19,5,3), 
  h2Reversals_rest = c(0, 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
  h2Trans_passes_rest = c(0, 1,0,1,1,0,4,0,0,0,0,0,0,3,0,3,3),
  h2Trans_points_rest = c(0, 0,0,0,2,0,3,0,0,0,1,0,2,3,0,3,3),
  oReb = c(7, 8,2,7,4,7,7,4,7,6,5,7,10,6,5,11,11),
  dReb = c(18, 23,19,25,18,30,26,15,20,18,21,14,23,20,21,20,20),
  assist = c(12, 14,15,11,12,15,20,16,11,15,14,9,16,14,12,10,14),
  assist_att = c(14, 18,20,27,24,16,24,20,27,20,17,18,17,24,18,11,21),
  turnovers = c(7, 15, 5,8,11,13,13,5,6,6,15,9,9,10,10,11,11)
)

uncc_basketball$h1Total_passes <- uncc_basketball$h1Passes_u16 + uncc_basketball$h1Passes_u12 + uncc_basketball$h1Passes_u8 + uncc_basketball$h1Passes_u4 + uncc_basketball$h1Passes_rest
uncc_basketball$h1Total_paint_touches <- uncc_basketball$h1Paint_touches_u16 + uncc_basketball$h1Paint_touches_u12 + uncc_basketball$h1Paint_touches_u8 + uncc_basketball$h1Paint_touches_u4 + uncc_basketball$h1Paint_touches_rest
uncc_basketball$h1Total_points <- uncc_basketball$h1Points_u16 + uncc_basketball$h1Points_u12 + uncc_basketball$h1Points_u8 + uncc_basketball$h1Points_u4 + uncc_basketball$h1Points_rest
uncc_basketball$h1Total_reversals <- uncc_basketball$h1Reversals_u16 + uncc_basketball$h1Reversals_u12 + uncc_basketball$h1Reversals_u8 + uncc_basketball$h1Reversals_u4 + uncc_basketball$h1Reversals_rest
uncc_basketball$h1Total_trans_passes <- uncc_basketball$h1Trans_passes_u16 + uncc_basketball$h1Trans_passes_u12 + uncc_basketball$h1Trans_passes_u8 + uncc_basketball$h1Trans_passes_u4 + uncc_basketball$h1Trans_passes_rest
uncc_basketball$h1Total_trans_points <- uncc_basketball$h1Trans_points_u16 + uncc_basketball$h1Trans_points_u12 + uncc_basketball$h1Trans_points_u8 + uncc_basketball$h1Trans_points_u4 + uncc_basketball$h1Trans_points_rest
uncc_basketball$h2Total_passes <- uncc_basketball$h2Passes_u16 + uncc_basketball$h2Passes_u12 + uncc_basketball$h2Passes_u8 + uncc_basketball$h2Passes_u4 + uncc_basketball$h2Passes_rest
uncc_basketball$h2Total_paint_touches <- uncc_basketball$h2Paint_touches_u16 + uncc_basketball$h2Paint_touches_u12 + uncc_basketball$h2Paint_touches_u8 + uncc_basketball$h2Paint_touches_u4 + uncc_basketball$h2Paint_touches_rest
uncc_basketball$h2Total_points <- uncc_basketball$h2Points_u16 + uncc_basketball$h2PointsU12 + uncc_basketball$h2Points_u8 + uncc_basketball$h2Points_u4 + uncc_basketball$h2Points_rest
uncc_basketball$h2Total_reversals <- uncc_basketball$h2Reversals_u16 + uncc_basketball$h2Reversals_u12 + uncc_basketball$h2Reversals_u8 + uncc_basketball$h2Reversals_u4 + uncc_basketball$h2Reversals_rest
uncc_basketball$h2Total_trans_passes <- uncc_basketball$h2Trans_passes_u16 + uncc_basketball$h2Trans_passes_u12 + uncc_basketball$h2Trans_passes_u8 + uncc_basketball$h2Trans_passes_u4 + uncc_basketball$h2Trans_passes_rest
uncc_basketball$h2Total_trans_points <- uncc_basketball$h2Trans_points_u16 + uncc_basketball$h2Trans_points_u12 + uncc_basketball$h2Trans_points_u8 + uncc_basketball$h2Trans_points_u4 + uncc_basketball$h2Trans_points_rest
uncc_basketball$tReb <- uncc_basketball$oReb + uncc_basketball$dReb

uncc_basketball

assist_analysis <- uncc_basketball |>
  group_by(cltOutcome) |>
  summarise(
    avg_assist_ratio = mean(assist / assist_att, na.rm = TRUE),
    .groups = "drop"
  )

assist_analysis

ggplot(assist_analysis, aes(x = factor(cltOutcome), y = avg_assist_ratio, fill = factor(cltOutcome))) +
  geom_bar(stat = "identity") +
  labs(title = "Assist Ratio in Wins vs. Losses",
       x = "Win/Loss", y = "Avg Assist Ratio",
       fill = "Win/Loss") +
  theme_minimal()

charlotte_h1points_avg1 <- uncc_basketball |>
  summarise(
    avg_u16 = mean(h1Points_u16, na.rm = TRUE),
    avg_u12 = mean(h1Points_u12, na.rm = TRUE),
    avg_u8 = mean(h1Points_u8, na.rm = TRUE),
    avg_u4 = mean(h1Points_u4, na.rm = TRUE),
    avg_rest = mean(h1Points_rest, na.rm = TRUE)
  )

charlotte_h1points_avg1

charlotte_h1points_avg_final <- charlotte_h1points_avg1 |>
  pivot_longer(cols = everything(), names_to = "Period", values_to = "AvgPoints") |>
  mutate(Period = fct_reorder(Period, AvgPoints))

# Bar chart
ggplot(charlotte_h1points_avg_final, aes(x = Period, y = AvgPoints)) +
  geom_bar(stat = "identity", fill = 'navy') +
  labs(title = "Charlotte's Average Points by Period (1st Half)",
       x = "Game Period",
       y = "Average Points") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("avg_u12" = "16-12 min(u12)", "avg_u4" = "8-4 min(u4)", "avg_u8" = "12-8 min(u8)", "avg_rest" = "4-End of Half(rest)", "avg_u16" = "20-16 min(u16)"))

charlotte_h2points_avg <- uncc_basketball |>
  summarise(
    avg_u16_h2 = mean(h2Points_u16, na.rm = TRUE),
    avg_u12_h2 = mean(h2PointsU12, na.rm = TRUE),
    avg_u8_h2 = mean(h2Points_u8, na.rm = TRUE),
    avg_u4_h2 = mean(h2Points_u4, na.rm = TRUE),
    avg_rest_h2 = mean(h2Points_rest, na.rm = TRUE)
  )

charlotte_h2points_avg_final <- charlotte_h2points_avg |>
  pivot_longer(cols = everything(), names_to = "Period", values_to = "AvgPoints") |>
  mutate(Period = fct_reorder(Period, AvgPoints))

# Bar chart
ggplot(charlotte_h2points_avg_final, aes(x = Period, y = AvgPoints)) +
  geom_bar(stat = "identity", fill = 'navy') +
  labs(title = "Charlotte's Average Points by Period (2nd Half)",
       x = "Game Period",
       y = "Average Points") +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_x_discrete(labels = c("avg_u12_h2" = "16-12 min(u12)", "avg_u4_h2" = "8-4 min(u4)", "avg_u8_h2" = "12-8 min(u8)", "avg_rest_h2" = "4-End of Half(rest)", "avg_u16_h2" = "20-16 min(u16)"))

# Reshaping the data for the 1st Half
charlotte_h1passes_final <- uncc_basketball |>
  select(h1Passes_u16, h1Passes_u12, h1Passes_u8, h1Passes_u4, h1Passes_rest) |>
  pivot_longer(cols = everything(), names_to = "Period", values_to = "Passes") |>
  mutate(Period = fct_reorder(Period, Passes))

# Box plot for the 1st Half
ggplot(charlotte_h1passes_final, aes(x = Period, y = Passes, fill = Period)) +
  geom_boxplot() +
  labs(title = "Distribution of Passes by Period (1st Half)",
       x = "Game Period",
       y = "Passes") +
  theme_minimal() +
  scale_fill_brewer(palette = "Greens") +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), breaks = seq(0, max(charlotte_h1passes_final$Passes), by = 10)) + 
  scale_x_discrete(labels = c("h1Passes_u12" = "16-12 min(u12)", "h1Passes_u4" = "8-4 min(u4)", "h1Passes_u8" = "12-8 min(u8)", "h1Passes_rest" = "4-End of Half(rest)", "h1Passes_u16" = "20-16 min(u16)"))

# Reshaping the data for the 2nd Half
charlotte_h2passes_final <- uncc_basketball |>
  select(h2Passes_u16, h2Passes_u12, h2Passes_u8, h2Passes_u4, h2Passes_rest) |>
  pivot_longer(cols = everything(), names_to = "Period", values_to = "Passes") |>
  mutate(Period = fct_reorder(Period, Passes))

# Box plot for the 2nd Half
ggplot(charlotte_h2passes_final, aes(x = Period, y = Passes, fill = Period)) +
  geom_boxplot() +
  labs(title = "Distribution of Passes by Period (2nd Half)",
       x = "Game Period",
       y = "Passes") +
  theme_minimal() +
  scale_fill_brewer(palette = "Greens") +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), breaks = seq(0, max(charlotte_h2passes_final$Passes), by = 10)) +
  scale_x_discrete(labels = c("h2Passes_u12" = "16-12 min(u12)", "h2Passes_u4" = "8-4 min(u4)", "h2Passes_u8" = "12-8 min(u8)", "h2Passes_rest" = "4-End of Half(rest)", "h2Passes_u16" = "20-16 min(u16)"))


