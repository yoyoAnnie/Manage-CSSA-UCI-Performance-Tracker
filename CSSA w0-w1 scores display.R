library(tidyverse)

# ---- Raw data entry ----
GradesDF <- tribble(
  ~TeamMember,       ~W0,  ~W1,
  "Shan Yi",          98,   90,
  "Martina Song",     95,   98,
  "Xinlei Liang",     90,   90,
  "Paul Jiang",       90,    0,
  "Yingbo Zhai",      80,   84,
  "Evelyn Hou",       80,   85,
  "Jenny Lin",        80,    0,
  "Yutong Liu",        0,   85,
  "Stella Dai",       80,   85
)

#. Define Task Groups
Communications      <- c("Evelyn Hou", "Jenny Lin", "Yutong Liu", "Stella Dai")
Research            <- c("Shan Yi", "Xinlei Liang", "Paul Jiang")
ExternalRelations   <- c("Martina Song", "Yingbo Zhai")

# Add group column
GradesDF <- GradesDF %>%
  mutate(TaskGroup = case_when(
    TeamMember %in% Communications ~ "Communications",
    TeamMember %in% Research ~ "Research",
    TeamMember %in% ExternalRelations ~ "External Relations",
    TRUE ~ "Other"
  ))
#Convert to Long Format
GradesLong <- GradesDF %>%
  pivot_longer(cols = c(W0, W1),
               names_to = "Week",
               values_to = "Grade") %>%
  mutate(Week = ifelse(Week == "W0", 0, 1))

#Overall Grade Distribution (Density)
ggplot(GradesLong, aes(x = Grade, fill = factor(Week))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("0" = "#4EA5D9", "1" = "#DE4C63"),
                    labels = c("Week 0", "Week 1")) +
  labs(
    title = "Overall Grade Distribution Across Weeks",
    x = "Grade (%)",
    y = "Density",
    fill = "Week"
  ) +
  theme_minimal()
#Grade Distribution by Task Group (Boxplot)
ggplot(GradesLong, aes(x = TaskGroup, y = Grade, fill = factor(Week))) +
  geom_boxplot(position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("0" = "#4EA5D9", "1" = "#DE4C63"),
                    labels = c("Week 0", "Week 1")) +
  labs(
    title = "Grade Distribution by Task Group",
    x = "Task Group",
    y = "Grade (%)",
    fill = "Week"
  ) +
  theme_minimal()

#Faceted “Mini Line Charts” One per Student

ggplot(GradesLong, aes(x = Week, y = Grade)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~ TeamMember) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("W0", "W1")
  ) +
  labs(
    title = "Individual Grade Trajectories (Faceted)",
    x = "Week",
    y = "Grade (%)"
  ) +
  theme_minimal()

#Dumbbell Chart 

DumbbellDF <- GradesDF %>%
  arrange(W1)

ggplot(DumbbellDF, aes(y = reorder(TeamMember, W1))) +
  # line between W0 and W1
  geom_segment(aes(x = W0, xend = W1,
                   yend = TeamMember),
               linewidth = 1.2,
               color = "grey60") +
  # starting point (W0)
  geom_point(aes(x = W0),
             size = 4,
             color = "#4EA5D9") +
  # ending point (W1)
  geom_point(aes(x = W1),
             size = 4,
             color = "#DE4C63") +
  labs(
    title = "Change in Grades from Week 0 to Week 1",
    x = "Grade (%)",
    y = "Team Member"
  ) +
  theme_minimal()


