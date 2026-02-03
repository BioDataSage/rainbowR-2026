
# Bullied at school ---- 

overall_prev <- yrbs %>%
  filter(
    Sexual_identity != "Don t know what this means",
    Transgender != "Don t know what this means",
    !is.na(Bullied_at_school)
  ) %>%
  mutate(
    LGBT = ifelse(
      Sexual_identity %in% c("Heterosexual (straight)", "Not sure") &
        Transgender != "Yes, I am transgender",
      "Heterosexual/Not sure",
      "LGBTQ+"
    )
  ) %>%
  group_by(LGBT) %>%
  summarise(
    prevalence = mean(
      Bullied_at_school == "Yes",
      na.rm = TRUE
    ),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pr_vs_nonLGBT = prevalence / prevalence[LGBT == "Heterosexual/Not sure"]
  )

overall_prev


## by age ------ 

age_prev <- yrbs_filter %>%
  filter(
    !is.na(age),
    !is.na(bullied)
  ) %>%
  group_by(age, LGBTQ) %>%
  summarise(
    prevalence = mean(bullied == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(age) %>%
  mutate(
    prev_nonLGBT = prevalence[LGBTQ == "Heterosexual/Not sure"],
    pr_vs_nonLGBT = prevalence / prev_nonLGBT
  ) %>%
  ungroup()

age_prev

# Keep only LGBTQ+ row (since non-LGBT is always PR = 1 baseline)
plot_df <- age_prev %>%
  filter(LGBTQ == "LGBTQ+") %>%
  filter(!is.na(pr_vs_nonLGBT))

ggplot(plot_df,
       aes(x = age,
           y = pr_vs_nonLGBT)) +
  geom_col(fill="skyblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    title = "Prevalence Ratio of Being Bullied at School",
    subtitle = "LGBTQ+ students compared to Heterosexual/Not sure students, within each age group",
    x = "Age",
    y = "Prevalence Ratio (PR)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


## by race ------ 


race_prev <- yrbs %>%
  filter(
    Sexual_identity != "Don t know what this means",
    Transgender != "Don t know what this means",
    !is.na(Race_Ethnicity),
    !is.na(Bullied_at_school)
  ) %>%
  mutate(
    LGBT = ifelse(
      Sexual_identity %in% c("Heterosexual (straight)", "Not sure") &
        Transgender != "Yes, I am transgender",
      "Heterosexual/Not sure",
      "LGBTQ+"
    )
  ) %>%
  group_by(Race_Ethnicity, LGBT) %>%
  summarise(
    prevalence = mean(Bullied_at_school == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Race_Ethnicity) %>%
  mutate(
    prev_nonLGBT = prevalence[LGBT == "Heterosexual/Not sure"],
    pr_vs_nonLGBT = prevalence / prev_nonLGBT
  ) %>%
  ungroup()

race_prev

# Keep only LGBTQ+ row (since non-LGBT is always PR = 1 baseline)
plot_df <- race_prev %>%
  filter(LGBT == "LGBTQ+") %>%
  filter(!is.na(pr_vs_nonLGBT))

# Bar plot of PR by race
ggplot(plot_df,
       aes(x = reorder(Race_Ethnicity, pr_vs_nonLGBT),
           y = pr_vs_nonLGBT)) +
  geom_col(fill="skyblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    title = "Prevalence Ratio of Being Bullied at School",
    subtitle = "LGBTQ+ students compared to Heterosexual/Not sure students, within each race group",
    x = "Race/Ethnicity",
    y = "Prevalence Ratio (PR)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Bar plot of PR by race
ggplot(plot_df,
       aes(x = reorder(In_what_grade_are_you, pr_vs_nonLGBT),
           y = pr_vs_nonLGBT)) +
  geom_col(fill="skyblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    title = "Prevalence Ratio of Being Bullied at School",
    subtitle = "LGBTQ+ students compared to Heterosexual/Not sure students",
    x = "Race/Ethnicity",
    y = "Prevalence Ratio (PR)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## negative mental health -----
# (During the past 30 days, how often was your mental health not good?
# (Poor mental health includes stress, anxiety, and depression.)

mh_prev <- yrbs %>%
  filter(
    Sexual_identity != "Don t know what this means",
    Transgender != "Don t know what this means",
    !is.na(Current_mental_health),
    !is.na(Bullied_at_school)
  ) %>%
  mutate(
    LGBT = ifelse(
      Sexual_identity %in% c("Heterosexual (straight)", "Not sure") &
        Transgender != "Yes, I am transgender",
      "Heterosexual/Not sure",
      "LGBTQ+"
    )
  ) %>%
  group_by(Current_mental_health, LGBT) %>%
  summarise(
    prevalence = mean(Bullied_at_school == "Yes", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Current_mental_health) %>%
  mutate(
    prev_nonLGBT = prevalence[LGBT == "Heterosexual/Not sure"],
    pr_vs_nonLGBT = prevalence / prev_nonLGBT
  ) %>%
  ungroup()

mh_prev


plot_df_mh <- mh_prev %>%
  filter(LGBT == "LGBTQ+") %>%
  filter(!is.na(pr_vs_nonLGBT))

plot_df_mh


ggplot(plot_df_mh,
       aes(x = reorder(Current_mental_health, pr_vs_nonLGBT),
           y = pr_vs_nonLGBT)) +
  geom_col(fill = "skyblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    title = "Prevalence Ratio of Being Bullied at School (by Mental Health)",
    subtitle = "LGBTQ+ students compared to Heterosexual/Not sure students",
    x = "Current Mental Health",
    y = "Prevalence Ratio (PR)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

