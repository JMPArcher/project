## Set up ----
## Load Libraries
library(dplyr)
library(ggplot2)
## Load data
individual <- readr::read_csv(
  here::here("data", "individual.csv")
) %>%
  select(stem_diameter, height, growth_form)

## Subsetting dat
analysis_df <- individual %>% 
  filter(complete.cases(.),
         growth_form != "liana")

## Order growth form levels
gf_levels <- table(analysis_df$growth_form) %>% 
  sort() %>%
  names()

analysis_df <- analysis_df %>%
  mutate(growth_form = factor(growth_form, levels = gf_levels))

analysis_df %>% 
  ggplot(aes(y = growth_form, 
             fill = growth_form, colour = growth_form)) + geom_bar(alpha = 0.5, show.legend = FALSE)

# Figure 2: Violin Plots of stem diameter and height across growth forms
analysis_df %>%
  tidyr::pivot_longer(
    cols = c(stem_diameter, height),
    names_to = "var",
    values_to = "value"
  ) %>%   
  ggplot(aes(x = log(value),
             y = growth_form,
             colour = growth_form, 
             fill = growth_form)) +
  geom_violin(alpha = 0.5, trim = TRUE, show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_grid(~var)

# Relationshp between Continous Variables - Fit overall Linear Model ----
lm_overall <- lm(log(stem_diameter) ~ log(height), data = analysis_df)
lm_overall %>% 
  broom::glance()
lm_overall %>%
  broom::tidy()

analysis_df %>%
  ggplot(aes(x = log(height), y = log(stem_diameter))) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") +
  xlab("Log of Height (m)") + 
  ylab("Log of Stem Diameter") +
  theme_linedraw()

## Fit Linear model with a growth form interaction
lm_growth <- lm(log(stem_diameter) ~ log(height) * growth_form,
                data = analysis_df)
lm_growth %>% 
  broom::glance()
lm_growth %>%
  broom::tidy()

## Plot
analysis_df %>%
  ggplot(aes(x = log(height),
             y = log(stem_diameter),
             colour = growth_form)) +
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm") +
  labs(x ="Log of Height (m)", 
       y = "Log of Stem Diameter (cm)",
       colour = " Growth forms"
       ) +
  theme_linedraw() 
