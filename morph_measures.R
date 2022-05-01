library(tidyverse)

morph_measures <- read_csv("morph_measures.csv")

eng_correct <- morph_measures %>%
  filter(language == "English (American)", measure == "stem_correct")

model_linear <- glm(produces ~ age + verbs_prop, data = eng_correct)
model_quadratic <- glm(produces ~ age + verbs_prop + I(verbs_prop^2), data = eng_correct)


model_comparison <- read_feather("data/model_comparison.feather")
write_csv(model_comparison)
ggplot(model_comparison, aes(x = 1 - mean_mse, y = fct_rev(formula_print),
                             colour = language_print)) +
  facet_grid(. ~ measure_print, scales = "free_x") +
  geom_point() +
  geom_point(aes(x = 1 - geo_mean_mse), colour = "black", size = 3,
             data = model_comparison %>% distinct(measure_print, formula_print, geo_mean_mse)) +
  .scale_colour_discrete(name = "") +
  labs(x = "1 - (mean squared prediction error)", y = "Model") +
  theme(legend.position = "top",
        panel.grid.major.y = .coef_line)

ggplot(morph_measures, aes(x = verbs_prop, y = ..count.., fill = produces)) +
  facet_grid(measure ~ language) +
  coord_fixed() +
  geom_density(position = "fill") +
  scale_fill_manual(values = c("white", "slategrey"), guide = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Verb vocabulary (proportion of items)", y = "Conditional density of production")
