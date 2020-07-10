## ----setup, include=FALSE, cache=FALSE----------------------------------------
library(knitr)
library(feather)
library(tidyverse)
library(glue)
library(magrittr)
library(directlabels)
library(ggstance)
library(cowplot)

.font <- "Source Sans Pro"
theme_set(langcog::theme_mikabr(base_family = .font))
# theme_update(plot.margin = margin(0, 0, 2, 0, "pt"),
#              legend.margin = margin(0, 0, 0, 0, "pt"))
.grey <- "grey70"
.refline <- "dotted"
.coef_line <- element_line(colour = .grey, size = 0.1)
.scale_colour_discrete <- ggthemes::scale_colour_ptol
.scale_fill_discrete <- ggthemes::scale_fill_ptol
.scale_colour_continuous <- viridis::scale_colour_viridis
.scale_fill_continuous <- viridis::scale_fill_viridis
.pal <- ggthemes::ptol_pal()

opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE, echo = FALSE)


## ----load_morph_measures------------------------------------------------------
format_languages <- function(languages) {
  languages %>% fct_reorder(str_length(.))
}

morph_measure_labels <- c(
  "Stem only" = "stem_only",
  "Stem + correct" = "stem_correct",
  "Stem + overregularized" = "stem_overreg"
)

format_measures <- function(measures) {
  measures %>%
    as_factor() %>%
    fct_relevel(morph_measure_labels) %>%
    fct_recode(!!!morph_measure_labels) %>%
    fct_relabel(~str_replace(., " \\(", "\n\\("))
}

format_formulas <- function(formulas) {
  fac_formulas <- formulas %>% as_factor()
  formatted_levels <- levels(fac_formulas) %>%
    str_remove("\\(1 \\| stem\\)") %>%
    str_remove(" \\+ $") %>%
    str_replace_all("verbs_prop", "verbs") %>%
    str_replace_all("\\^2", "²") %>%
    str_replace("^$", "1")
  fac_formulas %>%
    fct_recode(!!!set_names(levels(fac_formulas), formatted_levels))
}

format_terms <- function(terms) {
  terms %>%
    str_replace_all("verbs_prop", "verbs") %>%
    str_replace_all(" \\^ 2", "²") %>%
    fct_reorder(str_length(.)) %>%
    fct_relevel("(Intercept)")
}

kid_info <- read_feather("data/kid_info.feather")

morph_measures <- read_feather("data/morph_measures_inflecting.feather") %>%
  mutate(language_print = format_languages(language),
         measure_print = format_measures(measure)) %>%
  left_join(kid_info)


## ----sample_sizes-------------------------------------------------------------
sample_sizes <- morph_measures %>%
  distinct(language, data_id, stem) %>%
  group_by(language, stem) %>%
  count(name = "num_kids") %>%
  group_by(language, num_kids) %>%
  count(name = "num_stems") %>%
  ungroup() %>%
  arrange(num_stems)
sample_sizes %>% kable()


## ----plot_morph_measures, fig.width=8, fig.height=6---------------------------
ggplot(morph_measures, aes(x = verbs_prop, y = ..count.., fill = value)) +
  facet_grid(measure_print ~ language_print) +
  coord_fixed() +
  geom_density(position = "fill") +
  scale_fill_manual(values = c("white", .pal(1)), guide = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Verb vocabulary (proportion of items)", y = "Conditional density of production")


## ----plot_models_mse, fig.width=7, fig.height=7-------------------------------
model_comparison <- read_feather("data/model_comparison.feather") %>%
  mutate(language_print = format_languages(language),
         measure_print = format_measures(measure),
         formula_print = format_formulas(formula) %>% fct_reorder(dof))

best_models <- read_feather("data/best_models/best_models.feather") %>%
  mutate(language_print = format_languages(language),
         measure_print = format_measures(measure),
         formula_print = format_formulas(formula))
best_models_mse <- read_feather("data/best_models/best_models_mse.feather") %>%
  mutate(language_print = format_languages(language),
         measure_print = format_measures(measure),
         formula_print = format_formulas(formula))

ggplot(model_comparison, aes(x = 1 - mean_mse, y = fct_rev(formula_print),
                             colour = language_print)) +
  facet_grid(. ~ measure_print, scales = "free_x") +
  geom_point() +
  geom_point(aes(x = 1 - geo_mean_mse), colour = "black", size = 3,
             data = model_comparison %>% distinct(measure_print, formula_print, geo_mean_mse)) +
  # geom_point(size = 5, colour = "black", shape = 21, data = best_mse_models) +
  .scale_colour_discrete(name = "") +
  labs(x = "1 - (mean squared prediction error)", y = "Model") +
  theme(legend.position = "top",
        panel.grid.major.y = .coef_line)

# mse_geo_means_print <- mse_geo_means %>%
#   ungroup() %>%
#   mutate(language_print = format_languages(language),
#          measure_print = format_measures(measure),
#          formula_print = format_formulas(formula) %>% fct_reorder(dof))
# 
# ggplot(mse_geo_means_print, aes(x = 1 - mse, y = fct_rev(formula_print))) +
#   facet_grid(. ~ measure_print, scales = "free_x") +
#   # geom_point() +
#   ggridges::geom_density_ridges(aes(fill = language_print), alpha = 0.3, colour = .grey,
#                                 rel_min_height = 0.03, scale = 0.8) +
#   geom_point(aes(x = 1 - geo_mean_mse), colour = "black", size = 3,
#              data = mse_geo_means_print %>%
#                distinct(measure_print, formula_print, geo_mean_mse)) +
#   # geom_point(size = 5, colour = "black", shape = 21, data = best_mse_models) +
#   .scale_fill_discrete(name = "") +
#   labs(x = "1 - (mean squared prediction error)", y = "Model") +
#   theme(legend.position = "top",
#         panel.grid.major.y = .coef_line)


## ----demo_age_plots-----------------------------------------------------------
best_coefs <- read_feather("data/best_models/best_coefs.feather")
plot_coefs <- best_coefs %>%
  mutate(significant = if_else(p.value < 0.05, "*", ""),
         language_print = format_languages(language),
         term_print = format_terms(term) %>% fct_rev()) %>%
  filter(term != "(Intercept)", dataset == "inflecting")

demo_predictions_age <- read_feather("data/demo/demo_predictions_age.feather") %>%
  mutate(age_print = paste(age, "mos"),
         language_print = format_languages(language),
         measure_print = format_measures(measure))

plot_demo_coefs <- function(meas, effect = "all") {

  measure_coefs <- plot_coefs %>% filter(measure == meas)
  if (effect == "main")  measure_coefs %<>%
      filter(str_detect(term, "verb"), str_detect(term, "&", negate = TRUE))
  else measure_coefs %<>% filter(str_detect(term, "age"))
  
  ggplot(measure_coefs, aes(x = estimate, y = term_print)) +
    facet_grid(. ~ language_print) +
    geom_vline(xintercept = 0, linetype = .refline, colour = .grey) +
    geom_pointrangeh(aes(xmin = estimate - 1.96 * std.error,
                         xmax = estimate + 1.96 * std.error),
                     position = position_dodgev(height = 0.5)) +
    labs(x = "Coefficient estimate (log odds ratio)", y = "")

}

plot_demo_fits <- function(meas, effect = "all") {
  
  measure_age_fits <- demo_predictions_age %>% filter(measure == meas)
  if (effect == "main") measure_age_fits %<>% filter(age == min(age))
  
  ggplot(measure_age_fits, aes(x = verbs, y = .response, colour = age_print)) +
    facet_grid(. ~ language_print, scales = "free_y") +
    geom_line() +
    geom_dl(aes(label = age_print),
            method = list("last.qp", dl.trans(x = x + 0.15), fontfamily = .font, cex = 0.7)) +
    expand_limits(x = 130) +
    scale_x_continuous(breaks = c(10, 40, 70, 100)) +
    .scale_colour_discrete(guide = FALSE) +
    labs(x = "Verb vocabulary size",
         y = "Probability of producing")
}

demo_width <- 9.5
demo_height <- 5


## ---- dependson="demo_age_plots", fig.width=demo_width, fig.height=demo_height----
# plot_demo_age("stem_only", "main")

plot_demo_fits("stem_only", "main")
plot_demo_coefs("stem_only", "main")
plot_demo_fits("stem_only")
plot_demo_coefs("stem_only")



## ---- dependson="demo_age_plots", fig.width=demo_width, fig.height=demo_height----
# plot_demo_age("stem_only")


## ---- dependson="demo_age_plots", fig.width=demo_width, fig.height=demo_height----
# plot_demo_age("stem_correct", "main")


## ---- dependson="demo_age_plots", fig.width=demo_width, fig.height=demo_height----
# plot_demo_age("stem_correct")


## ---- dependson="demo_age_plots", fig.width=demo_width, fig.height=demo_height----
# plot_demo_age("stem_overreg", "main")


## ---- dependson="demo_age_plots", fig.width=demo_width, fig.height=demo_height----
# plot_demo_age("stem_overreg")


## ---- eval=FALSE--------------------------------------------------------------
## slopes_ranefs <- read_feather("data/slopes_ranefs.feather")
## 
## slopes_ranefs_plot <- slopes_ranefs %>%
##   select(-formula) %>%
##   mutate(language_print = format_languages(language),
##          measure_print = format_measures(measure),
##          term_print = format_terms(term)) #%>%
##   # filter(term != "(Intercept)")
## 
## slopes_ranefs_measure <- slopes_ranefs_plot %>%
##   select(-measure_print) %>%
##   spread(measure, estimate)
## 
## # slopes_ranefs_measure %>%
## #   group_by(language, term) %>%
## #   summarise(cor_so_sc = cor(stem_only, stem_correct),
## #             cor_so_sov = cor(stem_only, stem_overreg),
## #             cor_sc_sov = cor(stem_correct, stem_overreg)) %>%
## #   arrange(term)
## 
## ggplot(slopes_ranefs_measure, #%>% filter(language == "English (American)"),
##        aes(x = stem_correct, y = stem_overreg)) +
##   facet_grid(language ~ term_print, scales = "free") +
##   geom_vline(xintercept = 0, linetype = .refline, colour = .grey) +
##   geom_hline(yintercept = 0, linetype = .refline, colour = .grey) +
##   geom_text(aes(label = stem)) +
##   geom_smooth(method = "lm")
##   # labs(x = "Verbs coefficient (log odds ratio)",
##   #      y = "Age coefficient (log odds ratio")
## 
## slopes_ranefs_term <- slopes_ranefs_plot %>%
##   select(-term_print) %>%
##   spread(term, estimate)
## slopes_ranefs_term %>%
##   group_by(language, measure) %>%
##   summarise(cor_age_verbs = cor(age, verbs_prop)) %>%
##   arrange(measure)
## 
## ranefs_eng <- slopes_ranefs_plot %>%
##   filter(language == "English (American)", measure == "stem_overreg") %>%
##   arrange(term, estimate) %>%
##   mutate(order = factor(1:n()))
## ggplot(ranefs_eng, aes(x = estimate, y = order)) +
##   facet_wrap(~term, nrow = 1, scales = "free") +
##   geom_point() +
##   scale_y_discrete(breaks = ranefs_eng$order, labels = ranefs_eng$stem)


## ---- eval=FALSE--------------------------------------------------------------
## morph_by_stem <- morph_measures %>%
##   group_by(language, measure, age, stem) %>%
##   summarise(prop = mean(value), total = n()) %>%
##   ungroup() #%>%
##   # spread(measure, prop)
## 
## ggplot(morph_by_stem, #%>% filter(language == "English (American)"),
##        aes(x = age, y = stem_overreg)) +
##   facet_grid(. ~ language, scales = "free") +
##   # geom_vline(xintercept = 0, linetype = .refline, colour = .grey) +
##   # geom_hline(yintercept = 0, linetype = .refline, colour = .grey) +
##   # geom_text(aes(label = stem)) +
##   # geom_smooth(method = "lm")
##   # geom_line(aes(group = stem)) +
##   geom_smooth(aes(group = stem), method = "glm", method.args = list(family = "binomial"), se = FALSE)
## 
## stem_models <- morph_measures %>%
##   group_by(language, measure, stem) %>%
##   nest() %>%
##   ungroup() %>%
##   mutate(stem_model = map(data, function(stem_data) {
##     glm(value ~ age + verbs_prop, family = "binomial", data = stem_data)  #+ I(verbs_prop^2)
##   }))
## 
## stem_effects <- stem_models %>%
##   mutate(coefs = map(stem_model, broom::tidy)) %>%
##   select(language, measure, stem, coefs) %>%
##   unnest(cols = coefs) %>%
##   select(language, measure, stem, term, indep_estimate = estimate) %>%
##   group_by(language, measure, term) %>%
##   mutate(indep_estimate = indep_estimate - mean(indep_estimate))
## 
## stem_compare <- slopes_ranefs %>%
##   select(language, measure, stem, term, ranef_estimate = estimate) %>%
##   left_join(stem_effects)
## 
## stem_compare %>%
##   group_by(language, measure, term) %>%
##   summarise(cor = cor(indep_estimate, ranef_estimate))
## 
## ggplot(stem_compare %>% filter(language == "English (American)", term == "age"),
##        aes(x = indep_estimate, y = ranef_estimate)) +
##   # facet_grid(term ~ measure, scales = "free") +
##   facet_wrap(~measure, nrow = 1) +
##   coord_equal() +
##   geom_text(aes(label = stem))
## 
## stem_effects %>%
##   spread(measure, indep_estimate) %>%
##   group_by(language, term) %>%
##   summarise(cor_sc_sov = cor(stem_correct, stem_overreg))
## 
## slopes_ranefs %>%
##   select(language, measure, stem, term, ranef_estimate = estimate) %>%
##   spread(measure, ranef_estimate) %>%
##   group_by(language, term) %>%
##   summarise(cor_sc_sov = cor(stem_correct, stem_overreg))
## 
## stem_effects %>%
##   spread(term, indep_estimate) %>%
##   group_by(language, measure) %>%
##   summarise(cor_age_verbs = cor(age, verbs_prop))
## 
## slopes_ranefs %>%
##   select(language, measure, stem, term, ranef_estimate = estimate) %>%
##   spread(term, ranef_estimate) %>%
##   group_by(language, measure) %>%
##   summarise(cor_age_verbs = cor(age, verbs_prop))
## 


## ---- eval=FALSE--------------------------------------------------------------
## vocab_compare <- all_coefs %>%
##   filter(#str_detect(term, "verbs"),
##     term == "age",
##          str_detect(term, "&", negate = TRUE),
##          str_detect(formula, "2"), dataset == "inflecting") %>%
##   select(language, measure, formula, term, estimate, std.error) %>%
##   mutate(language_print = format_languages(language),
##          measure_print = format_measures(measure),
##          formula_print = format_formulas(formula),
##          term_print = format_terms(term))
## 
## ggplot(vocab_compare, aes(x = estimate, y = term_print, colour = formula_print)) +
##   facet_grid(measure_print ~ language_print) +
##   geom_vline(xintercept = 0, colour = .grey, linetype = .refline) +
##   geom_point() +
##   .scale_colour_discrete()


## -----------------------------------------------------------------------------
morph_measures_inflecting <- read_feather("data/morph_measures_inflecting.feather")
morph_measures_overregularizing <- read_feather("data/morph_measures_overregularizing.feather")

morph_by_kid <- morph_measures_overregularizing %>%
  group_by(language, measure, data_id, age, verbs_prop) %>%
  summarise(num_true = sum(value), total = n(), prop = num_true / total) %>%
  filter(str_detect(language, "Aus", negate = TRUE))

morph_by_kid %>%
  select(-num_true) %>%
  spread(measure, prop) %>%
ggplot(aes(x = stem_correct, y = stem_overreg)) +
  facet_grid(. ~ language) +
  geom_density_2d()
  # geom_jitter() +
  # geom_smooth()

morph_measures_overregularizing %>%
  filter(str_detect(language, "Aus", negate = TRUE)) %>%
  group_by(language, measure, stem) %>%
  summarise(prop = mean(value)) %>%
  spread(measure, prop) %>%
  ggplot(aes(x = stem_correct, y = stem_overreg)) +
  facet_grid(. ~ language) +
  geom_text(aes(label = stem)) +
  geom_smooth(method = "lm", se = FALSE)

