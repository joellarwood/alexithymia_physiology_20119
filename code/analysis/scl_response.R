
#################################################################
##                        Model tonic                        ##
#################################################################

# Load packages -----------------------------------------------------------

library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Linear Mixed-Effects Models using 'Eigen' and S4
library(AICcmodavg) # Model Selection and Multimodel Inference Based on (Q)AIC(c)
source("code/apa_f.r")

# Import Data -------------------------------------------------------------

data <-here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() 

tonic_log <- here::here(
  "data",
  "log.csv"
) %>%
  read_csv() %>%
  mutate(
    pid = as.factor(P)
  ) %>% 
  filter(
    grepl("bad", Eda)
  ) %>%
  select(pid)

tonic <- data %>% 
  anti_join(tonic_log) %>% 
  drop_na(liking, tas_z) %>% 
  filter(abs(tonic) < mean(tonic, na.rm = TRUE) + 3*sd(.$tonic, na.rm = TRUE)) 


message(
  paste0(nrow(data) - nrow(tonic), " Observations removed out of ", nrow(data), " or ", round((nrow(data) - nrow(tonic))/nrow(data)*100, 3), "%")
)

message(
  paste0(length(unique(data$pid)) - length(unique(tonic$pid)), " Observations removed")
)

message(
  paste0("There are ", nrow(tonic), " observations from ", length(unique(tonic$pid)), " participants")
)

# Fit data to null model 

tonic_null <- lmerTest::lmer(
  tonic ~ 1 + (1 | pid),
  data = tonic,
  REML = FALSE
)

tonic_item <- lmerTest::lmer(
  tonic ~ 1 + (1 | pid) + (1 | song),
  data = tonic,
  REML = FALSE
)

tonic_rand <- lmerTest::lmer(
  tonic ~ 1 + (1 + factor | pid) + (1 | song),
  data = tonic,
  REML = FALSE
)

anova(tonic_null, tonic_item, tonic_rand) # random model is prefered

# Random structure is best 


# Model 1: Factor only ----------------------------------------------------

tonic_factor <- update(
  tonic_item, 
  . ~ . + factor, 
  contrasts = list(factor = contr.sum)
)


# Model 2: Liking  --------------------------------------------------------

tonic_liking <- update(
  tonic_factor, 
  . ~ . + liking_z + liking_z : factor
)


# Model 3: Alexithymia ----------------------------------------------------

tonic_tas <- update(
  tonic_liking, 
  . ~ . + tas_z + factor : tas_z
)


# Model 4: Three way ------------------------------------------------------

tonic_all <- update(
  tonic_tas,
  . ~ . + factor : tas_z : liking_z
)


# Compare Models ----------------------------------------------------------

anova(tonic_factor, 
      tonic_liking, 
      tonic_tas, 
      tonic_all) %>% 
  apa_lrt(caption = "Model comparison: tonic")

# Follow up model ---------------------------------------------------------


apa_f(tonic_all, 
      caption = "Type 3 ANOVA table for best fitting tonic model")

## Get main effect

tonic_main <- emmeans::emmeans(tonic_tas, 
                              pairwise ~ factor,
                              infer = TRUE)

apa_post_hoc(tonic_main, caption = "Affect category differences: tonic")

tonic_main %>% apa_post_hoc_contrast()

# Follow up interactions ---------------------------------------------------

## Two way interaction 
### Liking

tonic_liking_emmeans <- emmeans::emtrends(
  tonic_all, 
  pairwise ~ factor,
  var = "liking_z",
  infer = TRUE
)

apa_trends(tonic_liking_emmeans)

tonic_liking_emmeans$contrasts

### Alexithymia

tonic_tas_emmeans <- emmeans::emtrends(
  tonic_all, 
  pairwise ~ factor,
  var = "tas_z",
  infer = TRUE
)

apa_trends(tonic_tas_emmeans)

tonic_tas_emmeans$contrasts

## Three way interaction

tonic_simple_simple <- emmeans::emtrends(
  tonic_all,
  pairwise ~ liking_z | factor,
  at = list(liking_z = c(-1, 0, 1)),
  var = "tas_z",
  infer = TRUE)


tonic_simple_simple$emtrends %>% 
  as.data.frame() %>% 
  arrange(factor, liking_z) %>% 
  distinct(factor, liking_z, .keep_all = TRUE) %>% 
  mutate(
    statistic = glue::glue("trend = {round(tas_z.trend, 2)}, p = {round(p.value, 3)}, 95% CI [{round(asymp.UCL, 2)}, {round(asymp.LCL, 2)}]")
  ) %>% 
  select(factor, liking_z, statistic) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling()


tonic_simple_simple$contrasts %>% 
  as.data.frame() %>% 
  arrange(contrast) %>% 
  tidyr::separate(contrast, 
                  into = c("ref", "contr"),
                  sep = " - ") %>% 
  tidyr::separate(ref,
                  into = c("Liking", "Tas"),
                  sep = "\\s") %>% 
  tidyr::separate(contr,
                  into = c("Liking.c", "Tas.c"),
                  sep = "\\s") %>% 
  arrange(factor, Liking) %>% 
  # select(
  #   contains(c(
  #     "factor",
  #     "Liking",
  #     "Tas",
  #     "p.value")
  #   )
  # ) %>% 
  filter(p.value < .05)

plot_tonic_3way <- emmeans::emmip(
  tonic_all,
  liking_z ~ tas_z | factor, 
  at = list(
    tas_z = c(-1, 0, 1), 
    liking_z = c(-1, 0, 1)
  ),
  plotit = FALSE
) %>% 
  mutate(
    tas_z = as.factor(tas_z),
    factor = recode(factor,
                    "Positive/High" = "Happy", 
                    "Positive/Low" = "Tender",
                    "Negative/High" = "Fear",
                    "Negative/Low" = "Sad"
    )
  ) %>% 
  ggplot2::ggplot(
    aes(
      x = liking_z,
      y = yvar,
      color = tas_z,
      group = tas_z
    )
  ) + 
  geom_path() +
  facet_wrap(~factor)+
  ylim(-.3, .1) +
  labs(
    x = "Song liking (Z transformed)",
    y = "Rated tonic",
    color = "Alexithymia \n(Z transformed)"
  ) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=12))

ggsave("output/plot/tonic_3way.svg", height = 10, width = 15, units = "cm")
