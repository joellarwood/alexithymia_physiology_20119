apa_f <- function(model, caption = NULL){
  anova(model, 3) %>% 
    broom::tidy() %>% 
    mutate(
      statistic = glue::glue(
        "F({round(NumDF,2)}, {round(DenDF, 2)}) = {round(statistic, 2)}, p = {if_else(p.value > .001, as.character(round(p.value, 3)), '< .001')}")
    ) %>% 
    select(term, statistic) %>% 
    relocate(term, statistic, everything()) %>% 
    kableExtra::kable(caption = caption) %>% 
    kableExtra::kable_styling()
}

apa_lrt <- function(anova,  caption = NULL){
  as_data <- as.data.frame(anova)
  
  as_data %>% 
    rownames_to_column("Model") %>% 
    transmute(
      Model = Model, 
      statistic = glue::glue("X2({Df}) = {round(Chisq, 2)}, {case_when(
      as_data[,ncol(as_data)] < .001 ~ 'p < .001',
      as_data[,ncol(as_data)] > .999 ~ 'p > .999',
      TRUE ~ paste0('p = ', as.character(
          round(
            as_data[,ncol(as_data)], 3)
        )
                             ))}")
    ) %>% 
    knitr::kable('html', caption = caption) %>% 
    kableExtra::kable_styling()
}


apa_post_hoc_means <- function(emmeans, caption = NULL){
  as_data <- as.data.frame(emmeans$emmeans)
  
  as_data %>% 
    transmute(
      `Song target affect` = as_data[,1],
      trend_CI = glue::glue("Est = {round(as_data[,2], 2)}, 95% CI [{round(asymp.LCL, 2)}, {round(asymp.UCL, 2)}]")
    ) %>% 
    kableExtra::kable(caption = caption) %>% 
    kableExtra::kable_styling()
  
}

apa_post_hoc_contrast <- function(emmeans, caption = NULL){
  as_data <- as.data.frame(emmeans$contrast)
  
  as_data %>% 
    transmute(
      contrast = contrast,
      statistic = glue::glue("Est = {round(as_data[,2], 2)}, {case_when(
      p.value < .001 ~ 'p < .001',
      p.value > .999 ~ 'p > .999',
      TRUE ~ paste0('p = ', as.character(
          round(
            p.value, 3)
        )
                             ))}, 95% CI [{round(asymp.LCL, 2)}, {round(asymp.UCL, 2)}]")
    ) %>% 
    kableExtra::kable(caption = caption) %>% 
    kableExtra::kable_styling()
}

apa_trends <- function(emtrends, caption= NULL){
  as_data <- as.data.frame(emtrends$emtrends)
  as_data %>% 
    transmute(
      `Song target affect` = as_data[,1],
      trend_CI = glue::glue("Est = {round(as_data[,2], 2)}, p = {round(p.value, 3)}, 95% CI [{round(asymp.LCL, 2)}, {round(asymp.UCL, 2)}]")
    ) %>% 
    kableExtra::kable(caption = caption) %>% 
    kableExtra::kable_styling()
    
  
}
