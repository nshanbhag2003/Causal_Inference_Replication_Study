---
title: "IPW"
output: html_document
date: "2024-11-30"
---


```{r}

library(dplyr)
library(ggplot2)
library(haven)

data <- read_dta("~/Desktop/DD_AnalysisDataset_NYR.dta")
diff <- read_dta("~/Desktop/DD_AnalysisDataset_Diffed_NYR.dta")
airquality_data <- read_dta("~/Desktop/AirQuality.dta")

```

## IPW Estimates

```{r}

# Ensure binary treatment variable
data <- data %>%
  mutate(
    treat_rfg = if_else(treat_rfg > 0, 1, 0),  
    ozone_max = if_else(ozone_max > 0, ozone_max, NA_real_) 
  ) %>%
  filter(!is.na(ozone_max))  

# Fit the propensity score model
propensity_model <- glm(
  treat_rfg ~ urban + income + TempMax + Rain + treat_rvpI + treat_rvpII + 
    treat_CARB, 
  data = data,
  family = binomial  
)

# Add propensity scores and calculate IPW weights
data <- data %>%
  mutate(
    propensity_score = predict(propensity_model, type = "response"),
    weight = if_else(treat_rfg == 1, 1 / propensity_score, 1 / 
                       (1 - propensity_score))
  )

# Weighted regression to estimate treatment effect
ipw_model <- lm(
  log(ozone_max) ~ treat_rfg + urban + income + TempMax + Rain + treat_rvpI + 
    treat_rvpII + treat_CARB, 
  data = data,
  weights = weight  
)

summary(ipw_model)
plot(ipw_model, which = 1)

```


## Adjusting for confounders 

```{r}

# Ensure binary treatment variable
data <- data %>%
  mutate(
    urban = if_else(urban > 0, 1, 0), 
    ozone_max = if_else(ozone_max > 0, ozone_max, NA_real_)  
  ) %>%
  filter(!is.na(ozone_max))  

# Fit the propensity score model
propensity_model <- glm(
  urban ~ income + TempMax + Rain + treat_rfg + treat_rvpI + treat_rvpII + 
    treat_CARB, 
  data = data,
  family = binomial  
)

# Add propensity scores and calculate IPW weights
data <- data %>%
  mutate(
    propensity_score = predict(propensity_model, type = "response"),
    weight = if_else(urban == 1, 1 / propensity_score, 1 / 
                       (1 - propensity_score))
  )

# Weighted regression to estimate treatment effect
ipw_model_2 <- lm(
  log(ozone_max) ~  urban + income + TempMax + Rain  + treat_rfg +
    treat_rvpI + treat_rvpII + treat_CARB, 
  data = data,
  weights = weight  
)

summary(ipw_model_2)
plot(ipw_model_2, which = 1)

```

