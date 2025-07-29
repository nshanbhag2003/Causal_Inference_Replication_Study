---
title: "table1"
output: html_document
date: "2024-12-03"
---

```{r}
library(haven)
library(dplyr)

data <- read_dta("DD_AnalysisDataset.dta")

```

table 1
```{r}

years <- function(yr){
  t1<- data %>%
    filter(!is.na(epa_8hr) & epa_8hr < 0.25 & month %in% c(6, 7, 8))
  sample<- t1[t1$year == yr, ]
  
  observation <- t1 %>%
    filter(year == yr) %>%
    nrow()
  
  counties <- length(unique(sample$fips))
  
  total <- length(unique(sample$panelid))
  urban <- length(sample %>%
    filter(urban == 1) %>%
    pull(panelid) %>%
    unique())
  
  rural <-length(sample %>%
    filter(urban == 3) %>%
    pull(panelid) %>%
    unique())

  RVP1 <- length(sample %>%
    filter(treat_rvpI == 1) %>%
    pull(fips) %>%
    unique())
  
  RVP2 <- length(sample %>%
                   filter(treat_rvpII == 1) %>%
                   pull(fips) %>%
                   unique())
  RFG95 <- length(unique_fips <- sample %>%
                    filter(treat_rfg == 1) %>%
                    pull(fips) %>%
                    unique())
  carb <- length(unique_fips <- sample %>%
                   filter(treat_CARB == 1) %>%
                   pull(fips) %>%
                   unique())
  
  index <- c(year = yr, observation, counties, total, urban, rural, RVP1, RVP2, RFG95, carb)
  return(index)
}
results <- lapply(1989:2003, years)
columns <- c("year", "observation", "counties", "total", "urban", "rural", "RVP1", "RVP2", "RFG95", "CARB")
final_df <- data.frame(do.call(rbind, results))
colnames(final_df) <- columns
print(final_df)

```

Total & Average
```{r}
sum(final_df$observation) #1144025
round(sum(final_df$observation) /15) #76268
round(sum(final_df$counties) /15) #471
round(sum(final_df$total) /15) #854
round(sum(final_df$urban)/15) #166
round(sum(final_df$rural) /15) #322

table1 <- rbind(
  final_df, c("Total",sum(final_df$observation), NA, "","","","", "","",""),
  c("Average",
    round(sum(final_df$observation)/15),
    round(sum(final_df$observation)),
    round(sum(final_df$counties) / 15),
    round(sum(final_df$total) / 15),
    round(sum(final_df$urban) / 15),
    round(sum(final_df$rural) / 15), "", "","",""
  )
)

print(table1)
```


