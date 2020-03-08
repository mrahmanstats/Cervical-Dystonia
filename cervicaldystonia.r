---
title: "Cervical Dystonia"
author: "Mohammed Rahman"
date: "February 8, 2020"
output:
  pdf_document: default
  html_document: default
---
```{r loading libraries, message=FALSE, warning=FALSE, paged.print=FALSE}
#Loading Libraries
library(haven)
library(data.table)
library(tidyverse)
library(skimr)
library(gridExtra)
library(gtsummary)
library(expss)
```

```{r loading in data}
#Loading in Data
cerv <- read_dta("cdystonia.dta")

glimpse(cerv)

head(cerv)

#Mutating the ID variable to capture the Site Location
cerv_dyst <- cerv %>%
             mutate(id = paste0(site, id)) %>%
             select(-site)

glimpse(cerv_dyst)

#Mutating the sex variable to numeric, Female = 1
cerv_dyst <- cerv_dyst %>%
             mutate(sex = as.numeric(sex)) %>%
             apply_labels(treat = "Treatment", sex = "Sex")
```

```{r include=FALSE}
library(ggthemr)
ggthemr("solarized")
```

# Exploratory Analysis
```{r}
#Table recoding
table_cerv <- cerv_dyst %>%
              mutate(
                sex = if_else(sex == 1, "Female", "Male"),
                treat = if_else(treat == 1, "10000U", if_else(
                  treat == 2,"5000U","Placebo")),
                week = paste0("Week ",week),
                week = factor(week, levels = c("Week 0",
                                               "Week 2",
                                               "Week 4",
                                               "Week 8",
                                               "Week 12",
                                               "Week 16")))
#Summary Table by Gender, Week, Treatment
table_cerv %>%
  select(-id, -week, -treat) %>%
  as.data.frame() %>%
  tbl_summary(by = sex) %>%
  add_p() %>% 
  bold_p()

table_cerv %>%
  select(-id, -sex, -week, -age) %>%
  as.data.frame() %>%
  tbl_summary(by = treat) %>%
  add_p() %>% 
  bold_p()
```

```{r}
#Histogram of Age and Rating Scales
age_hist <- ggplot(data = cerv_dyst, aes(x = age)) +
            geom_histogram(col = "white", fill = "seagreen3")

twstrs_hist <- ggplot(data = cerv_dyst, aes(x = twstrs)) +
               geom_histogram(col = "white", fill = "seagreen3") +
               labs(x = "Toronto Western Spasmodic Torticollis Rating Scale (TWSTRS)")

set.seed(123)
id_sample <- sample(cerv_dyst$id, 15)

# Plots of 15 random sample individuals
ggplot(data = cerv_dyst %>% filter(id %in% id_sample),
       aes(x = week, y = twstrs)) +
  geom_line() +
  facet_wrap(~id, ncol = 5) +
  theme(strip.background = element_rect(colour="white",
                                        fill="skyblue", 
                                        size=4,
                                        linetype="solid")) +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12, 16)) + 
  labs(x = "Week",
       y = "TWSTRS") 

# Every individual's change through the weeks with average
ggplot(data = cerv_dyst, aes(x = week, y = twstrs)) +
  geom_line(alpha=0.3, aes(group = id)) +
  geom_smooth(col = "seagreen4", size = 2) +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12, 16)) + 
  labs(x = "Week",
       y = "TWSTRS") 
  
```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

