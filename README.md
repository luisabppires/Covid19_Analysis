---
title: "Data Analysis"
author: "Luisa Pires"
date: "`r Sys.Date()`"
output:
  prettydoc::html_pretty:
    theme: leonids
    highlight: github
---

This shows an example of an analysis using the daily country data on number of deaths, recovered and confirmed cases of COVID 19. 

Here we focus on Portugal, Spain, UK and Italy. Analysis performed:

1. Daily differences (absolute,  percentage and 7-day rolling average)
2. % differences in accumulated data 
3. indicators per million inhabitants
4. Analysis of reference points (days since lockdown, days since 50th death reported)


Data from John Hopkins with thanks:
https://github.com/pomber/covid19
https://github.com/CSSEGISandData/COVID-19

"This is the data repository for the 2019 Novel Coronavirus Visual Dashboard operated by the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). Also, Supported by ESRI Living Atlas Team and the Johns Hopkins University Applied Physics Lab (JHU APL)."


```{r setup, echo=FALSE,  cache=FALSE, warning=FALSE, message=FALSE}

knitr::opts_chunk$set(
  echo = TRUE,
  cache = TRUE,
  prompt = FALSE,
  tidy = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE
)
knitr::opts_knit$set(width = 75)

```




```{r, echo = FALSE}
library(knitr)
library(rmdformats)
library(tidyverse)
library(httr)
library(jsonlite)
library(plotly)
library(kableExtra)
library(zoo)

make_table_comparison = function(df, xvariable, yvariable, n = 10) {
  df = select(df, one_of(yvariable, xvariable, "country"))
  
  if (xvariable == "date")  {
    df = filter(df, date > Sys.Date() - n)
  } else {
    df = filter(df, get(xvariable) > 0)
  }
  
  
  df = df %>% pivot_wider(names_from = "country", values_from = yvariable) %>%
    mutate_at(vars("Portugal", "Spain", "United.Kingdom", "Italy"), function(x) {
      cell_spec(x, bold = T,
                color = spec_color(x, end = 0.9))
    }) %>%
    
    kable(escape = F, align = "c") %>%
    kable_styling(c("striped", "hover"))
  
  return(df)
  
}

make_table = function(df, n = 30) {
  df %>%  tail(n) %>%
    kable() %>%
    kable_styling()
}


chart_variable_bar = function(df, xvariable, yvariable, n = 30) {
  if (xvariable == "date")  {
    df = filter(df, date > Sys.Date() - n)
  } else {
    df = filter(df, get(xvariable) > 0)
  }
  
  p = ggplot(data = df, aes_string(x = xvariable, y = yvariable, fill =
                                     "country")) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal() + xlab(xvariable) + ylab(yvariable)
  
  ggplotly(p)
  
}

chart_variable_line = function(df, xvariable, yvariable, n = 30) {
  if (xvariable == "date")  {
    df = filter(df, date > Sys.Date() - n)
  } else {
    df = filter(df, get(xvariable) > 0)
  }
  
  p = ggplot(data = df,
             aes_string(x = xvariable, y = yvariable, colour = "country")) +
    geom_line() +
    scale_color_brewer(palette = "Paired") +
    theme_minimal() + xlab(xvariable) + ylab(yvariable)
  
  ggplotly(p)
}
```

# Get the data

Here we get the data from  "https://pomber.github.io/covid19/timeseries.json". A few inconsistencies have been detected from worldmeter.

1. United Kingdom (16-19 April)
2. Spain (last 16019 April)

```{r}

#path to the API and request data
path <- "https://pomber.github.io/covid19/timeseries.json"
request <- GET(url = path)

# get the response
response <- content(request, as = "text", encoding = "UTF-8")

# data from json
data_json <- fromJSON(response) %>%
  data.frame()

# show data
data_json %>% select(1:6) %>% make_table(5)
```

Prepare the data

Here we do the following transformations

1. Split the first column into dimensions
2. Dimensions are furthers split into confirmed, deaths and recovered

```{r}

# find the last "."
data = enframe(unlist(data_raw)) %>%
  mutate(position = str_locate_all(name, "\\.")) %>%
  rowwise() %>%
  mutate(position = max(position)) %>% 
  ungroup()

# replace last dot with space
str_sub(data$name, data$position, data$position) = " "

# split into countries, dimensions and sub-simensions
data = data %>%
  separate(.,
           name,
           c("country", "dimension"),
           sep = " " ,
           remove = FALSE) %>%
  mutate(dimension1 = (str_extract(dimension, "[aA-zZ]+")),
         datapointid = (str_extract(dimension, "[0-9]+"))) %>%
  select(-name, -dimension, -position) %>%
  pivot_wider(names_from = dimension1,
              values_from = value) %>%
  select(-datapointid)

# show data
data %>% make_table(10)

```
Now we change formats to ensure:

1. dates are formatted as such
2. sub-dimensions are numeric

```{r}

data_clean = data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate_at(vars(confirmed, deaths, recovered), as.numeric)

```

We can prepare data for analysis

1. Filter by country

```{r}

data_clean = data_clean %>%
  filter(country %in% c("Portugal", "Spain", "Italy", "United.Kingdom"))

```

2. Compute daily numbers

```{r}

data_clean = group_by(data_clean, country) %>%
  mutate(
    previous_deaths  = lag(deaths, n = 1, default = NA),
    previous_confirmed  = lag(confirmed, n = 1, default = NA),
    previous_recovered  = lag(recovered, n = 1, default = NA),
    daily_deaths  = deaths - previous_deaths,
    daily_confirmed  = confirmed - previous_confirmed,
    daily_recovered  = recovered - previous_recovered ) 

data_clean %>% select (country, date, starts_with("daily")) %>% make_table(10)

```

3. Compute numbers per population

```{r}

#  population of countries
pop = c(60.36,46.94,66.65 ,10.28 )

# add inhabitants
data_clean = data_clean %>%
  mutate(population = ifelse(country == "Italy", pop[1], 0),
         population = ifelse(country == "Spain", pop[2], population),
         population = ifelse(country == "United.Kingdom", pop[3], population),
         population = ifelse(country == "Portugal", pop[4], population))

# compute KIPs/milion inhabitants
data_clean = data_clean %>%
    mutate(mil_deaths= round(deaths / population, 2),
    mil_confirmed = round(confirmed / population, 2),
    mil_recovered = round(recovered / population, 2)
  )

#show table
data_clean %>% select (country, date, starts_with("mil")) %>% make_table(10)

```

4. Compute percentage increases

```{r}
# compute percentage increases
data_clean = data_clean %>%
  mutate(
    perc_deaths = round(daily_deaths / previous_deaths, 2) * 100,
    perc_confirmed = round(daily_confirmed / previous_confirmed, 2) *
      100,
    perc_recovered = round(daily_recovered / previous_recovered, 2) *
      100
  ) %>%   select(-starts_with("previous"))


data_clean %>% select (country, date, starts_with("perc")) %>% make_table(10)

```

5. Compute daily differences and 7 day average

```{r}

# compute daily differences in percentages
data_clean = data_clean %>%
  mutate(
    prev_deaths  = lag(daily_deaths, n = 1, default = NA),
    prev_confirmed  = lag(daily_confirmed, n = 1, default = NA),
    prev_recovered  = lag(daily_recovered, n = 1, default = NA),
    per_daily_deaths  = round((deaths - prev_deaths)/prev_deaths,2)*100,
    per_daily_confirmed  = round((confirmed - prev_confirmed)/prev_confirmed,2)*100,
    per_daily_recovered  = round((recovered - prev_recovered)/prev_recovered,2)*100) %>% 
    select(-starts_with("prev"))

data_clean %>% select (country, date, starts_with(c("per_daily"))) %>% make_table(10)

```

```{r}

# compute daily rolling average
data_clean =  data_clean %>%
      mutate(roll_mean_daily_deaths = round(rollmean(daily_deaths, 7, align="right",na.pad = T),2),
            roll_mean_daily_recovered = round(rollmean(daily_recovered, 7, align="right",na.pad = T),2),
            roll_mean_daily_confirmed = round(rollmean(daily_confirmed, 7, align="right",na.pad = T),2))  


# show tables
data_clean %>% select (country, date, starts_with(c("roll_mean"))) %>% make_table(10)

```
# Chart

We can now how the results of the data preparation.

Daily  confirmed cases

```{r}

chart_variable_bar(data_clean,"date", "daily_confirmed",10)

```

## Changing reference points

We now add a few reference points

### Days since lock downs 

In order to compare the effect of lock down, below is the days per country. We add a flag that counts the number of days since lock down for these countries

```{r}
# lockdown days in italy, spain, united kingdom, portugal
lockdown =
  as.Date(c("2020-03-09", "2020-03-14",  "2020-03-23", "2020-03-18"),
          format = "%Y-%m-%d")

# count lockdiwn days
data_clean = data_clean %>%
  mutate(days_since_lockdown = ifelse((country == "Italy" &
                                         date >= lockdown[1]) |
                                        (country == "Spain" &
                                           date >= lockdown[2]) |
                                        (country == "Portugal" &
                                           date >= lockdown[4]) |
                                        (country == "United.Kingdom" &
                                           date >= lockdown[3]) ,
                                      1,
                                      0
  )) %>%
  mutate(days_since_lockdown = cumsum(days_since_lockdown))

# show data
data_clean %>% select(country,date, days_since_lockdown) %>%  make_table(10)

```

###  Says since 50 deaths reported

We now do the same for the 50th death.

```{r}
# find 50th death
data_clean = data_clean %>%
  mutate(days_after_50_death = ifelse(deaths >= 50, 1, 0)) %>%
  mutate(days_after_50_death = cumsum(days_after_50_death))

# show table
data_clean %>% select(country, date,days_after_50_death) %>% make_table(10)

```

# Comparative analysis

We can do now comparative analysis between countries

1. Days since 50th death vs cumulative deaths

```{r}

chart_variable_line(data_clean, "days_after_50_death", "deaths")

```


2. Days since 50th death vs cumulative deaths per inhabitants 

```{r}

chart_variable_line(data_clean, "days_since_lockdown", "mil_deaths")

```

3. Trend of cases since lock down

```{r}

chart_variable_line(data_clean, "days_since_lockdown", "perc_confirmed")

```

4. Growth since lock down as table

```{r}

data_clean %>% make_table_comparison("days_since_lockdown","perc_confirmed")

```

5. Rolling avergage of daily difference in deaths 
```{r}

chart_variable_line(data_clean, "days_since_lockdown", "roll_mean_daily_deaths",n=30)

```



6. Rolling avergage of daily difference in confirmed cases 

```{r}

chart_variable_line(data_clean, "days_since_lockdown", "roll_mean_daily_confirmed",n=30)

```

