PUBH 7462 Homework 2
================
Chris Wojan
2/10/2022

### Problem 3.1

#### Data Exploration and Cleaning

Data exploration code is hidden, but data cleaning code is presented
below:

``` r
### "Clean" the data

## Make column (variable) names consistent
brfss_clean <- clean_names(brfss_data) 

## Subset the data that we are interested in
brfss_clean <- brfss_clean %>%
  separate(col = locationdesc, into = c("state", "county"), sep = " - ") %>%
  filter(topic %in% "Overall Health") %>%
  select(year, state, county, response, sample_size, data_value)

## Check if column types need to be reformatted
str(brfss_clean)

## Format variables that represent categories as factors
brfss_clean <- brfss_clean %>%
  mutate(
    state = factor(state),
    county = str_remove(county, " County") %>%
      factor(),
    response = factor(response)
  )

## Check response level order
levels(brfss_clean$response)

## Reorder response levels by ordinal position
brfss_clean$response <- fct_relevel(brfss_clean$response, 
                                    c("Excellent", "Very good", "Good", "Fair", "Poor"))
```

### Problem 3.2

#### Data Description

This cleaned BRFSS SMART dataset represents information on the
self-reported general health of survey particpants by state and county.
It includes 10625 rows (or observations) and 6 columns (or variables).
Each observation represents a particular level of general health for a
given state, county and year. The variables are as follows:

| Variable    | Description                                                   |
|-------------|---------------------------------------------------------------|
| year        | Year of survey                                                |
| state       | US state where survey was conducted                           |
| county      | County where survey was conducted                             |
| response    | Self-reported general health status (Excellent - Poor)        |
| sample_size | Number of respondents that reported this health status (#)    |
| data_value  | Proportion of respondent that reported this health status (%) |

### Problem 3.3

#### 3.3.1

``` r
brfss_331 <- brfss_clean %>%
  filter(year == 2004) %>%
  select(year, state, county) %>%
  distinct()
```

The following states featured 6 surveyed counties (or locations): CO,
CT, MD, NM, SC, TX, UT, VT

#### 3.3.2
