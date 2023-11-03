ST558_Project3
================
Hui Fang
2023-11-02

# Introduction

The `Behavioral Risk Factor Surveillance System (BRFSS)` is an annual
health-related telephone survey conducted by the Centers for Disease
Control and Prevention (CDC) since 1984. It is designed to gather
standardized, state-specific information on preventive health practices
and risk behaviors associated with chronic diseases, injuries, and
preventable infectious diseases within the adult population.

Diabetes stands as one of the most prevalent chronic diseases in the
United States, affecting millions of Americans annually and imposing a
substantial economic burden on the nation.

For this project, we will use the
`diabetes _ binary _ health _ indicators _ BRFSS2015.csv` dataset,
sourced from [Diabetes Health Indicators
Dataset](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/).

This dataset comprises 253,680 survey responses to the CDC’s BRFSS2015.
The target variable `Diabetes_binary` has two classes: 0 for no
diabetes, and 1 for prediabetes or diabetes. This dataset encompasses 21
feature variables and is not balanced, with Diabetes_binary serving as
the response variable. The information of variable can be found
[here](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/?select=diabetes_binary_health_indicators_BRFSS2015.csv)

- Diabetes_binary: 0 = no diabetes 1 = prediabetes or diabetes  
- HighBP: High blood pressure  
- HighChol: High cholesterol  
- CholCheck: 0 = no cholesterol check in 5 years 1 = yes cholesterol
  check in 5 years  
- BMI: Body Mass Index  
- Smoker: Have you smoked at least 100 cigarettes in your entire life? 0
  = no 1 = yes  
- Stroke: 0 = no 1 = yes  
- HeartDiseaseorAttack: coronary heart disease (CHD) or myocardial
  infarction (MI) 0 = no 1 = yes  
- PhysActivity: physical activity in past 30 days - not including job 0
  = no 1 = yes  
- Fruits: Consume Fruit 1 or more times per day 0 = no 1 = yes  
- Veggies: Consume Vegetables 1 or more times per day 0 = no 1 = yes  
- HvyAlcoholConsump: (adult men \>=14 drinks per week and adult
  women\>=7 drinks per week) 0 = no 1 = yes  
- AnyHealthcare: Health care coverage 0 = no 1 = yes  
- NoDocbcCost: Was there a time in the past 12 months when you needed to
  see a doctor but could not because of cost? 0 = no 1 = yes  
- GenHlth: in general your health is: scale 1-5 1 = excellent 2 = very
  good 3 = good 4 = fair 5 = poor  
- MentHlth: days of poor mental health scale 1-30 days  
- PhysHlth: physical illness or injury days in past 30 days scale 1-30  
- DiffWalk: Do you have serious difficulty walking or climbing stairs? 0
  = no 1 = yes  
- Sex: 0 = female 1 = male  
- Age: 13-level age category 1 = 18-24 2 = 25-29 3 = 30-34 4 = 35-39 5 =
  40-44 6 = 45-49 7 = 50-54 8 = 55-59 9 = 60-64 10 = 65-69 11 = 70-74 12
  = 75-79 13 = 80 or older  
- Education: scale 1-6 1 = Never attended school or only kindergarten 2
  = Grades 1 through 8 (Elementary) 3 = Grades 9 through 11 (Some high
  school) 4 = Grade 12 or GED (High school graduate) 5 = College 1 year
  to 3 years (Some college or technical school) 6 = College 4 years or
  more (College graduate)  
- Income: scale 1-8 1 = less than \$10,000 5 = less than \$35,000 8 =
  \$75,000 or more

# Data

## Read in data

``` r
library(dplyr)
library(readr)
diabetes <- as_tibble(read.csv("diabetes_binary_health_indicators_BRFSS2015.csv", header = TRUE))
head(diabetes)
```

    ## # A tibble: 6 × 22
    ##   Diabetes_binary HighBP HighChol CholCheck   BMI Smoker Stroke
    ##             <dbl>  <dbl>    <dbl>     <dbl> <dbl>  <dbl>  <dbl>
    ## 1               0      1        1         1    40      1      0
    ## 2               0      0        0         0    25      1      0
    ## 3               0      1        1         1    28      0      0
    ## 4               0      1        0         1    27      0      0
    ## 5               0      1        1         1    24      0      0
    ## 6               0      1        1         1    25      1      0
    ## # ℹ 15 more variables: HeartDiseaseorAttack <dbl>, PhysActivity <dbl>,
    ## #   Fruits <dbl>, Veggies <dbl>, HvyAlcoholConsump <dbl>, AnyHealthcare <dbl>,
    ## #   NoDocbcCost <dbl>, GenHlth <dbl>, MentHlth <dbl>, PhysHlth <dbl>,
    ## #   DiffWalk <dbl>, Sex <dbl>, Age <dbl>, Education <dbl>, Income <dbl>

``` r
Diabetes_binary <- as.factor(diabetes$Diabetes_binary)
Income <- factor(diabetes$Income, levels = c("1", "2", "3", "4", "5", "6", "7", "8"))
```

## Subsetting the dataset based on education level

``` r
# Never attended school or only kindergarten to Grades 1 through 8 (Elementary)
diabetes2 <- diabetes %>% filter(Education %in% c(1, 2))

# Combine level 1 and 2 to make it as 2
diabetes2$Education <- ifelse(diabetes2$Education == 1, 2, 2)

# Grades 9 through 11 (Some high school) 
diabetes3 <- diabetes %>% filter(Education == 3)

# Grade 12 or GED (High school graduate) 
diabetes4 <- diabetes %>% filter(Education == 4)

# College 1 year to 3 years (Some college or technical school) 
diabetes5 <- diabetes %>% filter(Education == 5)

# College 4 years or more (College graduate) 
diabetes6 <- diabetes %>% filter(Education == 6)
```

## EDA for diabetes3

``` r
# Checking missing values of columns in diabetes3
missing_values <- colSums(is.na(diabetes3))
missing_values # no missing values
```

    ##      Diabetes_binary               HighBP             HighChol 
    ##                    0                    0                    0 
    ##            CholCheck                  BMI               Smoker 
    ##                    0                    0                    0 
    ##               Stroke HeartDiseaseorAttack         PhysActivity 
    ##                    0                    0                    0 
    ##               Fruits              Veggies    HvyAlcoholConsump 
    ##                    0                    0                    0 
    ##        AnyHealthcare          NoDocbcCost              GenHlth 
    ##                    0                    0                    0 
    ##             MentHlth             PhysHlth             DiffWalk 
    ##                    0                    0                    0 
    ##                  Sex                  Age            Education 
    ##                    0                    0                    0 
    ##               Income 
    ##                    0

``` r
# Describing 
library(knitr)
kable(summary(diabetes3))
```

|     | Diabetes_binary | HighBP         | HighChol       | CholCheck      | BMI           | Smoker         | Stroke          | HeartDiseaseorAttack | PhysActivity   | Fruits         | Veggies        | HvyAlcoholConsump | AnyHealthcare  | NoDocbcCost    | GenHlth       | MentHlth       | PhysHlth       | DiffWalk       | Sex            | Age            | Education | Income        |
|:----|:----------------|:---------------|:---------------|:---------------|:--------------|:---------------|:----------------|:---------------------|:---------------|:---------------|:---------------|:------------------|:---------------|:---------------|:--------------|:---------------|:---------------|:---------------|:---------------|:---------------|:----------|:--------------|
|     | Min. :0.0000    | Min. :0.0000   | Min. :0.0000   | Min. :0.0000   | Min. :13.00   | Min. :0.0000   | Min. :0.00000   | Min. :0.0000         | Min. :0.0000   | Min. :0.0000   | Min. :0.0000   | Min. :0.00000     | Min. :0.0000   | Min. :0.0000   | Min. :1.000   | Min. : 0.000   | Min. : 0.000   | Min. :0.0000   | Min. :0.0000   | Min. : 1.000   | Min. :3   | Min. :1.000   |
|     | 1st Qu.:0.0000  | 1st Qu.:0.0000 | 1st Qu.:0.0000 | 1st Qu.:1.0000 | 1st Qu.:25.00 | 1st Qu.:0.0000 | 1st Qu.:0.00000 | 1st Qu.:0.0000       | 1st Qu.:0.0000 | 1st Qu.:0.0000 | 1st Qu.:0.0000 | 1st Qu.:0.00000   | 1st Qu.:1.0000 | 1st Qu.:0.0000 | 1st Qu.:3.000 | 1st Qu.: 0.000 | 1st Qu.: 0.000 | 1st Qu.:0.0000 | 1st Qu.:0.0000 | 1st Qu.: 7.000 | 1st Qu.:3 | 1st Qu.:2.000 |
|     | Median :0.0000  | Median :1.0000 | Median :0.0000 | Median :1.0000 | Median :28.00 | Median :1.0000 | Median :0.00000 | Median :0.0000       | Median :1.0000 | Median :1.0000 | Median :1.0000 | Median :0.00000   | Median :1.0000 | Median :0.0000 | Median :3.000 | Median : 0.000 | Median : 0.000 | Median :0.0000 | Median :0.0000 | Median : 9.000 | Median :3 | Median :4.000 |
|     | Mean :0.2422    | Mean :0.5806   | Mean :0.4998   | Mean :0.9637   | Mean :29.64   | Mean :0.6221   | Mean :0.08757   | Mean :0.1707         | Mean :0.5654   | Mean :0.5236   | Mean :0.6763   | Mean :0.03904     | Mean :0.8804   | Mean :0.1628   | Mean :3.243   | Mean : 5.319   | Mean : 7.916   | Mean :0.3702   | Mean :0.4184   | Mean : 8.572   | Mean :3   | Mean :3.765   |
|     | 3rd Qu.:0.0000  | 3rd Qu.:1.0000 | 3rd Qu.:1.0000 | 3rd Qu.:1.0000 | 3rd Qu.:33.00 | 3rd Qu.:1.0000 | 3rd Qu.:0.00000 | 3rd Qu.:0.0000       | 3rd Qu.:1.0000 | 3rd Qu.:1.0000 | 3rd Qu.:1.0000 | 3rd Qu.:0.00000   | 3rd Qu.:1.0000 | 3rd Qu.:0.0000 | 3rd Qu.:4.000 | 3rd Qu.: 5.000 | 3rd Qu.:15.000 | 3rd Qu.:1.0000 | 3rd Qu.:1.0000 | 3rd Qu.:11.000 | 3rd Qu.:3 | 3rd Qu.:5.000 |
|     | Max. :1.0000    | Max. :1.0000   | Max. :1.0000   | Max. :1.0000   | Max. :95.00   | Max. :1.0000   | Max. :1.00000   | Max. :1.0000         | Max. :1.0000   | Max. :1.0000   | Max. :1.0000   | Max. :1.00000     | Max. :1.0000   | Max. :1.0000   | Max. :5.000   | Max. :30.000   | Max. :30.000   | Max. :1.0000   | Max. :1.0000   | Max. :13.000   | Max. :3   | Max. :8.000   |

result shows that there is no missing values in dataset diabetes3

``` r
# Set the figure size (in inches)
options(repr.plot.width = 8, repr.plot.height = 6)
```