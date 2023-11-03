ST558_Project3
================
Hui Fang
2023-11-02

# Introduction

Diabetes is a seriously pervasive chronic disease that disrupts the
body’s ability to regulate blood glucose levels, leading to a diminished
quality of life and reduced life expectancy. It stands as one of the
most prevalent chronic illnesses in the United States, impacting
millions of Americans annually and imposing a significant economic
burden on the nation.

In this project, we will use the `diabetes binary health indicators`
dataset obtained from
[Kaggle](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/)
to conduct comprehensive exploratory data analysis (EDA) and develop
predictive models. This dataset comprises 253,680 survey responses to
the CDC’s BRFSS (Behavioral Risk Factor Surveillance System) from year
2015. The primary target variable, `Diabetes_binary`, offers binary
classification, distinguishing between 0 for no diabetes, and 1 for
prediabetes or diabetes. This dataset encompasses 21 feature variables
and is not balanced. Detailed information of variable can be found
[here](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/?select=diabetes_binary_health_indicators_BRFSS2015.csv).

Our analysis will primarily focus on a subset of key variables,
including High blood pressure (HighBP), High cholesterol (HighChol),
cholesterol check (CholCheck), Body Mass Index (BMI), Smoker, Fruits,
Veggies, and Age.  
In our EDA phase, we will start by summarizing basic statistics
visualizing variable frequencies. This will be followed by the
exploration of correlations between variables and the creation of
contingency tables to better understand the interplay of these factors.

Based on the results from EDA, we will split the dataset into training
(70%) and test (30%) subsets for each educational level. Subsequently,
we will employ the training data to fit six distinct models, including a
logistic regression, a LASSO logistic regression, a classification tree
model, a random forest model, a xx model, as well as a xxx model. The
performance of these models will be rigorously evaluated using the test
dataset, and we will determine the most effective model for predicting
diabetes outcomes.

Description of variables in the data set: + Diabetes_binary: 0 = no
diabetes 1 = prediabetes or diabetes  
+ HighBP: High blood pressure  
+ HighChol: High cholesterol  
+ CholCheck: 0 = no cholesterol check in 5 years 1 = yes cholesterol
check in 5 years  
+ BMI: Body Mass Index  
+ Smoker: Have you smoked at least 100 cigarettes in your entire life? 0
= no 1 = yes  
+ Stroke: 0 = no 1 = yes  
+ HeartDiseaseorAttack: coronary heart disease (CHD) or myocardial
infarction (MI) 0 = no 1 = yes  
+ PhysActivity: physical activity in past 30 days - not including job 0
= no 1 = yes  
+ Fruits: Consume Fruit 1 or more times per day 0 = no 1 = yes  
+ Veggies: Consume Vegetables 1 or more times per day 0 = no 1 = yes  
+ HvyAlcoholConsump: (adult men \>=14 drinks per week and adult
women\>=7 drinks per week) 0 = no 1 = yes  
+ AnyHealthcare: Health care coverage 0 = no 1 = yes  
+ NoDocbcCost: Was there a time in the past 12 months when you needed to
see a doctor but could not because of cost? 0 = no 1 = yes  
+ GenHlth: in general your health is: scale 1-5 1 = excellent 2 = very
good 3 = good 4 = fair 5 = poor  
+ MentHlth: days of poor mental health scale 1-30 days  
+ PhysHlth: physical illness or injury days in past 30 days scale 1-30  
+ DiffWalk: Do you have serious difficulty walking or climbing stairs? 0
= no 1 = yes  
+ Sex: 0 = female 1 = male  
+ Age: 13-level age category 1 = 18-24 2 = 25-29 3 = 30-34 4 = 35-39 5 =
40-44 6 = 45-49 7 = 50-54 8 = 55-59 9 = 60-64 10 = 65-69 11 = 70-74 12 =
75-79 13 = 80 or older  
+ Education: scale 1-6 1 = Never attended school or only kindergarten 2
= Grades 1 through 8 (Elementary) 3 = Grades 9 through 11 (Some high
school) 4 = Grade 12 or GED (High school graduate) 5 = College 1 year to
3 years (Some college or technical school) 6 = College 4 years or more
(College graduate)  
+ Income: scale 1-8 1 = less than \$10,000 5 = less than \$35,000 8 =
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

<<<<<<< HEAD
``` r
# Convert Diabetes_binary and Income variables to factor
Diabetes_binary <- as.factor(diabetes$Diabetes_binary)
Income <- factor(diabetes$Income, levels = c("1", "2", "3", "4", "5", "6", "7", "8"))
```
=======
# grouping Education levels

diabetes$Education <- ifelse(diabetes$Education %in% c(1, 2),
“Non_Elementary”,
ifelse(diabetes$Education == 3, "SomeHighSchool",  ifelse(diabetes$Education
== 4, “HighSchool”,
ifelse(diabetes$Education == 5, "SomeCollege",  ifelse(diabetes$Education
== 6, “College”, NA)))))

# Convert some variables to factor

diabetes$Education <- as.factor(diabetes$Education)
\#diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)
\#diabetes$Income <- as.factor(diabetes$Income)
diabetes$HighBP <- as.factor(diabetes$HighBP)
diabetes$HighChol <- as.factor(diabetes$HighChol)
diabetes$Sex <- as.factor(diabetes$Sex)
diabetes$Fruits <- as.factor(diabetes$Fruits) params\$Edu
>>>>>>> joyz

## Subsetting the dataset based on education level

``` r
<<<<<<< HEAD
# Never attended school or only kindergarten to Grades 1 through 8 (Elementary)
diabetes2 <- diabetes %>% filter(Education %in% c(1, 2))

# Combine education level 1 and 2 to make it as 2
diabetes2$Education <- ifelse(diabetes2$Education == 1, 2, 2)

# Grades 9 through 11 (Some high school) 
diabetes3 <- diabetes %>% filter(Education == 3)

# Grade 12 or GED (High school graduate) 
diabetes4 <- diabetes %>% filter(Education == 4)

# College 1 year to 3 years (Some college or technical school) 
diabetes5 <- diabetes %>% filter(Education == 5)

# College 4 years or more (College graduate) 
diabetes6 <- diabetes %>% filter(Education == 6)
params$Education
```

<<<<<<< HEAD
    ## [1] "2"

## EDA

``` r
# Checking missing values of columns in diabetes2
=======
## EDA for diabetes for education 2

``` r
# Checking missing values of columns in diabetes3
>>>>>>> f0c30b759d95e0613be060fc81ddae23e1a0e4a5
missing_values <- colSums(is.na(diabetes2))
missing_values # no missing values
=======
EducationData <- filter(diabetes, (Education == params$Edu))
```

## EDA

``` r
# Checking missing values of columns in diabetes2
missing_values <- colSums(is.na(EducationData))
>>>>>>> joyz
```

``` r
# Describing 
library(knitr)
<<<<<<< HEAD
kable(summary(diabetes2))
```

|     | Diabetes_binary | HighBP         | HighChol      | CholCheck      | BMI           | Smoker         | Stroke          | HeartDiseaseorAttack | PhysActivity   | Fruits         | Veggies        | HvyAlcoholConsump | AnyHealthcare  | NoDocbcCost    | GenHlth       | MentHlth       | PhysHlth       | DiffWalk       | Sex            | Age            | Education | Income        |
|:----|:----------------|:---------------|:--------------|:---------------|:--------------|:---------------|:----------------|:---------------------|:---------------|:---------------|:---------------|:------------------|:---------------|:---------------|:--------------|:---------------|:---------------|:---------------|:---------------|:---------------|:----------|:--------------|
|     | Min. :0.0000    | Min. :0.0000   | Min. :0.000   | Min. :0.0000   | Min. :12.00   | Min. :0.0000   | Min. :0.00000   | Min. :0.0000         | Min. :0.0000   | Min. :0.0000   | Min. :0.0000   | Min. :0.00000     | Min. :0.0000   | Min. :0.0000   | Min. :1.000   | Min. : 0.000   | Min. : 0.000   | Min. :0.0000   | Min. :0.0000   | Min. : 1.000   | Min. :2   | Min. :1.000   |
|     | 1st Qu.:0.0000  | 1st Qu.:0.0000 | 1st Qu.:0.000 | 1st Qu.:1.0000 | 1st Qu.:25.00 | 1st Qu.:0.0000 | 1st Qu.:0.00000 | 1st Qu.:0.0000       | 1st Qu.:0.0000 | 1st Qu.:0.0000 | 1st Qu.:0.0000 | 1st Qu.:0.00000   | 1st Qu.:1.0000 | 1st Qu.:0.0000 | 1st Qu.:3.000 | 1st Qu.: 0.000 | 1st Qu.: 0.000 | 1st Qu.:0.0000 | 1st Qu.:0.0000 | 1st Qu.: 7.000 | 1st Qu.:2 | 1st Qu.:2.000 |
|     | Median :0.0000  | Median :1.0000 | Median :1.000 | Median :1.0000 | Median :28.00 | Median :0.0000 | Median :0.00000 | Median :0.0000       | Median :1.0000 | Median :1.0000 | Median :1.0000 | Median :0.00000   | Median :1.0000 | Median :0.0000 | Median :4.000 | Median : 0.000 | Median : 1.000 | Median :0.0000 | Median :0.0000 | Median :10.000 | Median :2 | Median :3.000 |
|     | Mean :0.2917    | Mean :0.5874   | Mean :0.534   | Mean :0.9718   | Mean :29.46   | Mean :0.4804   | Mean :0.08537   | Mean :0.1914         | Mean :0.5682   | Mean :0.5767   | Mean :0.6929   | Mean :0.02656     | Mean :0.8406   | Mean :0.1788   | Mean :3.471   | Mean : 5.219   | Mean : 8.368   | Mean :0.3801   | Mean :0.4586   | Mean : 9.111   | Mean :2   | Mean :3.313   |
|     | 3rd Qu.:1.0000  | 3rd Qu.:1.0000 | 3rd Qu.:1.000 | 3rd Qu.:1.0000 | 3rd Qu.:33.00 | 3rd Qu.:1.0000 | 3rd Qu.:0.00000 | 3rd Qu.:0.0000       | 3rd Qu.:1.0000 | 3rd Qu.:1.0000 | 3rd Qu.:1.0000 | 3rd Qu.:0.00000   | 3rd Qu.:1.0000 | 3rd Qu.:0.0000 | 3rd Qu.:4.000 | 3rd Qu.: 5.000 | 3rd Qu.:15.000 | 3rd Qu.:1.0000 | 3rd Qu.:1.0000 | 3rd Qu.:12.000 | 3rd Qu.:2 | 3rd Qu.:5.000 |
|     | Max. :1.0000    | Max. :1.0000   | Max. :1.000   | Max. :1.0000   | Max. :84.00   | Max. :1.0000   | Max. :1.00000   | Max. :1.0000         | Max. :1.0000   | Max. :1.0000   | Max. :1.0000   | Max. :1.00000     | Max. :1.0000   | Max. :1.0000   | Max. :5.000   | Max. :30.000   | Max. :30.000   | Max. :1.0000   | Max. :1.0000   | Max. :13.000   | Max. :2   | Max. :8.000   |

<<<<<<< HEAD
result shows that there is no missing values in dataset diabetes2
=======
result shows that there is no missing values in the dataset.
>>>>>>> f0c30b759d95e0613be060fc81ddae23e1a0e4a5

``` r
# Create a correlation matrix between variables
Cor_Matrix <- cor(diabetes2)
```

    ## Warning in cor(diabetes2): the standard deviation is zero

``` r
# Round the correlation matrix to two decimal places
rounded_Cor_Matrix <- round(Cor_Matrix, digits = 2)
# Print the rounded correlation matrix
kable(rounded_Cor_Matrix)
```

|                      | Diabetes_binary | HighBP | HighChol | CholCheck |   BMI | Smoker | Stroke | HeartDiseaseorAttack | PhysActivity | Fruits | Veggies | HvyAlcoholConsump | AnyHealthcare | NoDocbcCost | GenHlth | MentHlth | PhysHlth | DiffWalk |   Sex |   Age | Education | Income |
|:---------------------|----------------:|-------:|---------:|----------:|------:|-------:|-------:|---------------------:|-------------:|-------:|--------:|------------------:|--------------:|------------:|--------:|---------:|---------:|---------:|------:|------:|----------:|-------:|
| Diabetes_binary      |            1.00 |   0.25 |     0.23 |      0.08 |  0.17 |   0.02 |   0.08 |                 0.19 |        -0.06 |  -0.02 |   -0.03 |             -0.06 |          0.10 |        0.00 |    0.26 |     0.10 |     0.16 |     0.20 | -0.04 |  0.16 |        NA |  -0.15 |
| HighBP               |            0.25 |   1.00 |     0.31 |      0.08 |  0.14 |   0.07 |   0.15 |                 0.20 |        -0.06 |  -0.08 |   -0.08 |              0.02 |          0.16 |        0.01 |    0.23 |     0.13 |     0.18 |     0.25 | -0.05 |  0.30 |        NA |  -0.14 |
| HighChol             |            0.23 |   0.31 |     1.00 |      0.04 |  0.09 |   0.11 |   0.11 |                 0.17 |        -0.06 |  -0.07 |   -0.05 |              0.02 |          0.08 |        0.05 |    0.22 |     0.14 |     0.17 |     0.22 | -0.06 |  0.12 |        NA |  -0.08 |
| CholCheck            |            0.08 |   0.08 |     0.04 |      1.00 |  0.01 |  -0.03 |   0.03 |                 0.05 |        -0.02 |   0.00 |   -0.02 |             -0.03 |          0.10 |       -0.01 |    0.08 |     0.00 |     0.04 |     0.07 | -0.03 |  0.11 |        NA |  -0.06 |
| BMI                  |            0.17 |   0.14 |     0.09 |      0.01 |  1.00 |  -0.02 |   0.01 |                 0.04 |        -0.08 |  -0.03 |   -0.03 |             -0.01 |         -0.02 |        0.03 |    0.10 |     0.07 |     0.08 |     0.17 | -0.07 | -0.12 |        NA |  -0.05 |
| Smoker               |            0.02 |   0.07 |     0.11 |     -0.03 | -0.02 |   1.00 |   0.09 |                 0.13 |        -0.07 |  -0.14 |   -0.08 |              0.10 |          0.09 |       -0.02 |    0.11 |     0.10 |     0.11 |     0.13 |  0.28 |  0.06 |        NA |   0.03 |
| Stroke               |            0.08 |   0.15 |     0.11 |      0.03 |  0.01 |   0.09 |   1.00 |                 0.26 |        -0.07 |  -0.03 |   -0.04 |              0.01 |          0.06 |        0.03 |    0.13 |     0.10 |     0.14 |     0.19 |  0.02 |  0.14 |        NA |  -0.07 |
| HeartDiseaseorAttack |            0.19 |   0.20 |     0.17 |      0.05 |  0.04 |   0.13 |   0.26 |                 1.00 |        -0.08 |  -0.06 |   -0.04 |             -0.01 |          0.09 |        0.03 |    0.20 |     0.10 |     0.18 |     0.23 |  0.06 |  0.20 |        NA |  -0.07 |
| PhysActivity         |           -0.06 |  -0.06 |    -0.06 |     -0.02 | -0.08 |  -0.07 |  -0.07 |                -0.08 |         1.00 |   0.13 |    0.13 |             -0.01 |         -0.02 |       -0.03 |   -0.17 |    -0.12 |    -0.17 |    -0.18 |  0.06 | -0.02 |        NA |   0.10 |
| Fruits               |           -0.02 |  -0.08 |    -0.07 |      0.00 | -0.03 |  -0.14 |  -0.03 |                -0.06 |         0.13 |   1.00 |    0.28 |             -0.04 |         -0.05 |       -0.02 |   -0.10 |    -0.06 |    -0.08 |    -0.09 | -0.08 | -0.01 |        NA |   0.12 |
| Veggies              |           -0.03 |  -0.08 |    -0.05 |     -0.02 | -0.03 |  -0.08 |  -0.04 |                -0.04 |         0.13 |   0.28 |    1.00 |             -0.03 |         -0.05 |       -0.03 |   -0.08 |    -0.06 |    -0.07 |    -0.10 | -0.03 | -0.07 |        NA |   0.07 |
| HvyAlcoholConsump    |           -0.06 |   0.02 |     0.02 |     -0.03 | -0.01 |   0.10 |   0.01 |                -0.01 |        -0.01 |  -0.04 |   -0.03 |              1.00 |         -0.02 |        0.03 |   -0.02 |     0.02 |    -0.01 |     0.00 |  0.11 | -0.06 |        NA |   0.07 |
| AnyHealthcare        |            0.10 |   0.16 |     0.08 |      0.10 | -0.02 |   0.09 |   0.06 |                 0.09 |        -0.02 |  -0.05 |   -0.05 |             -0.02 |          1.00 |       -0.26 |    0.03 |     0.02 |     0.09 |     0.13 | -0.01 |  0.37 |        NA |  -0.04 |
| NoDocbcCost          |            0.00 |   0.01 |     0.05 |     -0.01 |  0.03 |  -0.02 |   0.03 |                 0.03 |        -0.03 |  -0.02 |   -0.03 |              0.03 |         -0.26 |        1.00 |    0.14 |     0.15 |     0.13 |     0.08 | -0.04 | -0.20 |        NA |  -0.09 |
| GenHlth              |            0.26 |   0.23 |     0.22 |      0.08 |  0.10 |   0.11 |   0.13 |                 0.20 |        -0.17 |  -0.10 |   -0.08 |             -0.02 |          0.03 |        0.14 |    1.00 |     0.31 |     0.51 |     0.40 | -0.06 |  0.08 |        NA |  -0.26 |
| MentHlth             |            0.10 |   0.13 |     0.14 |      0.00 |  0.07 |   0.10 |   0.10 |                 0.10 |        -0.12 |  -0.06 |   -0.06 |              0.02 |          0.02 |        0.15 |    0.31 |     1.00 |     0.40 |     0.26 | -0.07 | -0.08 |        NA |  -0.15 |
| PhysHlth             |            0.16 |   0.18 |     0.17 |      0.04 |  0.08 |   0.11 |   0.14 |                 0.18 |        -0.17 |  -0.08 |   -0.07 |             -0.01 |          0.09 |        0.13 |    0.51 |     0.40 |     1.00 |     0.45 | -0.05 |  0.07 |        NA |  -0.19 |
| DiffWalk             |            0.20 |   0.25 |     0.22 |      0.07 |  0.17 |   0.13 |   0.19 |                 0.23 |        -0.18 |  -0.09 |   -0.10 |              0.00 |          0.13 |        0.08 |    0.40 |     0.26 |     0.45 |     1.00 | -0.11 |  0.20 |        NA |  -0.22 |
| Sex                  |           -0.04 |  -0.05 |    -0.06 |     -0.03 | -0.07 |   0.28 |   0.02 |                 0.06 |         0.06 |  -0.08 |   -0.03 |              0.11 |         -0.01 |       -0.04 |   -0.06 |    -0.07 |    -0.05 |    -0.11 |  1.00 |  0.01 |        NA |   0.21 |
| Age                  |            0.16 |   0.30 |     0.12 |      0.11 | -0.12 |   0.06 |   0.14 |                 0.20 |        -0.02 |  -0.01 |   -0.07 |             -0.06 |          0.37 |       -0.20 |    0.08 |    -0.08 |     0.07 |     0.20 |  0.01 |  1.00 |        NA |  -0.12 |
| Education            |              NA |     NA |       NA |        NA |    NA |     NA |     NA |                   NA |           NA |     NA |      NA |                NA |            NA |          NA |      NA |       NA |       NA |       NA |    NA |    NA |         1 |     NA |
| Income               |           -0.15 |  -0.14 |    -0.08 |     -0.06 | -0.05 |   0.03 |  -0.07 |                -0.07 |         0.10 |   0.12 |    0.07 |              0.07 |         -0.04 |       -0.09 |   -0.26 |    -0.15 |    -0.19 |    -0.22 |  0.21 | -0.12 |        NA |   1.00 |
=======
kable(summary(EducationData))
```

|     | Diabetes_binary | HighBP      | HighChol    | CholCheck   | BMI         | Smoker      | Stroke      | HeartDiseaseorAttack | PhysActivity | Fruits      | Veggies     | HvyAlcoholConsump | AnyHealthcare | NoDocbcCost | GenHlth     | MentHlth    | PhysHlth    | DiffWalk    | Sex         | Age         | Education   | Income      |
|:----|:----------------|:------------|:------------|:------------|:------------|:------------|:------------|:---------------------|:-------------|:------------|:------------|:------------------|:--------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|
|     | Min. : NA       | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA            | Min. : NA    | Min. : NA   | Min. : NA   | Min. : NA         | Min. : NA     | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA   | Min. : NA   |
|     | 1st Qu.: NA     | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA          | 1st Qu.: NA  | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA       | 1st Qu.: NA   | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA | 1st Qu.: NA |
|     | Median : NA     | Median : NA | Median : NA | Median : NA | Median : NA | Median : NA | Median : NA | Median : NA          | Median : NA  | Median : NA | Median : NA | Median : NA       | Median : NA   | Median : NA | Median : NA | Median : NA | Median : NA | Median : NA | Median : NA | Median : NA | Median : NA | Median : NA |
|     | Mean :NaN       | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN            | Mean :NaN    | Mean :NaN   | Mean :NaN   | Mean :NaN         | Mean :NaN     | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN   | Mean :NaN   |
|     | 3rd Qu.: NA     | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA          | 3rd Qu.: NA  | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA       | 3rd Qu.: NA   | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA | 3rd Qu.: NA |
|     | Max. : NA       | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA            | Max. : NA    | Max. : NA   | Max. : NA   | Max. : NA         | Max. : NA     | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA   | Max. : NA   |

``` r
# Create a correlation matrix between variables
Cor_Matrix <- EducationData %>% 
         select(Diabetes_binary, BMI, MentHlth, PhysHlth, Age, Income) %>%
         cor()

# Round the correlation matrix to two decimal places
rounded_Cor_Matrix <- round(Cor_Matrix, digits = 2)
# Print the rounded correlation matrix
kable(rounded_Cor_Matrix)
```

|                 | Diabetes_binary | BMI | MentHlth | PhysHlth | Age | Income |
|:----------------|----------------:|----:|---------:|---------:|----:|-------:|
| Diabetes_binary |              NA |  NA |       NA |       NA |  NA |     NA |
| BMI             |              NA |  NA |       NA |       NA |  NA |     NA |
| MentHlth        |              NA |  NA |       NA |       NA |  NA |     NA |
| PhysHlth        |              NA |  NA |       NA |       NA |  NA |     NA |
| Age             |              NA |  NA |       NA |       NA |  NA |     NA |
| Income          |              NA |  NA |       NA |       NA |  NA |     NA |
>>>>>>> joyz
