ST558_Project3
================
Hui and Joy
2023-11-02

# Introduction

Diabetes is a seriously pervasive chronic disease that disrupts the
body’s ability to regulate blood glucose levels, leading to a diminished
quality of life and reduced life expectancy. It stands as one of the
most prevalent chronic illnesses in the United States, impacting
millions of Americans annually and imposing a significant economic
burden on the nation.

In this project, we will use the **diabetes binary health indicators**
dataset obtained from the [Kaggle
website](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/)
to conduct comprehensive exploratory data analysis (EDA) and develop
predictive models. This dataset comprises 253,680 survey responses to
the CDC’s BRFSS (Behavioral Risk Factor Surveillance System) from year
2015. The primary target variable, **Diabetes_binary**, offers binary
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
model, a random forest model, a partial least squares model, as well as
a regularized logistic regression model. The performance of these models
will be rigorously evaluated using the test data set, and we will
determine the most effective model for predicting diabetes outcomes.

Description of variables in the data set:  
+ **Diabetes_binary:** 0 = no diabetes 1 = prediabetes or diabetes  
+ **HighBP:** High blood pressure  
+ **HighChol:** High cholesterol  
+ **CholCheck:** 0 = no cholesterol check in 5 years 1 = yes cholesterol
check in 5 years  
+ **BMI:** Body Mass Index  
+ **Smoker:** Have you smoked at least 100 cigarettes in your entire
life? 0 = no 1 = yes  
+ **Stroke:** 0 = no 1 = yes  
+ **HeartDiseaseorAttack:** coronary heart disease (CHD) or myocardial
infarction (MI) 0 = no 1 = yes  
+ **PhysActivity:** physical activity in past 30 days - not including
job 0 = no 1 = yes  
+ **Fruits:** Consume Fruit 1 or more times per day 0 = no 1 = yes  
+ **Veggies:** Consume Vegetables 1 or more times per day 0 = no 1 =
yes  
+ **HvyAlcoholConsump:** (adult men \>=14 drinks per week and adult
women\>=7 drinks per week) 0 = no 1 = yes  
+ **AnyHealthcare:** Health care coverage 0 = no 1 = yes  
+ **NoDocbcCost:** Was there a time in the past 12 months when you
needed to see a doctor but could not because of cost? 0 = no 1 = yes  
+ **GenHlth:** in general your health is: scale 1-5 1 = excellent 2 =
very good 3 = good 4 = fair 5 = poor  
+ **MentHlth:** days of poor mental health scale 1-30 days  
+ **PhysHlth:** physical illness or injury days in past 30 days scale
1-30  
+ **DiffWalk:** Do you have serious difficulty walking or climbing
stairs? 0 = no 1 = yes  
+ **Sex:** 0 = female 1 = male  
+ **Age:** 13-level age category 1 = 18-24 2 = 25-29 3 = 30-34 4 = 35-39
5 = 40-44 6 = 45-49 7 = 50-54 8 = 55-59 9 = 60-64 10 = 65-69 11 = 70-74
12 = 75-79 13 = 80 or older  
+ **Education:** scale 1-6 1 = Never attended school or only
kindergarten 2 = Grades 1 through 8 (Elementary) 3 = Grades 9 through 11
(Some high school) 4 = Grade 12 or GED (High school graduate) 5 =
College 1 year to 3 years (Some college or technical school) 6 = College
4 years or more (College graduate)  
+ **Income:** scale 1-8 1 = less than \$10,000 5 = less than \$35,000 8
= \$75,000 or more

# Data

## Read in data

``` r
library(dplyr)
library(readr)
diabetes <- xfun::cache_rds(
  as_tibble(read.csv("diabetes_binary_health_indicators_BRFSS2015.csv", header = TRUE)), 
  file = "diabetes.rds", 
  hash = file.mtime("diabetes_binary_health_indicators_BRFSS2015.csv"))
head(diabetes)
```

    ## # A tibble: 6 × 22
    ##   Diabetes_binary HighBP HighChol
    ##             <dbl>  <dbl>    <dbl>
    ## 1               0      1        1
    ## 2               0      0        0
    ## 3               0      1        1
    ## 4               0      1        0
    ## 5               0      1        1
    ## 6               0      1        1
    ## # ℹ 19 more variables: CholCheck <dbl>,
    ## #   BMI <dbl>, Smoker <dbl>,
    ## #   Stroke <dbl>,
    ## #   HeartDiseaseorAttack <dbl>,
    ## #   PhysActivity <dbl>, Fruits <dbl>,
    ## #   Veggies <dbl>,
    ## #   HvyAlcoholConsump <dbl>, …

## Group data based on education levels

``` r
library(xfun)
diabetes$Education <- ifelse(diabetes$Education %in% c(1, 2), "SomeElementary",
                         ifelse(diabetes$Education == 3, "SomeHighSchool", 
                              ifelse(diabetes$Education == 4, "HighSchool",
                                   ifelse(diabetes$Education == 5, "SomeCollege",
                                        ifelse(diabetes$Education == 6, "College", NA)
                                         )
                                    )
                               )
                            )

xfun::cache_rds(diabetes, file = "new_cached_data.rds")
```

    ## # A tibble: 253,680 × 22
    ##    Diabetes_binary HighBP HighChol
    ##              <dbl>  <dbl>    <dbl>
    ##  1               0      1        1
    ##  2               0      0        0
    ##  3               0      1        1
    ##  4               0      1        0
    ##  5               0      1        1
    ##  6               0      1        1
    ##  7               0      1        0
    ##  8               0      1        1
    ##  9               1      1        1
    ## 10               0      0        0
    ## # ℹ 253,670 more rows
    ## # ℹ 19 more variables: CholCheck <dbl>,
    ## #   BMI <dbl>, Smoker <dbl>,
    ## #   Stroke <dbl>,
    ## #   HeartDiseaseorAttack <dbl>,
    ## #   PhysActivity <dbl>, Fruits <dbl>,
    ## #   Veggies <dbl>, …

``` r
params$Edu
```

    ## [1] "SomeCollege"

## Subset the data based on education levels

``` r
# Subset the data based on parameter `Edu`
  EducationData <- filter(diabetes, 
                          (Education == params$Edu)
                          )
```

# Explanatory Data Analyzes

## Check missing values

``` r
  sum(is.na(EducationData))
```

    ## [1] 0

## Check the structure of **EducationData**

``` r
  str(EducationData)
```

    ## tibble [69,910 × 22] (S3: tbl_df/tbl/data.frame)
    ##  $ Diabetes_binary     : num [1:69910] 0 1 0 0 1 0 1 1 0 0 ...
    ##  $ HighBP              : num [1:69910] 1 1 1 0 0 0 1 1 0 0 ...
    ##  $ HighChol            : num [1:69910] 1 1 1 0 0 0 0 1 0 0 ...
    ##  $ CholCheck           : num [1:69910] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ BMI                 : num [1:69910] 24 30 34 26 23 28 27 34 22 26 ...
    ##  $ Smoker              : num [1:69910] 0 1 1 1 1 1 0 1 0 1 ...
    ##  $ Stroke              : num [1:69910] 0 0 0 0 0 0 0 1 0 0 ...
    ##  $ HeartDiseaseorAttack: num [1:69910] 0 1 0 0 0 0 0 0 0 0 ...
    ##  $ PhysActivity        : num [1:69910] 1 0 0 0 1 0 1 1 1 1 ...
    ##  $ Fruits              : num [1:69910] 1 1 1 0 0 0 1 0 1 1 ...
    ##  $ Veggies             : num [1:69910] 1 1 1 1 0 1 1 0 1 1 ...
    ##  $ HvyAlcoholConsump   : num [1:69910] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AnyHealthcare       : num [1:69910] 1 1 1 1 1 1 1 1 1 0 ...
    ##  $ NoDocbcCost         : num [1:69910] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ GenHlth             : num [1:69910] 2 5 3 3 2 3 1 4 2 1 ...
    ##  $ MentHlth            : num [1:69910] 3 30 0 0 0 0 0 0 0 0 ...
    ##  $ PhysHlth            : num [1:69910] 0 30 30 15 0 7 0 7 10 1 ...
    ##  $ DiffWalk            : num [1:69910] 0 1 1 0 0 0 0 1 0 0 ...
    ##  $ Sex                 : num [1:69910] 0 0 0 0 1 1 0 0 0 1 ...
    ##  $ Age                 : num [1:69910] 11 9 10 7 7 5 13 9 12 4 ...
    ##  $ Education           : chr [1:69910] "SomeCollege" "SomeCollege" "SomeCollege" "SomeCollege" ...
    ##  $ Income              : num [1:69910] 4 1 1 7 6 5 4 4 7 3 ...

## Summary statistics for numeric variables

``` r
library(dplyr)

# Filter only numeric variables
  numeric_vars <- EducationData %>% select_if(is.numeric) 

# Create a summary table
  summary_table <- numeric_vars %>%
    sapply(function(x) {
      c( count = sum(!is.na(x)),
        mean = mean(x, na.rm = TRUE),
        std = sd(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        Q1 = quantile(x, 0.25, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        Q3 = quantile(x, 0.75, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
      )
    })
  
# Transpose the summary table
  transposed_summary_table <- t(round(summary_table, 2)
                                ) 
  colnames(transposed_summary_table) <- c("count", "mean", "std", 
                                        "min", "Q1", "median", "Q3", "max")
# Print the transposed table
  knitr::kable(transposed_summary_table)
```

|                      | count |  mean |  std | min |  Q1 | median |  Q3 | max |
|:---------------------|------:|------:|-----:|----:|----:|-------:|----:|----:|
| Diabetes_binary      | 69910 |  0.15 | 0.36 |   0 |   0 |      0 |   0 |   1 |
| HighBP               | 69910 |  0.44 | 0.50 |   0 |   0 |      0 |   1 |   1 |
| HighChol             | 69910 |  0.42 | 0.49 |   0 |   0 |      0 |   1 |   1 |
| CholCheck            | 69910 |  0.96 | 0.20 |   0 |   1 |      1 |   1 |   1 |
| BMI                  | 69910 | 28.88 | 6.76 |  12 |  24 |     28 |  32 |  98 |
| Smoker               | 69910 |  0.49 | 0.50 |   0 |   0 |      0 |   1 |   1 |
| Stroke               | 69910 |  0.04 | 0.20 |   0 |   0 |      0 |   0 |   1 |
| HeartDiseaseorAttack | 69910 |  0.10 | 0.30 |   0 |   0 |      0 |   0 |   1 |
| PhysActivity         | 69910 |  0.74 | 0.44 |   0 |   0 |      1 |   1 |   1 |
| Fruits               | 69910 |  0.62 | 0.49 |   0 |   0 |      1 |   1 |   1 |
| Veggies              | 69910 |  0.81 | 0.39 |   0 |   1 |      1 |   1 |   1 |
| HvyAlcoholConsump    | 69910 |  0.06 | 0.23 |   0 |   0 |      0 |   0 |   1 |
| AnyHealthcare        | 69910 |  0.95 | 0.23 |   0 |   1 |      1 |   1 |   1 |
| NoDocbcCost          | 69910 |  0.10 | 0.30 |   0 |   0 |      0 |   0 |   1 |
| GenHlth              | 69910 |  2.58 | 1.05 |   1 |   2 |      2 |   3 |   5 |
| MentHlth             | 69910 |  3.65 | 7.93 |   0 |   0 |      0 |   2 |  30 |
| PhysHlth             | 69910 |  4.68 | 9.13 |   0 |   0 |      0 |   4 |  30 |
| DiffWalk             | 69910 |  0.19 | 0.39 |   0 |   0 |      0 |   0 |   1 |
| Sex                  | 69910 |  0.41 | 0.49 |   0 |   0 |      0 |   1 |   1 |
| Age                  | 69910 |  7.90 | 3.10 |   1 |   6 |      8 |  10 |  13 |
| Income               | 69910 |  5.88 | 2.00 |   1 |   5 |      6 |   8 |   8 |

The center and spread of each variable can be found in this table.

## Correlation between `Diabetes` and the other variables

``` r
library(knitr)
# Create a correlation matrix between variables
  Cor_Matrix <- EducationData %>% 
           select(Diabetes_binary, BMI, MentHlth, PhysHlth, Age, Income) %>%
           cor()

# Round the correlation matrix to two decimal places
  rounded_Cor_Matrix <- round(Cor_Matrix, digits = 2)
  
# Print the rounded correlation matrix
  kable(rounded_Cor_Matrix)
```

|                 | Diabetes_binary |   BMI | MentHlth | PhysHlth |   Age | Income |
|:----------------|----------------:|------:|---------:|---------:|------:|-------:|
| Diabetes_binary |            1.00 |  0.21 |     0.06 |     0.16 |  0.18 |  -0.12 |
| BMI             |            0.21 |  1.00 |     0.09 |     0.13 | -0.06 |  -0.06 |
| MentHlth        |            0.06 |  0.09 |     1.00 |     0.36 | -0.11 |  -0.21 |
| PhysHlth        |            0.16 |  0.13 |     0.36 |     1.00 |  0.09 |  -0.25 |
| Age             |            0.18 | -0.06 |    -0.11 |     0.09 |  1.00 |  -0.08 |
| Income          |           -0.12 | -0.06 |    -0.21 |    -0.25 | -0.08 |   1.00 |

## Visualization of correlation

``` r
  library(ggplot2)

# Exclude the character variable from the data
  data <- EducationData[, !(names(EducationData) %in% "Education")]

# Calculate the correlation of each variable with 'Diabetes_binary'
  correlations <- sapply(data[-1], function(x) cor(x, data$Diabetes_binary))

# Create a data frame for the correlations
  correlation_data <- data.frame(Variable = names(correlations), 
                                 Correlation = correlations)

# Create a bar chart of correlations
  ggplot(correlation_data, aes(x = Variable, y = Correlation)) +
    geom_bar(stat = "identity", fill = "orange") +
    labs(title = "Correlation with Diabetes_binary", 
         x = "Variable", 
         y = "Correlation") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
```

![](SomeCollege_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

In this chart, some of the variables exhibited positive correlations
with `Diabetes_binary`, while others showed negative correlations with
it.

## Convert variables to factor

``` r
  EducationData$Diabetes_binary <- factor(EducationData$Diabetes_binary, 
                                         levels = c(0, 1),
                                         labels = c("NonDiabetes", "Diabetes"))
  EducationData$Education <- as.factor(EducationData$Education)
  EducationData$HighBP <- factor(EducationData$HighBP,
                                 levels = c(0, 1),
                                 labels = c("NonHighBP", "HighBP"))
  EducationData$HighChol <- factor(EducationData$HighChol,levels = c(0, 1),
                                   labels = c("NonHighChol", "HighCholesterol"))
  EducationData$Sex <- factor(EducationData$Sex,levels = c(0, 1), 
                              labels = c("Female", "Male"))
  EducationData$Fruits <- factor(EducationData$Fruits,levels = c(0, 1), 
                                labels = c("No", "Yes"))
  EducationData$Veggies <- factor(EducationData$Veggies,levels = c(0, 1),
                                  labels = c("No", "Yes"))
  EducationData$CholCheck <- as.factor(EducationData$CholCheck)
  EducationData$Smoker <- as.factor(EducationData$Smoker)
  EducationData$Stroke <- as.factor(EducationData$Stroke)
  EducationData$HeartDiseaseorAttack <- as.factor(EducationData$HeartDiseaseorAttack)
  EducationData$PhysActivity <- factor(EducationData$PhysActivity, levels = c(0, 1),
                                         labels = c("No", "Yes"))
  EducationData$HvyAlcoholConsump <- as.factor(EducationData$HvyAlcoholConsump)
  EducationData$AnyHealthcare <- as.factor(EducationData$AnyHealthcare)
  EducationData$NoDocbcCost <- as.factor(EducationData$NoDocbcCost)
  EducationData$DiffWalk <- factor(EducationData$DiffWalk, levels = c(0, 1),
                                         labels = c("No", "Yes"))
```

## Summaries for character variables

``` r
library(knitr)
# one-way table
  kable(table(EducationData$Diabetes_binary))
```

| Var1        |  Freq |
|:------------|------:|
| NonDiabetes | 59556 |
| Diabetes    | 10354 |

``` r
  kable(table(EducationData$HighBP))
```

| Var1      |  Freq |
|:----------|------:|
| NonHighBP | 39019 |
| HighBP    | 30891 |

``` r
# two-way table
  kable(table(EducationData$Diabetes_binary, EducationData$HighChol))
```

|             | NonHighChol | HighCholesterol |
|:------------|------------:|----------------:|
| NonDiabetes |       36855 |           22701 |
| Diabetes    |        3368 |            6986 |

``` r
  kable(table(EducationData$Diabetes_binary, EducationData$Fruits))
```

|             |    No |   Yes |
|:------------|------:|------:|
| NonDiabetes | 22335 | 37221 |
| Diabetes    |  4268 |  6086 |

``` r
  kable(table(EducationData$Diabetes_binary, EducationData$Veggies))
```

|             |    No |   Yes |
|:------------|------:|------:|
| NonDiabetes | 10817 | 48739 |
| Diabetes    |  2319 |  8035 |

``` r
  kable(table(EducationData$Diabetes_binary, EducationData$Sex))
```

|             | Female |  Male |
|:------------|-------:|------:|
| NonDiabetes |  35539 | 24017 |
| Diabetes    |   5683 |  4671 |

``` r
# Three-way table
  kable(table(EducationData$Diabetes_binary, EducationData$HighBP, EducationData$Sex))
```

| Var1        | Var2      | Var3   |  Freq |
|:------------|:----------|:-------|------:|
| NonDiabetes | NonHighBP | Female | 22486 |
| Diabetes    | NonHighBP | Female |  1427 |
| NonDiabetes | HighBP    | Female | 13053 |
| Diabetes    | HighBP    | Female |  4256 |
| NonDiabetes | NonHighBP | Male   | 14013 |
| Diabetes    | NonHighBP | Male   |  1093 |
| NonDiabetes | HighBP    | Male   | 10004 |
| Diabetes    | HighBP    | Male   |  3578 |

## Graphical summaries for main predictors

### Bar plots

``` r
library(ggplot2)
library(cowplot)

# Checking The relation B/W HighBP and Diabetes
  g1 <- ggplot(data = EducationData, aes(x = HighBP, y = after_stat(count), 
                                         fill = Diabetes_binary)) +
               geom_bar(position = "dodge") +
               labs(title = "Diabetes Frequency for High Blood Pressure",
                    x = "High Blood Pressure",
                    y = "Frequency") +
              theme(text = element_text(size = 8)
                    ) 
# Checking The relation B/W HighChol and Diabetes
  g2 <- ggplot(data = EducationData, aes(x = HighChol, y = after_stat(count), 
                                         fill = Diabetes_binary)) +
               geom_bar(position = "dodge") +
               labs(title = "Diabetes Frequency for High Cholesterol",
                    x = "High Cholesterol",
                    y = "Frequency") +
              theme(legend.position = "none",  # Hide the legend
                    text = element_text(size = 8)
                    ) 
# Checking The relation B/W DiffWalk and Diabetes
  g3 <- ggplot(data = EducationData, aes(x = DiffWalk, y = after_stat(count), 
                                         fill = Diabetes_binary)) +
               geom_bar(position = "dodge") +
               labs(title = "Diabetes Frequency for Difficult Walking",
                    x = "Difficult Walking",
                    y = "Frequency") + 
              theme(legend.position = "none",  # Hide the legend
                    text = element_text(size = 8)
                    ) 
# Checking The relation B/W Smoker and Diabetes
  g4 <- ggplot(data = EducationData, aes(x = Smoker, y = after_stat(count), 
                                         fill = Diabetes_binary)) +
               geom_bar(position = "dodge") +
               labs(title = "Diabetes Frequency for Smoker",
                    x = "Smoker",
                    y = "Frequency") +
              theme(legend.position = "none",  # Hide the legend
                    text = element_text(size = 8)
                    )            
# Arrange the plots into a 2x2 gird using cowplot
  combined_plots <- plot_grid(g1, g2, g3, g4,
                              ncol = 2,
                              labels = c("a", "b", "c", "d"))
# Print the combined plots
  combined_plots
```

<img src="SomeCollege_files/figure-gfm/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

``` r
library(ggplot2)

# Define labels for 'GenHlth'
  genhlth_labels <- c("Excellent", "Very Good", "Good", "Fair", "Poor")

# Create a plot with two facets (subplots)
  g5 <- ggplot(EducationData, aes(x = factor(GenHlth, levels = 1:5, 
                                             labels = genhlth_labels), 
                                 fill = Diabetes_binary)) +
                geom_bar(position = 'dodge', alpha = 0.5, width = 0.5) +
                facet_wrap(~ Diabetes_binary, ncol = 2) +
                labs(title = 'General Health Conditions Distribution', x = NULL, y = 'Count') +
                theme_minimal() +
                theme(legend.title = element_blank()) +
                theme(legend.position = "none", axis.text.x = element_text(size = 8)
                      )
# Print g5
  g5
```

<img src="SomeCollege_files/figure-gfm/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

BMI distribution

``` r
  g6 <- ggplot(data = EducationData, aes(x = BMI, y = after_stat(count), 
                                         fill = Diabetes_binary)) +
               geom_bar() +
               labs(title = "BMI distribution",
                    x = "BMI",
                    y = "Count")  + theme_minimal() +
              theme(text = element_text(size = 8),
                    legend.position = c(.85, .85)
              )
# Print g6
  g6
```

<img src="SomeCollege_files/figure-gfm/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

### Box plots

``` r
library(gridExtra)

  g7 <- ggplot(EducationData, aes(x = Diabetes_binary, y = BMI, 
                                  fill = Diabetes_binary)) +
       geom_boxplot() +
       labs(title = "BMI Distribution vs. Diabetes",
         x = " ",
         y = "BMI") +
       theme(text = element_text(size = 8),
             legend.position = "none"
             ) 
  g8 <- ggplot(EducationData, aes(x = Diabetes_binary, y = Age, 
                                  fill = Diabetes_binary)) +
        geom_boxplot() +
        labs(title = "Age Distribution vs. Diabetes",
             x = " ",
             y = "Age") +
        theme(text = element_text(size = 8),
              legend.position = "none"
              ) 
  g9 <- ggplot(EducationData, aes(x = Diabetes_binary, y = Income, 
                                  fill = Diabetes_binary)) +
        geom_boxplot() +
        labs(title = "Income Distribution vs. Diabetes",
             x = " ",
             y = "Income") +
        theme(text = element_text(size = 8),
              legend.position = "none"
              )
  g10 <- ggplot(EducationData, aes(x = Diabetes_binary, y = MentHlth, 
                                   fill = Diabetes_binary)) +
         geom_boxplot() +
          labs(title = "Mental Health Distribution vs. Diabetes",
          x = " ",
          y = "Mental Health") +
          theme(text = element_text(size = 8),
                legend.position = "none"
                ) 
# Arrange the plots into a 2x2 gird using cowplot
  complots <- plot_grid(g7, g8, g9, g10, ncol = 2,
                        labels = c("a", "b", "c", "d")
                        )
# Print the combined plots
  complots
```

<img src="SomeCollege_files/figure-gfm/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

### Density plots

``` r
  g11 <- ggplot(EducationData, aes(x = Income)) +
            geom_density(adjust = 0.5, alpha = 0.5, 
                          aes(fill = Diabetes_binary), position = "stack") +
            labs(title = "Distribution of Income vs Diabetes",
                 x = "Income",
                 y = "Density") +
            theme(legend.position = "none", 
                  text = element_text(size = 8)
                  )
    
  g12 <- ggplot(EducationData, aes(x = Age)) +
            geom_density(adjust = 0.5, alpha = 0.5, 
                          aes(fill = Diabetes_binary), position = "stack") +
            labs(title = "Distribution of Age vs Diabetes",
                 x = "Age",
                 y = "Density") +
            theme(text = element_text(size = 8),
                  legend.position = c(.20, .85)
                  )
# Arrange the plots into a 2x2 gird using cowplot
  complots <- plot_grid(g11, g12, ncol = 2,
                        labels = c("a", "b")
                        )
# Print the combined plots
  complots
```

<img src="SomeCollege_files/figure-gfm/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

# Modeling

## Split the data

``` r
library(caret)
library(lattice)

# Set seed for reproducibility
  set.seed(110)

# Create a data partition for training (70%) and testing (30%) data
  intrain <- createDataPartition(y = EducationData$Diabetes_binary,
                                 p = 0.70,
                                 list = FALSE
                                 )
# Create the training data set using the partition
  train_set <- EducationData[intrain,]

# Create the testing data set using he partition
  test_set <- EducationData[-intrain,]
```

## Summarize and fit models

### What is **log loss** and why may we prefer it to things like accuracy?

**Log loss**, also known as logistic loss or cross-entropy loss, is a
loss function used in classification problems to evaluate the
performance of a model, especially in scenarios with binary response
variables. It quantifies the performance of a model by measuring the
difference between the predicted probability distribution produced by
the model and the actual probability distribution. Log loss is
calculated as the negative logarithm of the predicted probability
assigned to the true class label. It penalizes the model for incorrect
predictions and a lower log loss value indicates better model predictive
performance.

**Accuracy**, on the other hand, is a metric used to evaluate
classification model performance by measuring the percentage of
correctly classified instances. While accuracy is a useful metric, it
can be misleading in some cases. For example, in imbalanced data sets
where one class significantly outweighs the other, a model that always
predicts the majority class will yield high accuracy but may not be
practically useful. In such cases, log loss can offer a more reliable
metric as it considers the model’s predictions uncertainty.

In summary, log loss is a preferred metric over accuracy when the data
is imbalanced or when the cost of false positives and false negatives is
different.

### Logistic Regression Model

- What is a **logistic regression** and why we apply it to this kind of
  data?

A **logistic regression model** is a statistical method used for
analyzing the relationship between a binary outcome variable and one or
more predictor variables. It is commonly used in classification
problems, where the goal is to predict one of two possible outcomes,
typically represented as 0 and 1 or “Yes” and “No”.

The relationship between response variable and the predictor variables
is evaluated using the logistic function (also known as the sigmoid
function), which ensures that the predicted probabilities fall between 0
and 1. The model parameters are estimated using a process called maximum
likelihood estimation. The key output of a logistic regression model
includes the coefficients (log-odds) associated with each predictor
variable, which describe the strength and direction of their influence
on the probability of the outcome.

Logistic regression is designed to handle categorical dependent
variables. In our situation, Diabetes_binary is a binary variable and is
suitable for fitting logistic regression model.

- **Fit Logistic Regression Models**

``` r
library(caret)

# Set seed for reproducibility
  set.seed(111)

# Fit the logistic regression models
  log_reg1 <- train(Diabetes_binary ~ HighBP + HighChol + CholCheck + 
                      Smoker + Fruits + Veggies + Sex + Income, 
                 data = train_set, 
                 family = "binomial",
                 method = "glm",
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "repeatedcv",
                                          number = 5,
                                          repeats = 3,
                                          classProbs = TRUE,
                                          summaryFunction = mnLogLoss),
                 metric = "logLoss",  
                 trace = FALSE
                 )

  log_reg2 <- train(Diabetes_binary ~ HighBP*HighChol*CholCheck + 
                      Smoker + Fruits + Veggies + Sex + Income, 
                 data = train_set, 
                 family = "binomial",
                 method = "glm",
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "repeatedcv",
                                          number = 5,
                                          repeats = 3,
                                          classProbs = TRUE,
                                          summaryFunction = mnLogLoss),
                 metric = "logLoss",  
                 trace = FALSE
                 )

  log_reg3 <- train(Diabetes_binary ~ HighBP + HighChol + 
                      CholCheck + Smoker, 
                 data = train_set, 
                 family = "binomial",
                 method = "glm",
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 5,
                                          repeats = 3,
                                          classProbs = TRUE,
                                          summaryFunction = mnLogLoss),
                 metric = "logLoss",  
                 trace = FALSE
                 )

# Print the model results
  log_reg1
```

    ## Generalized Linear Model 
    ## 
    ## 48938 samples
    ##     8 predictor
    ##     2 classes: 'NonDiabetes', 'Diabetes' 
    ## 
    ## Pre-processing: centered (8), scaled (8) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 39150, 39151, 39150, 39151, 39150, 39150, ... 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.3694467

``` r
  log_reg2
```

    ## Generalized Linear Model 
    ## 
    ## 48938 samples
    ##     8 predictor
    ##     2 classes: 'NonDiabetes', 'Diabetes' 
    ## 
    ## Pre-processing: centered (12),
    ##  scaled (12) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 39150, 39151, 39150, 39150, 39151, 39151, ... 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.3692628

``` r
  log_reg3
```

    ## Generalized Linear Model 
    ## 
    ## 48938 samples
    ##     4 predictor
    ##     2 classes: 'NonDiabetes', 'Diabetes' 
    ## 
    ## Pre-processing: centered (4), scaled (4) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 39150, 39150, 39151, 39150, 39151, 39151, ... 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.3735997

Based on the model fitting results, we select the one with lower log
loss value.

### LASSO Logistic Regression Model

- What is a **LASSO logistic regression** and why we might try to use it
  over basic logistic regression?

**LASSO** (`least absolute shrinkage and selection operator`) is a
regression analysis method that performs both variable selection and
regularization to enhance the prediction accuracy and interpretability
of the resulting statistical model. It uses `glmnet` package in R to fit
the model.

The primary goal of LASSO regression is to find a balance between model
simplicity and accuracy. It achieves this by adding a penalty term to
the traditional linear regression model, which encourages sparse
solutions where some coefficients are forced to be exactly zero. This
feature makes LASSO particularly useful for feature selection, as it can
automatically identify and discard irrelevant or redundant variables.
That’s why we might try to use LASSO logistic regression over basic
logistic regression.

- **Fit a LASSO Logistic Regression Model**

``` r
library(glmnet)
library(Metrics)

# Set seed for reproducibility
  set.seed(112)

# Select variables for fitting the model
  predictors <- c("HighBP", "HighChol", "CholCheck", 
                  "BMI", "Smoker", "Stroke", "PhysActivity",
                  "HvyAlcoholConsump", "GenHlth", "MentHlth", 
                  "PhysHlth", "Sex", "Age", "Income"
                  )

# Create a train control object for cross-validation
  ctrl <- trainControl(method = "repeatedcv", # Cross-validation method (k-fold)
                       number = 5, # Number of cross-validation folds
                       repeats = 3,
                       verboseIter = FALSE,
                       classProbs = TRUE,
                       summaryFunction = mnLogLoss
                       )
                      

 lambdas <- c(seq(0, 2, length = 3))

# Fit a Lasso model
  lasso_fit <- train(Diabetes_binary ~ .,
                     data = train_set[, c("Diabetes_binary", predictors)],
                     method = "glmnet",
                     trControl = ctrl,
                     tuneGrid = expand.grid(alpha = 1, lambda = 0), # Alpha = 1 for LASSO
                     metric = "logLoss" # Use logLoss metric for evaluation
                     )

# Predict probabilities on the test set
  predicted_lasso <- predict(lasso_fit, newdata = test_set)

# Print model results
  lasso_fit
```

    ## glmnet 
    ## 
    ## 48938 samples
    ##    14 predictor
    ##     2 classes: 'NonDiabetes', 'Diabetes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 39151, 39150, 39150, 39150, 39151, 39151, ... 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.3358909
    ## 
    ## Tuning parameter 'alpha' was
    ## 
    ## Tuning parameter 'lambda' was
    ##  held constant at a value of 0

### Classification Tree Model

- What is a **classification tree model** and why we might try to use
  it?

A **classification tree model** is a type of supervised learning
algorithm used in statistics, data mining, and machine learning. It is
used to predict the value of a categorical response variable based on
one or more predictor variables. The model is built by recursively
partitioning the data into smaller and smaller subsets, with each
partition being based on a different predictor variable. The result is a
tree-like structure where each internal node represents a test on a
predictor variable, each branch represents the outcome of the test, and
each leaf node represents a predicted value for the response variable.
The tree is constructed in such a way that the most important predictor
variables are at the top of the tree, and the least important ones are
at the bottom. The goal of the algorithm is to create a tree that is as
small as possible while still accurately predicting the response
variable.

Classification trees are a powerful and flexible machine learning
algorithm that can be used for both classification and regression
problems. They are particularly useful when the data has non-linear
relationships, variable interactions, or outliers, and when
interpretability is important. However, classification trees also have
limitations, such as being prone to overfitting when they become too
complex. To address, ensemble techniques like random forests and
gradient boosting are often used to to enhance their performance.

- **Fit a Classification Tree Model**

``` r
library(rpart)

# Set a seed for reproducibility
  set.seed(123)

# Set trainControl parameters
  trctrl <- trainControl(method = "repeatedcv",
                         number = 5,
                         repeats = 3,
                         classProbs = TRUE,
                         summaryFunction = mnLogLoss
                         )
# Create a tuneGrid with tuning value of cp 0, 0.001 0.002,..., 0.1
  tuneGrid <- expand.grid(cp = seq(0, 0.025, by = 0.001))

# Fit the classification model with log loss as the metric
  class_fit <- train(Diabetes_binary ~.,
                     #data = train_set,
                     data = train_set[, c("Diabetes_binary", predictors)],
                     method = "rpart",
                     trControl = trctrl,
                     preProcess = c("center", "scale"),
                     # Preprocess data by centering and scaling
                     tuneGrid = tuneGrid,
                     metric = "logLoss"
                     )

# Check the model result
  class_fit
```

    ## CART 
    ## 
    ## 48938 samples
    ##    14 predictor
    ##     2 classes: 'NonDiabetes', 'Diabetes' 
    ## 
    ## Pre-processing: centered (14),
    ##  scaled (14) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 39150, 39151, 39150, 39151, 39150, 39151, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp     logLoss  
    ##   0.000  0.4350732
    ##   0.001  0.3691348
    ##   0.002  0.3695192
    ##   0.003  0.3695077
    ##   0.004  0.3726101
    ##   0.005  0.3792438
    ##   0.006  0.4032535
    ##   0.007  0.4129503
    ##   0.008  0.4194092
    ##   0.009  0.4194092
    ##   0.010  0.4194092
    ##   0.011  0.4194092
    ##   0.012  0.4194092
    ##   0.013  0.4194092
    ##   0.014  0.4194092
    ##   0.015  0.4194092
    ##   0.016  0.4194092
    ##   0.017  0.4194092
    ##   0.018  0.4194092
    ##   0.019  0.4194092
    ##   0.020  0.4194092
    ##   0.021  0.4194092
    ##   0.022  0.4194092
    ##   0.023  0.4194092
    ##   0.024  0.4194092
    ##   0.025  0.4194092
    ## 
    ## logLoss was used to select the
    ##  optimal model using the
    ##  smallest value.
    ## The final value used for the model
    ##  was cp = 0.001.

``` r
# Plot the results of accuracy vs k-value
  plot(class_fit)
```

![](SomeCollege_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### Random Forest

- What is a **random forest** and why we might use it instead of a basic
  classification tree?

A **random forest regression model**, also known as a random forest, is
a versatile machine learning algorithm used for both regression and
classification tasks. It is an ensemble technique that uses multiple
decision trees to make predictions. In this model, each decision tree is
trained on a different subset of the data, and the final output is the
average of all the outputs of the individual decision trees. This
technique helps to reduce overfitting and improve the accuracy of the
model. Random forest can predict continuous values, such as stock
prices, temperature, or sales figures, as well as performing
classification variables.

Random forests have several advantages over the basic decision trees:
reduced overfitting, improved accuracy, less sensitive to outliers, and
identifying the most important predictors and improving the
interpretability of the model. Therefore, we prefer random forest over
basic classification trees.

- **Fit a Random Forest Model**

``` r
# fit a random forest model
  rf_fit <- train(Diabetes_binary ~ HighBP + HighChol + 
                  BMI + GenHlth + PhysHlth + DiffWalk, 
                  data = train_set, 
                  method = "rf",
                  trControl = trainControl(method = "repeatedcv", 
                                           number = 5,
                                           repeats = 3,
                                           classProbs = TRUE,
                                           summaryFunction = mnLogLoss),
                  preProcess = c("center", "scale"), 
                  metric = "logLoss",
                  tuneGrid = data.frame(mtry = 1:6))

# Print and visualize the model results  
  rf_fit
```

    ## Random Forest 
    ## 
    ## 48938 samples
    ##     6 predictor
    ##     2 classes: 'NonDiabetes', 'Diabetes' 
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 39151, 39150, 39150, 39151, 39150, 39151, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  logLoss 
    ##   1     2.620439
    ##   2     2.186015
    ##   3     1.990874
    ##   4     1.953045
    ##   5     1.928715
    ##   6     1.963036
    ## 
    ## logLoss was used to select the
    ##  optimal model using the
    ##  smallest value.
    ## The final value used for the model
    ##  was mtry = 5.

``` r
  plot(rf_fit)
```

![](SomeCollege_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

### Partial Least Squares Model

- A summary of **partial least squares model**

A **partial least squares (PLS) model** is a supervised learning
algorithm used to model the relationship between two sets of variables.
The PLS model is used when there are multicollinearity, i.e., the
explanatory variables are correlated, and high dimensionality in the
data. PLS allows us to reduce the dimensionality of correlated variables
and model the underlying, shared, information of those variables (in
both dependent and independent variables). Moreover, PLS can model
multiple outcome variables, which many statistics and machine learning
models cannot directly deal with.

- **Fit a Partial Least Squares Model**

``` r
# load the required library
library(pls)

# fit partial least squares model
  pls_fit <- train(Diabetes_binary ~ ., 
                  data = train_set[, c("Diabetes_binary", predictors)], 
                  method = "pls",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 5,
                                           repeats = 3,
                                           classProbs = TRUE,
                                           summaryFunction = mnLogLoss), 
                  preProcess = c("center", "scale"), 
                  metric = "logLoss",
                  tuneGrid = data.frame(ncomp = 1:14)
                  )

# Print and visualize the model results  
  pls_fit
```

    ## Partial Least Squares 
    ## 
    ## 48938 samples
    ##    14 predictor
    ##     2 classes: 'NonDiabetes', 'Diabetes' 
    ## 
    ## Pre-processing: centered (14),
    ##  scaled (14) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 39151, 39151, 39150, 39150, 39150, 39151, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   ncomp  logLoss  
    ##    1     0.4778994
    ##    2     0.4759034
    ##    3     0.4756997
    ##    4     0.4756425
    ##    5     0.4756359
    ##    6     0.4756355
    ##    7     0.4756355
    ##    8     0.4756355
    ##    9     0.4756355
    ##   10     0.4756355
    ##   11     0.4756355
    ##   12     0.4756355
    ##   13     0.4756355
    ##   14     0.4756355
    ## 
    ## logLoss was used to select the
    ##  optimal model using the
    ##  smallest value.
    ## The final value used for the model
    ##  was ncomp = 7.

``` r
  plot(pls_fit)
```

![](SomeCollege_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### Regularized Logistic Regression Model

- A summary of **regularized logistic regression**

A **regularized logistic regression model** is a variation of logistic
regression that includes regularization techniques to prevent
overfitting and improve the model’s generalization ability. It’s used
for binary classification problems, where the goal is to predict one of
two possible outcomes.

Regularized logistic regression is particularly useful when dealing with
high-dimensional data with many predictor variables. It helps to improve
model performance, maintain model simplicity, and select the most
relevant features. It’s commonly used in machine learning and data
analysis when dealing with classification problems, especially when
overfitting is a concern.

- **Fit a Regularized Logistic Regression Model**

``` r
# Load required libraries
library(caret)
library(glmnet)
library(LiblineaR)
library(Metrics)

# Select variables for fitting the model
  predictors <- c("HighBP", "HighChol", "CholCheck", 
                  "BMI", "Smoker", "Stroke", "PhysActivity",
                  "HvyAlcoholConsump", "GenHlth", "MentHlth", 
                  "PhysHlth", "Sex", "Age", "Income"
                  )

# Create a grid of values for cost, loss, and epsilon
  tuning_grid <- expand.grid(cost = c(0.01, 0.1, 1, 10),
                             loss = "L1",
                             epsilon = c(0.001, 0.01, 0.1)
                             )

# Fit a regularized logistic regression model
  reglog_fit <- train(Diabetes_binary ~ .,
                      data = train_set[, c("Diabetes_binary", predictors)],
                      method = "regLogistic",
                      trControl = trainControl(method = "repeatedcv",
                                               number = 5,
                                               repeats = 3,
                                               classProbs = TRUE,
                                               summaryFunction = mnLogLoss), 
                      preProcess = c("center", "scale"), 
                      metric = "logLoss",
                      tuneGrid = tuning_grid
                      )  

# Print and visualize the model results  
  reglog_fit  
```

    ## Regularized Logistic Regression 
    ## 
    ## 48938 samples
    ##    14 predictor
    ##     2 classes: 'NonDiabetes', 'Diabetes' 
    ## 
    ## Pre-processing: centered (14),
    ##  scaled (14) 
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 39151, 39150, 39151, 39150, 39150, 39151, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cost   epsilon  logLoss  
    ##    0.01  0.001    0.3365606
    ##    0.01  0.010    0.3365940
    ##    0.01  0.100    0.3371212
    ##    0.10  0.001    0.3359827
    ##    0.10  0.010    0.3359832
    ##    0.10  0.100    0.3361746
    ##    1.00  0.001    0.3359824
    ##    1.00  0.010    0.3359847
    ##    1.00  0.100    0.3360802
    ##   10.00  0.001    0.3359831
    ##   10.00  0.010    0.3359817
    ##   10.00  0.100    0.3361129
    ## 
    ## Tuning parameter 'loss' was
    ##  held constant at a value of L1
    ## logLoss was used to select the
    ##  optimal model using the
    ##  smallest value.
    ## The final values used for the
    ##  model were cost = 10, loss = L1
    ##  and epsilon = 0.01.

``` r
  plot(reglog_fit)
```

![](SomeCollege_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

## Final Model Selection

``` r
# convert Diabetes_binary factors to numeric.
  test_set$Diabetes_binary <- as.numeric(test_set$Diabetes_binary)
# Obtain predicted probabilities
  predicted_prob1 <- predict(log_reg1, newdata = test_set, type = "prob")
  predicted_prob2 <- predict(lasso_fit, newdata = test_set, type = "prob")
  predicted_prob3 <- predict(class_fit, newdata = test_set, type = "prob")
  predicted_prob4 <- predict(rf_fit, newdata = test_set, type = "prob")
  predicted_prob5 <- predict(pls_fit, newdata = test_set, type = "prob")
  predicted_prob6 <- predict(reglog_fit, newdata = test_set, type = "prob")
  
# Calculate Log Loss manually for each model
  log_loss1 <- logLoss(test_set$Diabetes_binary, predicted_prob1$Diabetes)
  log_loss2 <- logLoss(test_set$Diabetes_binary, predicted_prob2$Diabetes)
  log_loss3 <- logLoss(test_set$Diabetes_binary, predicted_prob3$Diabetes)

# Add a small constant value to the predicted probabilities to avoid infinite output
  predicted_prob4$Diabetes <- pmax(predicted_prob4$Diabetes, 1e-15)
  predicted_prob4$Diabetes <- pmin(predicted_prob4$Diabetes, 1 - 1e-15)
  log_loss4 <- logLoss(test_set$Diabetes_binary, predicted_prob4$Diabetes)
  
  log_loss5 <- logLoss(test_set$Diabetes_binary, predicted_prob5$Diabetes)
  log_loss6 <- logLoss(test_set$Diabetes_binary, predicted_prob6$Diabetes)

# Create a data frame with log loss values
  log_loss_values <- data.frame(
    Model = c("log_loss1", "log_loss2", "log_loss3", "log_loss4", "log_loss5", "log_loss6"),
    LogLoss = c(log_loss1, log_loss2, log_loss3, log_loss4, log_loss5, log_loss6)
              )

# Print the data frame
  log_loss_values
```

    ##       Model   LogLoss
    ## 1 log_loss1  2.414168
    ## 2 log_loss2  2.619665
    ## 3 log_loss3  2.349794
    ## 4 log_loss4 24.433093
    ## 5 log_loss5  1.176513
    ## 6 log_loss6  2.626396

Based on the results, the model with the lowest log loss value is the
optimal model for selection.
