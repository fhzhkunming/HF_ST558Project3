# ST558_Project3

This is a repo containing work from project3 for ST558. 

This project uses a diabetes data set from [Kaggle](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/) for exploratory data analysis (EDA) and predictive modeling of diabetes outcomes. The primary target variable, Diabetes_binary, distinguishes 0 for no diabetes and 1 for prediabetes or diabetes. Key predictors include High blood pressure, High cholesterol, BMI, Smoker, Fruits, Veggies, and Age. EDA involves basic statistics, visualizations, correlations, and contingency tables. After EDA, the data set is split into 70% training and 30% test subsets for each educational level. Six models are trained and evaluated to identify the most effective model for predicting diabetes outcomes.

The following R packages were used for this project:  

+ [**`tidyverse`**](https://www.tidyverse.org/) An opinionated collection of R packages designed for data science.  
+ [**`caret`**](https://cran.r-project.org/web/packages/caret/) A set of functions that attempt to streamline the process for creating predictive models.  
+ [**`shiny`**](https://cran.r-project.org/web/packages/shiny/index.html) Provides an elegant and powerful web framework for building web applications using R.  
+ [**`DT`**](https://rstudio.github.io/DT/) Provides an R interface to the JavaScript library Data Tables.  
+ [**`metrics`**](https://cran.r-project.org/web/packages/Metrics/index.html)  Provides metrics for regression, time series, binary classification, classification, and information retrieval problems.  
+ [**`glmnet`**](https://cran.r-project.org/web/packages/glmnet/index.html) A package that fits generalized linear and similar models via penalized maximum likelihood.
+ [**`LiblineaR`**](https://cran.r-project.org/web/packages/LiblineaR/index.html)  Provides a simple library for solving large-scale regularized linear classification and regression problems.  
+ [**`pls`**](https://cran.r-project.org/web/packages/pls/index.html) Provides functions for performing Partial Least Squares analysis.

The code used to create the analyses from a single .Rmd file (i.e. the render() code)
```
library(rmarkdown)

EducationLv <- unique(diabetes$Education)
output_file <- paste0(EducationLv, ".md")
params = lapply(EducationLv, FUN = function(x){list(Edu = x)})
reports <- tibble::tibble(output_file, params)
reports

apply(reports, MARGIN = 1,
      FUN = function(x){render(input = "work.Rmd", 
				output_file = x[[1]], 
				params = x[[2]])
 				})
```
 				
You can access the rendered documents:  

+ Analysis for [SomeElementary](SomeElementary.html)  
+ Analysis for [SomeHighSchool](SomeHighSchool.html)  
+ Analysis for [HighSchool](HighSchool.html)  
+ Analysis for [SomeCollege](SomeCollege.html)  
+ Analysis for [College](College.html)  
 
