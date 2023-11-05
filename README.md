# ST558_Project3

This is a repo containing work from project3 for ST558.

The following R packages used for this project:
+ [`tidyverse`](https://www.tidyverse.org/)  
+ [`caret`](https://cran.r-project.org/web/packages/caret/)
+ [`shiny`](https://cran.r-project.org/web/packages/shiny/index.html)
+ [`DT`](https://rstudio.github.io/DT/)
+ [`metrics`](https://cran.r-project.org/web/packages/Metrics/index.html)
+ [`glmnet`](https://cran.r-project.org/web/packages/glmnet/index.html)

The code used to create the analyses from a single .Rmd file (i.e. the render() code)
```
library(rmarkdown)

EducationLv <- unique(diabetes$Education)
output_file <- paste0(EducationLv, ".html")
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
+ Analysis for [work.md](work.html)
+ Analysis for [SomeElementaty](SomeElementary.html)    
+ Analysis for [SomeHighSchool](SomeHighSchool.html) 
+ Analysis for [HighSchool](HighSchool.html)  
+ Analysis for [SomeCollege](SomeCollege.html)  
+ Analysis for [College](College.html)  
