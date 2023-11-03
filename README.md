# ST558_Project3

This is a repo containing work from project3 for ST558.

The following R packages used for this project:
+ [`tidyverse`](https://www.tidyverse.org/)  
+ [`caret`](https://cran.r-project.org/web/packages/caret/)
+ [`shiny`](https://cran.r-project.org/web/packages/shiny/index.html)
+ [`DT`](https://rstudio.github.io/DT/)

The code used to create the analyses from a single .Rmd file (i.e. the render() code)
```{r}
library(rmarkdown)
# get unique Education level
EducationLv <- unique(diabetes$Education)
# create filenames
output_file <- paste0(EducationLv, ".html")
# create a list for each team with just the team name parameter
params = lapply(EducationLv, FUN = function(x){list(Edu = x)})

# put into a data frame
reports <- tibble::tibble(output_file, params)
reports
                 
# need to use x[[1]] to get at elements since tibble doesn't simplify
apply(reports, MARGIN = 1,
      FUN = function(x){
				render(input = "work.Rmd", 
				output_file = x[[1]], 
				params = x[[2]])
 				})
                  
```                  

links to .html files of the generated analyses (which will be created by github pages! Not you!)  
For example,
+ Analysis for [work.md](work.html). 
Note you should only have a college_graduate_analysis.md file in the repo - github pages will render the .html file for you
