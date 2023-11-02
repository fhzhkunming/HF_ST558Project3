# ST558_Project3

This is a repo containing work from project3 for ST558.

The following R packages used for this project:
+ [`tidyverse`](https://www.tidyverse.org/) An opinionated collection of R packages designed for data science  
+ [`caret`](https://cran.r-project.org/web/packages/caret/) Multiple functions for training and plotting classification and regression models.
+ [`knitr`](https://cran.r-project.org/web/packages/knitr/index.html) Provides a general-purpose tool for dynamic report generation in R using Literate Programming techniques.

+ the code used to create the analyses from a single .Rmd file (i.e. the render() code)
rmarkdown::render("work.Rmd", output_file = "Cleveland Browns.html",
                  params = list(team = "Cleveland Browns"))

links to .html files of the generated analyses (which will be created by github pages! Not you!)  
For example,
+ Analysis for [work.md](work.html). 
Note you should only have a college_graduate_analysis.md file in the repo - github pages will render the .html file for you