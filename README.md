# ST558Project2
## Project Summary
This github repo stores a collaberation project that David Arthur and James Carr worked on for the class **Data Science for Statisticians** as part of NC State's Masters in Statistics program. We were asked to download data [found here](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset) that relates to a bike sharing service. The data tracks the number of riders per day, along with a few categorical and quantitative predictor varabies. For all seven days of the week, we ran summary analyses, fit two linear regression models, a random forest model, and a boosting model. 

## Packages Required
The following packages are required to run this program: 

*  tidyverse
*  caret 
*  corrplot
*  GGally
*  knitr
*  faraway
*  leaps
*  gridExtra

## Links to Each Day of the Week
For each day of the week, we looked at summary statistics and created multiple models to predict the number of registered riders. You can find each day's analyses at the following links: 

*  [Sunday](https://github.com/davidearthur/ST558Project2/blob/main/Sunday.md)
*  [Monday](https://github.com/davidearthur/ST558Project2/blob/main/Monday.md)
*  [Tuesday](https://github.com/davidearthur/ST558Project2/blob/main/Tuesday.md)
*  [Wednesday](https://github.com/davidearthur/ST558Project2/blob/main/Wednesday.md)
*  [Thursday](https://github.com/davidearthur/ST558Project2/blob/main/Thursday.md)
*  [Friday](https://github.com/davidearthur/ST558Project2/blob/main/Friday.md)
*  [Saturday](https://github.com/davidearthur/ST558Project2/blob/main/Saturday.md)

## Code used to create analyses
    library(rmarkdown)
    days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    output_file <- paste0(days, ".md")
    params <- lapply(days, FUN = function(x){list(dayOfWeek = x)})
    documents <- tibble(output_file, params)
    apply(documents, MARGIN = 1, FUN = function(x){
      render(input = "/Users/davidarthur/Repos/ST558Project2/ST558Project2.Rmd",
             output_file = x[[1]], params = x[[2]])
      })
