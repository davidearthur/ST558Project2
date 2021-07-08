library(rmarkdown)
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
output_file <- paste0(days, ".md")
params <- lapply(days, FUN = function(x){list(dayOfWeek = x)})
documents <- tibble(output_file, params)
apply(documents, MARGIN = 1, FUN = function(x){
  render(input = "/Users/davidarthur/Repos/ST558Project2/ST558Project2.Rmd",
         output_file = x[[1]], params = x[[2]])
  })
