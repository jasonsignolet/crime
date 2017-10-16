## Adelaide crime stats exploration

## Packages ----
library(readxl)
library(data.table)
library(magrittr)
library(foreach)
library(ggplot2)
library(doParallel)

## UDFs ----

#' Column renamer
#'
#' @description Strip out the punctuation and white space from imported colnames so they can be autocompleted
#' @param x Character vector of column names or data.frame/data.table object
#' @return The same names as input, but lowercase with underscores instead of spaces (multiple spaces are reduced to single)
name_cleaner <- function(x) {
  if (!is.vector(x)) {
    if (is.data.frame(x)) {
      x <- colnames(x)
      message("Detected data.frame input")
    } else {
      stop("Expecting a character vector or data.frame")
    }
  }

  recurse_spaces <- function(y) {
    # Base case
    if (length(grep("__", y)) == 0) { return(y) }

    y <- gsub("__", "_", y)

    return(recurse_spaces(y))
  }

  x %>%
    tolower %>%
    gsub("[\n\r\t ]", "_", .) %>% # whitespace -> underscore
    gsub("[^[:alnum:]^_^]", "", .) %>% # strip out other punctuation
    recurse_spaces %>%
    gsub("_$", "", .) # strip out trailing underscores
}



## Define file names ----
DIR <- "data"
year_from <- 2012:2016
year_to <- 13:17
files <- sprintf("crime-statistics-%s-%s.xlsx", year_from, year_to)

## Load data from xlsx ----
cl <- makeCluster(5)
registerDoParallel(cl)

crime <- foreach(i = seq_along(files)) %do% {
  f <- file.path(DIR, files[i])
  y <- year_from[i]

  read_excel(f) %>%
    setDT %>%
    .[, year := y]
} %>% rbindlist

stopCluster(cl)

setnames(crime, name_cleaner(crime))



## What's the distribution of offence level 1? ----
ggplot(crime, aes(offence_level_1_description)) +
  geom_bar(stat = "count") +
  coord_flip() +
  facet_wrap(~ year)

crime[union(grep("HOMICIDE", offence_level_1_description), (grep("HOMICIDE", offence_level_2_description))), .N]
