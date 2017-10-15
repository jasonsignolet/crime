## Adelaide crime stats exploration

library(readxl)
library(data.table)
library(magrittr)
library(foreach)
library(ggplot2)

DIR <- "data"
#files <- list.files(DIR)

year_from <- 2012:2016
year_to <- 13:17
files <- sprintf("crime-statistics-%s-%s.xlsx", year_from, year_to)

crime <- foreach(i = seq_along(files)) %do% {
  f <- file.path(DIR, files[i])
  y <- year_from[i]

  read_excel(f) %>%
    setDT %>%
    .[, year := y]
} %>% rbindlist


ggplot(crime, aes(`Offence Level 1  Description`)) +
  geom_bar(stat = "count") +
  coord_flip() +
  facet_wrap(~ year)
