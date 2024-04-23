## code to prepare `macro` dataset goes here


load("./inst/dataINSEE/Destinie/macro.Rda")

macro <- data.table::setDT(macro)[,.SD,.SDcols = c('annee','Prix')]


usethis::use_data(macro, overwrite = TRUE)

