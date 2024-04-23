
#' Read inheritance data
#'
#' @param path_survey Path to survey data
#' @param year Year
#'
#' @import data.table
#' @importFrom lest case_when
#' @importFrom haven read_sas
#' @importFrom data.table .SD
#' @export

read_inheritance <- function(path_survey = "./inst/dataINSEE/Enquete Patrimoine",
                             year = 2009){

  #Avoid NSE notes
  macro<-NULL

  if (year==2009){
    directory <- paste0(path_survey,"/GEN_A1635100_DDIFASAS/")
  } else{
    directory <- paste0(path_survey,"/GEN_A1635140_DFPRPATRISAS/")
  }
  path_complete <- paste0(directory,"/TRANSMISSION.sas7bdat")


  df_transmission <- haven::read_sas(path_complete)
  df_transmission <- data.table::setDT(df_transmission)

  if (year == 2009){
    cols_to_keep <- c("IDENTMEN",
                      "IDENTPOS",
                      "IDENTTRANS",
                      "TRANSNA",
                      "POND",
                      "NOP",
                      "ANNEE",
                      "MTDONRC","MTDONR",
                      "MTDONVC", "MTDONV",
                      "MTHERC","MTHER",
                      "TRANSQUA_MOB",
                      "TRANSQUA_ARG","TRANSQUA_AVI",
                      "VALEUR")
  } else{
    cols_to_keep <- c("IDENT",
                      "IDENTPOS",
                      "IDENTTRANS",
                      "TRANSNA",
                      "POND",
                      "NOP",
                      "ANNEE",
                      "MTDONRC","MTDONR",
                      "MTDONVC", "MTDONV",
                      "MTHERC","MTHER",
                      "TRANSQUA_MOB",
                      "TRANSQUA_ARG","TRANSQUA_AVI",
                      "VALEUR")
  }

  df_transmission <- df_transmission[,.SD,
                                     .SDcols = cols_to_keep]

  load("./inst/dataINSEE/Destinie/macro.Rda")

  macro <- data.table::setDT(macro)[,.SD,.SDcols = c('annee','Prix')]

  data.table::setnames(df_transmission,
                       old = "ANNEE",
                       new = "annee")

  df_transmission[df_transmission == ""] <- NA

  changeCols<- names(Filter(is.character, df_transmission))

  df_transmission[,(changeCols):= lapply(.SD, as.factor), .SDcols = changeCols]

  df_transmission <- merge(df_transmission, macro,
                           by = "annee")


  df_transmission[, `:=` ('MTDONRC_real' = lest::case_when(
    get('VALEUR') == 1 ~ get('MTDONRC')/get('Prix'),
    TRUE ~ get('MTDONRC')
  ),
  'MTDONVC_real' = lest::case_when(
    get('VALEUR') == 1 ~ get('MTDONVC')/get('Prix'),
    TRUE ~ get('MTDONVC')
  )
  )]

  return(df_transmission)
}

#' Prepare inheritance data
#'
#' @inheritParams read_inheritance
#'
#' @export

data_inheritance <- function(path_survey = "./inst/dataINSEE/Enquete Patrimoine",
                             year = 2015){

  # ----------------------------------------------------------------
  #     LOAD DATA (TO EXTRACT HOUSEHOLD HEAD AGE)
  # ----------------------------------------------------------------

  if (year == 2015){
    wealth_survey_id <- "IDENT"
  } else{
    wealth_survey_id <- "IDENTMEN"
  }


  # LOAD INDIVIDUAL LEVEL DATA
  # --------------------------------
  # Annee 2009: GEN_A1635100_DDIFASAS
  # Annee 2015: GEN_A1635140_DFPRPATRISAS

  if (year==2009){
    directory <- paste0(path_survey,"/GEN_A1635100_DDIFASAS/")
  } else{
    directory <- paste0(path_survey,"/GEN_A1635140_DFPRPATRISAS/")
  }
  path_complete <- paste0(directory,"/INDIVIDU.sas7bdat")

  # READ SAS INDIVIDUAL TABLE
  data_patri <- haven::read_sas(path_complete)


  if (year==2009){
    cols_to_keep <- c(wealth_survey_id,
                      "IDENTIND",
                      "POND",
                      "NOI",
                      "SEXE",
                      "ANAIS",
                      "AGE",
                      "AGFINETU",
                      "ZSALAIRES_I",
                      "ZRETRAITES_I",
                      "ZCHOMAGE_I")
  } else{
    cols_to_keep <- c(wealth_survey_id,
                      "IDENTIND14",
                      "IDENTINDL",
                      "POND",
                      "NOI",
                      "SEXE",
                      "ANAIS",
                      "AGE",
                      "DATEFORA",
                      "ZSALAIRES_I",
                      "ZRETRAITES_I",
                      "ZCHOMAGE_I")
  }

  # GET IT AS DATA.TABLE
  data_patri <- data.table::setDT(data_patri)
  data_patri <- data_patri[,.SD,
                           .SDcols = cols_to_keep]


  # DEDUCE FINDET FOR 2015 DATA
  # --------------------------------

  if (year==2015){
    data_patri[,'AGFINETU' := get('DATEFORA') - get('ANAIS')]
  }


  # LOAD INHERITANCE TABLE
  # ---------------------------------


  df_transmission <- read_inheritance(
    path_survey = path_survey,
    year = year)


  EP_inheritance <- df_transmission[!is.na(get('MTHER'))]

  EP_inheritance[, 'source' := 'inheritance']

  # MERGE WITH INDIVIDUAL LEVEL INFORMATION
  # ------------------------------------------

  EP_completed <- merge(EP_inheritance,
                        data_patri,
                        by = intersect(names(EP_inheritance), names(data_patri)),
                        all = TRUE)


  EP_completed2 <- EP_completed[!is.na(get('MTHER')) & !is.na(get('source'))]

  EP_completed2[,'MTHER' := factor(get('MTHER'),
                                   ordered = TRUE)]


  return(EP_completed2)
}


