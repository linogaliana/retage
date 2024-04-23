
#' Prepare data for estimation
#'
#' @param data Wealth survey data from \link{data_inheritance}
#' @param .cols Columns to keep
#'
#' @importFrom stats na.omit
#' @importFrom lest case_when
#'
#' @export

prepare_estimation <- function(data_estimation,
                               .cols = c("MTHER", "AGFINETU", "AGE",
                                         "ZSALAIRES_I",
                                         "ZRETRAITES_I",
                                         "ZCHOMAGE_I",
                                         "SEXE"),
                               taille_tr_age = 5,
                               taille_tr_agfinetu = 2
){

  # data_estimation <- data[,.SD,
  #                         .SDcols = .cols]

  # data_estimation <- na.omit(data_estimation)


  data_estimation[,'w' := get('ZSALAIRES_I') +  get('ZRETRAITES_I') +  get('ZCHOMAGE_I')]
  data_estimation[,`:=` ('lw' = log(get('w')))]

  data_estimation[,'AGFINETU' := lest::case_when(
    get('AGFINETU') > 29 ~ 30,
    get('AGFINETU') < 13 ~ 14,
    TRUE ~ get('AGFINETU')
  )]

  # data_estimation[,'AGE' := lest::case_when(
  #   get('AGE') > 100 ~ 100,
  #   get('AGE') < 15 ~ 15,
  #   TRUE ~ get('AGE')
  # )]


  data_estimation[, `:=` ('tr_age' = factor(taille_tr_age*floor(get('AGE')/taille_tr_age)),
                          'tr_agfinetu' = factor(taille_tr_agfinetu*floor(get('AGFINETU')/taille_tr_agfinetu)))]
  data_estimation[,'SEXE' := as.factor(get('SEXE'))]

  return(data_estimation)

}
