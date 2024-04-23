
#' Function to estimate polytomic discrete order models
#'
#' @inheritParams oglm::oglmx
#' @param data Dataframe used for estimation
#' @param formula An object of class \link[stats]{formula}:
#'  a symbolic description of the model used to explain the
#'  mean of the latent variable. The response variable should
#'  be a numeric vector or factor variable such that the
#'  numerical assignments for the levels of the
#'  factor have ordinal meaning.
#' @param thresholds \code{NULL} (default) or numeric vector.
#'  Thresholds that define discretization
#'  of the response variable. Must either be a numeric vector,
#'  assuming a known threshold ordered discrete model or
#'  \code{NULL} value in which case a unordered discrete
#'  choice model is applied. Vector should be of length
#'  equal to the number of outcomes
#'  minus one. \code{NA} should be entered for threshold
#'  parameters to be estimated by the model.
#' @param ... Additional parameters that should be supplied
#'  to \link[oglmx]{oglmx} function
#'
#' @details Behind the stage, \link[oglmx]{oglmx} is
#'  used to estimate models
#'  for which the outcome variable is discrete and the mean
#'  and/or variance of the underlying latent variable can
#'  be modelled as a linear combination of explanatory
#'  variables. \link[oglmx]{oglmx} is preferred to
#'  \link[MASS]{polr} because the latter does not
#'  allpw constrained models where thresholds in latent
#'  space for response variable are known
#'
#' @seealso \link[oglmx]{oglmx}, \link[MASS]{polr}
#'
#' @examples
#' iris$y <- sample(1:5, size = nrow(iris),
#'                 replace = TRUE)
#' summary(
#'  ordered_model_threshold(
#'   iris,
#'   formula = "y ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width",
#'   link = "probit",
#'   thresholds = NULL)
#' )
#'
#' @importFrom stats as.formula
#' @export


ordered_model_threshold <- function(data,
                                    formula = "MTHER ~ tr_agfinetu + tr_age + revenu + SEXE",
                                    formulaSD = NULL,
                                    link = c("logit","probit"),
                                    delta = NULL,
                                    constantMEAN = TRUE,
                                    constantSD = TRUE,
                                    thresholds = NULL,
                                    ...){


  if (missing(link)) link <- "logit"

  ordered_model <- oglm::oglmx(
    formulaMEAN = as.formula(formula),
    formulaSD = formulaSD,
    data = data,
    link = link,
    constantMEAN = constantMEAN,
    constantSD = constantSD,
    delta = delta,
    threshparam = thresholds,
    ...)


  return(ordered_model)
}




# ordered_model_threshold2 <- function(data,
#                                      formula = "MTHER ~ tr_agfinetu + tr_age + revenu + SEXE",
#                                      formulaSD = NULL,
#                                      link = c("logit","probit"),
#                                      delta = NULL,
#                                      constantMEAN = TRUE,
#                                      constantSD = TRUE){
#
#   data = data.table::copy(
#     data
#   )
#
#   lb <- as.numeric(
#     do.call(rbind,
#             lapply(seq_len(nrow(data)), function(i) log(c(NA, thresholds)[as.numeric(as.character(data$MTHER))[i]])),
#     )
#   )
#   ub <- as.numeric(
#     do.call(rbind,
#             lapply(seq_len(nrow(data)), function(i) log(c(thresholds,NA)[as.numeric(as.character(data$MTHER))[i]]))
#     )
#   )
#
#   (Y_surv <- with(data, survival::Surv(time = lb, time2 = ub,
#                                        event = rep(3, nrow(data)),
#                                        type = "interval"))
#     )
#
#   m <- survival::survreg(
#     Y_surv ~ tr_agfinetu + tr_age + revenu + SEXE, data = data, dist = "gaussian"
#   )
#
#   return(m)
# }


# ordered_model_threshold2 <- function(data,
#                                      formula = "MTHER ~ tr_agfinetu + tr_age + revenu + SEXE",
#                                      formulaSD = NULL,
#                                      link = c("logit","probit"),
#                                      delta = NULL,
#                                      constantMEAN = TRUE,
#                                      constantSD = TRUE){
#
#   data = data.table::copy(
#     data
#   )
#
#   lb <- as.numeric(
#     do.call(rbind,
#             lapply(seq_len(nrow(data)), function(i) log(c(0, thresholds)[as.numeric(as.character(data$MTHER))[i]])),
#     )
#   )
#   ub <- as.numeric(
#     do.call(rbind,
#             lapply(seq_len(nrow(data)), function(i) log(c(thresholds,Inf)[as.numeric(as.character(data$MTHER))[i]]))
#     )
#   )
#
#   m <- icenReg::ic_par(
#     cbind(lb,ub) ~ tr_agfinetu + tr_age + revenu + SEXE, data = data, model = "po",
#     dist = "lnorm"
#   )
#   # m2 <- icenReg::ic_par(
#   #   cbind(lb,ub) ~ tr_agfinetu + tr_age + revenu + SEXE, data = data, model = "po",
#   #   dist = "loglogistic"
#   # )
#
#   summary(m2)
#
#   return(m)
# }
