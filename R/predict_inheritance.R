#' Predict inheritance from an econometric model
#'
#' @param inheritance_model A `oglmx` model
#' @param data Dataframe
#' @param scale Scale in the latent variable space
#' @param bias_correction Logical value indicating, if the
#'  model is in `log` whether predictions should be corrected
#'  by a factor \eqn{exp(-\sigma^2/2)}
#' @param variable_name Variable name for the prediction
#'
#' @return Initial dataframe with a new column corresponding to
#'  `variable_name` supplied value
#'
#' @export


predict_inheritance <- function(inheritance_model, data, selection_model = NULL,
                                scale = "log",
                                bias_correction = FALSE,
                                variable_name = "y_pred",
                                lbounds = NULL){

  newdata <- data.table::copy(data)

  if (!is.null(selection_model)){

    prediction_2step <- predict_two_steps(probit = selection_model,
                      intreg = inheritance_model, newdata, scale = "latent",
                      y_scale = "log", lbounds = lbounds)
    newdata[,c(variable_name, "proba_selection_step1", "y_latent_step2") := prediction_2step]
    return(newdata)

  } else{

    if (inherits(inheritance_model, "oglm")){
      newdata[,'Hg' := predict(inheritance_model,
                               newdata = newdata,
                               type = "latent")$y_latent_pred]
    } else{
      newdata <- predict_selection(inheritance_model, data = newdata)
    }

  }


  if ((scale == "log") && (is.null(selection_model)))
    newdata[, c(variable_name) := exp(get('Hg'))]

  if ((bias_correction) && (scale == "log")){
    sigma_vectorized <- sigma(inheritance_model, newdata = data)
    newdata[,c(variable_name) := get(variable_name)*exp(-sigma_vectorized/2)]
  }

  return(newdata)
}



predict_selection <- function(inheritance_model, data){

  probs0 <- data.frame(
    p0 = as.numeric(predict(inheritance_model, newdata = data,
                            type = "response", part = "selection")),
    Hg = as.numeric(predict(inheritance_model, newdata = data,
                            type = "conditional", part = 'outcome'))
  )
  return(
    cbind(data, probs0)
  )
  # equivalent to : as.numeric(predict(oglm_model, type = "P[y == 0|Z]", newdata = dat))
  # equivalent to : predict(oglm_model, type = "E[y|X,y>0]", newdata = dat)

}



get_labs <- function(lbounds){
  labs <- c(sprintf("Less than %s", exp(lbounds[1])),
            as.character(
              lapply(1:(length(lbounds)-1), function(i){
                sprintf("%s - %s",
                        format(round(exp(lbounds[i])), scientific = FALSE, big.mark = ","),
                        format(round(exp(lbounds[i+1])), scientific = FALSE, big.mark = ","))
              })),
            sprintf("More than %s", format(round(exp(max(lbounds))), scientific = FALSE, big.mark = ","))
  )
  return(labs)
}

# new levels bounded
clamp <- function(x, a, b) {
  pmax(a, pmin(x, b) )
}

cut_out_level <- function(xvar, model, data){
  if (sum(grepl(xvar, names(model$xlevels)))>0){
    levels_in_model <- as.numeric(model$xlevels[grepl(xvar, names(model$xlevels))][[1]])
    data[, c(xvar) :=  clamp(as.numeric(as.character(get(xvar))),
                             min(levels_in_model),
                             max(levels_in_model))]
  }
}

predict_two_steps <- function(probit, intreg, data, lbounds = NULL, scale = c("class","latent"),
                              y_scale = "log"){

  scale <- match.arg(scale)

  cut_out_level("tr_age", probit, data)
  cut_out_level("tr_agfinetu", probit, data)

  proba_selection <- predict(probit, newdata = data, type = "link")
  proba_selection <- proba_selection + rnorm(length(proba_selection), sd = sd(probit$residuals))
  pred_selection <- as.numeric(proba_selection > 0)
  pred_oglm <- predict(intreg, newdata = data,
                       type = "latent")

  if (scale == 'class'){
    if (is.null(lbounds)) stop("When predicting classes, lbounds arg shoud be provided")
    labs <- get_labs(lbounds)
    pred_level <- cut(pred_oglm$y_latent_pred, breaks = c(0, lbounds, Inf), labels = labs)
  } else{
    pred_level <- pred_oglm$y_latent_pred
    if (y_scale == "log"){
      pred_level <- exp(pred_level)
    }
  }

  prediction <- pred_selection
  prediction[prediction == 1] <- as.numeric(pred_level)[prediction == 1]

  return(
    data.table::data.table(
      "prediction" = prediction,
      "y_latent_step1" = proba_selection,
      "y_latent_step2" = pred_oglm$y_latent_pred
      )
  )
}
