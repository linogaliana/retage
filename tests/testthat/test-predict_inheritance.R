set.seed(242)

n <- 250

x1 <- sample(c(0, 1), n,
             replace = TRUE,
             prob = c(0.75, 0.25))

x2 <- vector("numeric", n)
x2[x1 == 0] <- sample(c(0, 1),
                      n - sum(x1 == 1),
                      replace = TRUE,
                      prob = c(2/3, 1/3))

z <- rnorm(n, 0.5)
# create latent outcome variable
latenty <- -0.5 + 1.5 * x1 - 0.5 * x2 + 0.5 * z + rnorm(n, sd = exp(0.5 *
                                                                      x1 - 0.5 * x2))
# observed y has four possible values: -1,0,1,2
# threshold values are: -0.5, 0.5, 1.5.
y <- vector("numeric", n)
y[latenty < 0.5] <- -1
y[latenty >= 0.5 & latenty < 1.5] <- 0
y[latenty >= 1.5 & latenty < 2.5] <- 1
y[latenty >= 2.5] <- 2
dataset <- data.frame(y, x1, x2)

bounds <- c(0.5, 1.5, 2.5)
lbounds <- log(bounds)

ordered_probit <- REtage::ordered_model_threshold(
  dataset,
  formula = "y ~ x1+x2",
  link = "probit",
  thresholds = lbounds,
  constantSD = TRUE)

data.table::setDT(dataset)

pred_data <-  predict_inheritance(inheritance_model = ordered_probit,
                                  data =  dataset,
                                  scale = "log",
                                  bias_correction = FALSE,
                                  variable_name = "y_pred")


testthat::test_that("New dataframe with additional columns", {

  testthat::expect_equal(
    c(colnames(dataset), c("Hg","y_pred")),
    colnames(
      predict_inheritance(inheritance_model = ordered_probit,
                          data =  dataset,
                          scale = "log",
                          bias_correction = FALSE,
                          variable_name = "y_pred")
    )
  )

  testthat::expect_equal(
    c(colnames(dataset), c("Hg")),
    colnames(
      predict_inheritance(inheritance_model = ordered_probit,
                          data =  dataset,
                          scale = "level",
                          bias_correction = FALSE,
                          variable_name = "y_pred")
    )
  )

})

# set.seed(123)

# testthat::test_that("predict_inheritance same than predict without bias correction", {
#   testthat::expect_equal(
#     predict_inheritance(inheritance_model = ordered_probit,
#                         data =  dataset,
#                         scale = "level",
#                         bias_correction = FALSE,
#                         variable_name = "latent")[['Hg']],
#     as.numeric(predict(ordered_probit, dataset, type = "latent")$y_latent_pred)
#   )
#
# })




