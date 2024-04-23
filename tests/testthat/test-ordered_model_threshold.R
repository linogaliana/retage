context("ordered_model_threshold implements ordered discrete choice models")



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




# PART I: COEFFICIENTS UNKNOWN -----


# A/ LOGIT =======================

ordered_logit <- REtage::ordered_model_threshold(
  dataset,
  formula = "y ~ x1+x2",
  link = "logit",
  delta = 0,
  constantMEAN = FALSE,
  constantSD = FALSE,
  thresholds = NULL
)

ordered_logit_MASS <- MASS::polr(
  "I(factor(y)) ~ x1+x2",
  data = dataset,
  Hess = TRUE,
  method = "logistic"
)



# B/ PROBIT =======================

ordered_probit <- REtage::ordered_model_threshold(
  dataset,
  formula = "y ~ x1+x2",
  link = "probit",
  delta = 0,
  constantMEAN = FALSE,
  constantSD = FALSE,
  thresholds = NULL
)

ordered_probit_MASS <- MASS::polr(
  "I(factor(y)) ~ x1+x2",
  data = dataset,
  Hess = TRUE,
  method = "probit"
)



test_that("Unknown coefficient model produces same output than MASS::polr function",
          {
            expect_equal(as.numeric(coef(ordered_logit)),
                         as.numeric(c(
                           coef(ordered_logit_MASS), ordered_logit_MASS$zeta
                         )),
                         tolerance = 1e-03)
            expect_equal(as.numeric(coef(ordered_probit)),
                         as.numeric(c(
                           coef(ordered_probit_MASS), ordered_probit_MASS$zeta
                         )),
                         tolerance = 1e-03)
          })

test_that("link reported is consistent with arguments", {
  expect_equal(ordered_logit$link, "logit")
  expect_equal(ordered_probit$link, "probit")
})

# INTERVAL REGRESSION: KNOWN COEFFICIENTS ---------------

# ASSUME THERE EXISTS SOME UNDERLYING BOUNDS

bounds <- c(0.5, 1.5, 2.5)
lbounds <- log(bounds)

testthat::test_that("[no heteroskedasticity] Model with known threshold does not produce any error", {

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "logit",
      thresholds = bounds,
      constantSD = TRUE
    )
  ), NA)

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "probit",
      thresholds = bounds,
      constantSD = TRUE
    )
  ), NA)

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "probit",
      thresholds = lbounds,
      constantSD = TRUE
    )
  ), NA)

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "logit",
      thresholds = lbounds,
      constantSD = TRUE
    )
  ), NA)


})


testthat::test_that("[heteroskedasticity] Model with known threshold does not produce any error", {

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "logit",
      thresholds = bounds,
      SameModelMEANSD = TRUE
    )
  ), NA)

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "probit",
      thresholds = bounds,
      SameModelMEANSD = TRUE
    )
  ), NA)

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "probit",
      thresholds = lbounds,
      SameModelMEANSD = TRUE
    )
  ), NA)

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "logit",
      thresholds = lbounds,
      SameModelMEANSD = TRUE
    )
  ), NA)


  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "logit",
      thresholds = bounds,
      formulaSD = as.formula("y ~ x2")
    )
  ), NA)

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "probit",
      thresholds = bounds,
      SameModelMEANSD = TRUE,
      formulaSD = as.formula("y ~ x2")
    )
  ), NA)

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "probit",
      thresholds = lbounds,
      formulaSD = as.formula("y ~ x2")
    )
  ), NA)

  testthat::expect_error(summary(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "logit",
      thresholds = lbounds,
      formulaSD = as.formula("y ~ x2")
    )
  ), NA)


})


testthat::test_that("[heteroskedasticity] error when formula is a string", {

  testthat::expect_error(
    REtage::ordered_model_threshold(
      dataset,
      formula = "y ~ x1 + x2",
      link = "logit",
      thresholds = lbounds,
      formulaSD = "y ~ x2"
    ), "operator is invalid for atomic vectors")


})


# INTERVAL REGRESSION: KNOWN COEFFICIENTS ---------------

# data("Smoke", package = "sampleSelection")
#
#
# bounds <- c(5, 10, 20, 50)
# lbounds <- log(bounds)
#
# Smoke2 <- na.omit(Smoke)
# # Smoke2$cigs_intervals <- factor(Smoke2$cigs_intervals, levels = 1:5, ordered = TRUE)
#
# summary(
#   REtage::ordered_model_threshold(
#     Smoke2,
#     formula = "cigs_intervals ~ educ",
#     link = "logit",
#     thresholds = bounds,
#     constantSD = TRUE)
# )
#
# summary(
#   REtage::ordered_model_threshold(
#     Smoke2,
#     formula = "cigs_intervals ~ educ",
#     link = "probit",
#     thresholds = lbounds,
#     constantSD = TRUE)
# )
#
# summary(
#   REtage::ordered_model_threshold(
#     Smoke2,
#     formula = "cigs_intervals ~ educ + I(log(income))",
#     link = "probit",
#     thresholds = lbounds,
#     constantSD = TRUE)
# )
#
#
# summary(
#   REtage::ordered_model_threshold(
#     Smoke2,
#     formula = "cigs_intervals ~ educ",
#     link = "probit",
#     thresholds = lbounds,
#     constantSD = TRUE)
# )
#
#
# summary(
#   REtage::ordered_model_threshold(
#     Smoke2,
#     formula = "cigs_intervals ~ educ",
#     link = "probit",
#     thresholds = lbounds,
#     SameModelMEANSD = TRUE
# ))
#
# summary(
#   REtage::ordered_model_threshold(
#     Smoke2,
#     formula = "cigs_intervals ~ educ + age",
#     link = "probit",
#     thresholds = lbounds,
#     formulaSD = as.formula("cigs_intervals ~ age"))
# )
#
#
# # data.table::fwrite(
# #   Smoke2, "D:/W3CRK9/Mes Documents/PROJECTS/Microsimulation_archives/reports/2019-04-26-Heritage/heritage/smoke.csv"
# # )
