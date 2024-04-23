testthat::context(
  "Implementation of legal rules is consistent"
)



# CHECK THE LENGTH OF RETURN IS CONSISTENT ---------
N <- 100L

testthat::test_that(
  "Length of return is the same than input length",
  testthat::expect_equal(length(
    taxation_inheritance(rnorm(N), deduction = 3,
                                 bounds_legal = c(6, 9),
                                 taxes = c(0.01, 0.3,0.5))
  ),
  N
  )
)


# CHECK ERRORS ARE CONSISTENT --------------------

# Length of taxes should be > length bounds_legal
testthat::test_that(
  "Length of taxes should be > length bounds_legal",
  testthat::expect_error(
    taxation_inheritance(rnorm(N), deduction = 3,
                         bounds_legal = c(6, 9),
                         taxes = 0.2),
    regexp  = "must be equal to length"
  )
)



# INHERITANCE BELOW DEDUCTION  -------------

deduction <- 100

testthat::test_that(
  "Every values below deduction should not be affected by taxes",
  testthat::expect_equal(
    taxation_inheritance(x = seq_len(deduction-1),
                         deduction = deduction,
                         bounds_legal = c(6, 9),
                         taxes = c(0.01, 0.3,0.5)),
    seq_len(deduction-1)
  )
)



#

# Ex: pour que l'enfait ait 109,
# il faut donner plus

testthat::test_that(
  "Values consistent with implementation",
  testthat::expect_equal(
    taxation_inheritance(x = c(109, 119, 129),
                         deduction = deduction,
                         bounds_legal = 10,
                         taxes = c(0.1, 0.2)),
    c(109 + 9*0.1, 119 + 10*0.1 + 9*0.2,
      129 + 10*0.1 + 19*0.2),
    tolerance = 1e-3
  )
)


# For instance, example in the paper
deduction = 159325L
bounds = c(8072, 12109, 15932, 552324, 902838,
           1805677)
tax_rates =  c(5,10,15,20,30,40,45)/100

REtage::taxation_inheritance(
  x = 200000,
  deduction = deduction,
  bounds_legal = bounds,
  taxes = tax_rates
)

# x <- 206129.6-159325
# tx <- 8072*0.05 + (12109-8072)*0.1 + (15932-12109)*0.15 + (x-15932)*0.2
#
#
testthat::test_that(
  "Values consistent with implementation",
  testthat::expect_equal(
    REtage::taxation_inheritance(x = 200000,
                         deduction = 159325,
                         bounds_legal = c(8072,12109,15932,552324,902838,1805677),
                         taxes = c(5,10,15,20,30,40,45)/100),
    200000 + tx
  )
)

