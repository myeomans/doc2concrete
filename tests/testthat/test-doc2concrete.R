
data("feedback_dat")
feedback_dat <- feedback_dat[1:10,]

context("doc2concrete function")

d2c_open <- doc2concrete(texts = feedback_dat$feedback, domain = "open")
d2c_advice <- doc2concrete(texts = feedback_dat$feedback, domain = "advice")
d2c_plans <- doc2concrete(texts = feedback_dat$feedback, domain = "plans")

test_that("longth of vectors in & out", {
  expect_equal(length(feedback_dat$feedback), length(d2c_open))
  expect_equal(length(feedback_dat$feedback), length(d2c_advice))
  expect_equal(length(feedback_dat$feedback), length(d2c_plans))
})

test_that("empty or na string", {

  df_polite <- doc2concrete(texts = c("","a"), domain = "open")
  expect_equal(length(df_polite), 2)

  df_polite <- doc2concrete(texts = c("","a"), domain = "advice")
  expect_equal(length(df_polite), 2)

  df_polite <- doc2concrete(texts = c("","a"), domain = "plans")
  expect_equal(length(df_polite), 2)

  df_polite <- doc2concrete(texts = c(NA_character_,"a"), domain = "open")
  expect_equal(length(df_polite), 2)

  df_polite <- doc2concrete(texts = c(NA_character_,"a"), domain = "advice")
  expect_equal(length(df_polite), 2)

  df_polite <- doc2concrete(texts = c(NA_character_,"a"), domain = "plans")
  expect_equal(length(df_polite), 2)

  feedback_dat$feedback[1] <- NA_character_

  df_polite <- doc2concrete(texts = feedback_dat$feedback, domain = "open")
  expect_equal(length(df_polite), length(feedback_dat$feedback))

  df_polite <- doc2concrete(texts = feedback_dat$feedback, domain = "advice")
  expect_equal(length(df_polite), length(feedback_dat$feedback))

  df_polite <- doc2concrete(texts = feedback_dat$feedback, domain = "plans")
  expect_equal(length(df_polite), length(feedback_dat$feedback))
})

test_that("factor handling", {
  expect_equal(doc2concrete(texts = as.factor(feedback_dat$feedback), domain = "open"),
               doc2concrete(texts = (feedback_dat$feedback), domain = "open"))
  expect_equal(doc2concrete(texts = as.factor(feedback_dat$feedback), domain = "advice"),
               doc2concrete(texts = (feedback_dat$feedback), domain = "advice"))
  expect_equal(doc2concrete(texts = as.factor(feedback_dat$feedback), domain = "plans"),
               doc2concrete(texts = (feedback_dat$feedback), domain = "plans"))
})

test_that("text of length 0", {
  empty_open <- doc2concrete(texts = "", domain = "open")
  empty_adv  <- doc2concrete(texts = "", domain = "advice")
  empty_pla <- doc2concrete(texts = "", domain = "plans")
  expect_true(is.numeric(empty_open))
  expect_true(is.numeric(empty_adv))
  expect_true(is.numeric(empty_pla))
})

test_that("text of length 1", {
  empty_open <- doc2concrete(texts = "hello", domain = "open")
  empty_adv  <- doc2concrete(texts = "hello", domain = "advice")
  empty_pla <- doc2concrete(texts = "hello", domain = "plans")
  expect_true(is.numeric(empty_open))
  expect_true(is.numeric(empty_adv))
  expect_true(is.numeric(empty_pla))
})

