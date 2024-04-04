library(testthat)
library(dplyr)
test_that("Number of hospitals is correct", {
  expected_hospitals <- 5
  observed_hospitals <- length(unique(data$Hospital))
  expect_equal(observed_hospitals, expected_hospitals)
})
test_that("Years range from 2003 to 2022", {
  expect_equal(range(data$Year), c(2003, 2022))
})
test_that("Number of cancer types is correct", {
  expected_cancer_types <- 5
  observed_cancer_types <- length(unique(data$CancerType))
  expect_equal(observed_cancer_types, expected_cancer_types)
})
test_that("All expected columns are present", {
  expected_columns <- c("Year", "Hospital", "CancerType", "NumberOfDeaths")
  expect_equal(names(data), expected_columns)
})
test_that("Number of deaths is within expected range", {
  expect_true(all(data$NumberOfDeaths >= 5 & data$NumberOfDeaths <= 200))
})
test_that("There are no missing values", {
  expect_true(!any(is.na(data)))
})
test_that("Total number of rows is correct", {
  expected_rows <- 20 * 5 * 5 # Years * Hospitals * Cancer Types
  expect_equal(nrow(data), expected_rows)
})
test_that("Each hospital has data for every year and cancer type", {
  data_grouped <- data %>%
    group_by(Hospital) %>%
    summarise(Entries = n()) %>%
    ungroup()
  
  expected_entries_per_hospital <- 20 * 5 # Years * Cancer Types
  expect_true(all(data_grouped$Entries == expected_entries_per_hospital))
})
test_that("All rows are unique", {
  expect_equal(nrow(data), nrow(unique(data)))
})
test_that("Year column contains only integers", {
  expect_true(all(apply(data.frame(data$Year), 2, function(x) all(x == floor(x)))))
})
