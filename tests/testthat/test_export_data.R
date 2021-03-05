library(odkr)
context("Check exported data")

test_that("Error for no target", {
  expect_error(export_data(id = "stakeholders",
                           to = dirPath,
                           from = dirPath,
                           filename = "test.csv",
                           overwrite = TRUE),
               "Cannot locate ODK Briefcase .jar file. Check target location of .jar file is correct.")
})

test_that("Error for no id", {
  expect_error(export_data(target = dirPath,
                           to = dirPath,
                           from = dirPath,
                           filename = "test.csv",
                           overwrite = TRUE),
               "Form id not specified. Try again.")
})

test_that("Error for no from", {
  expect_error(export_data(target = dirPath,
                           id = "stakeholders",
                           to = dirPath,
                           filename = "test.csv",
                           overwrite = TRUE),
               "Cannot locate source ODK directory. Check target location of source ODK directory is correct.")
})

test_that("Error for no to", {
  expect_error(export_data(target = dirPath,
                           id = "stakeholders",
                           from = dirPath,
                           filename = "test.csv",
                           overwrite = TRUE),
               "Cannot locate destination folder for ODK Briefcase Storage. Check destination location is correct.")
})

test_that("Error for incorrect pem path", {
  expect_error(export_data(target = dirPath,
                           id = "odkr_encrypted_Test",
                           from = dirPath,
                           to = dirPath,
                           filename = "encrypted_test.csv",
                           overwrite = TRUE,
                           pem = "C:\\mistake\\in\\path"),
               "The pem file you specified cannot be found. Check your file path.")
})

test_that("Successful decryption process", {
  expect_equal(export_data(#briefcase = "ODK-Briefcase-v1.16.3",
                           target = dirPath,
                           id = "odkr_encrypted_Test",
                           from = dirPath,
                           to = dirPath,
                           filename = "decryption_test.csv",
                           overwrite = TRUE,
                           pem = system.file("inst/extdata", "odkrPrivateKey.pem", package = "odkr", mustWork = TRUE)),
                0)
})

test_that("CSV successfully written",{
  csvFileName <- "decryption_test.csv"
  csvFilePath <- paste0(dirPath, "\\", csvFileName)
  export_data(#briefcase = "ODK-Briefcase-v1.16.3",
              target = dirPath,
              id = "odkr_encrypted_Test",
              from = dirPath,
              to = dirPath,
              filename = "decryption_test.csv",
              overwrite = TRUE,
              group.names = FALSE,
              pem = system.file("extdata", "odkrPrivateKey.pem", package = "odkr", mustWork = TRUE))
  decrypted.df <- read.csv(csvFilePath, stringsAsFactors = FALSE)

})
