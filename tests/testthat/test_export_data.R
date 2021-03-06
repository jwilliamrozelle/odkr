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

  # HELPER VARIABLES
  ## create flexible naming components for file
  test_dir <- "./inst/testdata/"
  briefcase_test_name <- "test_odkbc"
  ## identify the directory that will be used in testing
  form_name <- "odkr_encrypted_Test"
  ## name test file & directory
  csvFileName <- paste0(form_name, ".csv")
  csvFilePath <- paste0(test_dir, "/", csvFileName)

  # CLEAN UP
  ## When the test finishes, delete r objects
  on.exit(rm(briefcase_test_name,
             test_dir,
             form_name,
             csvFileName,
             csvFilePath),
          add = TRUE,
          after = FALSE)
  ## remove csv file resulting from the test
  on.exit(file.remove(csvFilePath), add = TRUE, after = FALSE)

  # TESTS
  ## Test successful completion of decryption without detected errors
  expect_equal(export_data(briefcase = briefcase_test_name,
                           target = test_dir,
                           id = form_name,
                           from = test_dir,
                           to = test_dir,
                           filename = csvFileName,
                           overwrite = TRUE,
                           group.names = FALSE,
                           pem = system.file("extdata",
                                             "odkrPrivateKey.pem",
                                             package = "odkr",
                                             mustWork = TRUE)),
                0)

  decrypted.df <- read.csv(csvFilePath)

  ## Test whether decryption was successful by looking for this UUID in the resulting csv
  expect_true("uuid:6e836c48-fd2f-4bc4-957a-a63d87f1e0a9" %in% decrypted.df$KEY)

})
