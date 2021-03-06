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

  # create flexible naming components for file
  briefcase_test_name <- "test_odk"
  test_odk_path <- paste0(dirPath, "/", briefcase_test_name, ".jar")

  get_briefcase(dirPath,
                briefcase = briefcase_test_name,
                version_dl_url = "https://github.com/getodk/briefcase/releases/download/v1.11.3/ODK-Briefcase-v1.11.3.jar")

  # identify the directory that will be used in testing
  form_name <- "odkr_encrypted_Test"
  odkbc_test_path <- paste0(dirPath, "/ODK Briefcase Storage/forms/", form_name)

  # name test file & directory
  csvFileName <- paste0(form_name, ".csv")
  csvFilePath <- paste0(dirPath, "/", csvFileName)

  # pull data in preparation
  pull_remote(
    target = dirPath,
    sd = TRUE,
    id = form_name,
    username = "validtrial",
    password = "zEF-STN-5ze-qom",
    from = "https://odk.ona.io/validtrial",
    to = dirPath
  )

  # CLEAN UP
  ## When the test finishes, delete r objects
  on.exit(rm(odkbc_test_path, briefcase_test_name, test_odk_path),
          add = TRUE,
          after = FALSE)
  ## When the test finishes, delete the downlaoded directory
  on.exit(unlink(odkbc_test_path, recursive = TRUE),
          add = TRUE,
          after = FALSE)
  ## Remove the testing version of odk_briefcase
  on.exit(file.remove(test_odk_path), add = TRUE, after = FALSE)
  ## remove csv file resulting from the test
  on.exit(file.create(csvFilePath), add = TRUE, after = FALSE)


  expect_equal(export_data(briefcase = briefcase_test_name,
                           target = dirPath,
                           id = form_name,
                           from = dirPath,
                           to = dirPath,
                           filename = csvFileName,
                           overwrite = TRUE,
                           group.names = FALSE,
                           pem = system.file("extdata", "odkrPrivateKey.pem", package = "odkr", mustWork = TRUE)),
                0)

  decrypted.df <- read.csv(csvFilePath)

  expect_true("uuid:6e836c48-fd2f-4bc4-957a-a63d87f1e0a9" %in% decrypted.df$KEY)

})

# test_that("CSV successfully written",{
#
#   # create flexible naming components for file
#   briefcase_test_name <- "test_odk"
#   test_odk_path <- paste0(dirPath, "/", briefcase_test_name, ".jar")
#
#   # cleanup the downloaded example
#   on.exit(rm(briefcase_test_name, test_odk_path), add = TRUE, after = FALSE)
#   on.exit(file.remove(test_odk_path), add = TRUE, after = FALSE)
#
#   # name test file & directory
#   csvFileName <- "decryption_test.csv"
#   csvFilePath <- paste0(dirPath, "\\", csvFileName)
#
#   export_data(briefcase = briefcase_test_name,
#               target = dirPath,
#               id = "odkr_encrypted_Test",
#               from = dirPath,
#               to = dirPath,
#               filename = "decryption_test.csv",
#               overwrite = TRUE,
#               group.names = FALSE,
#               pem = system.file("extdata", "odkrPrivateKey.pem", package = "odkr", mustWork = TRUE))
#   decrypted.df <- read.csv(csvFilePath, stringsAsFactors = FALSE)
#
# })
