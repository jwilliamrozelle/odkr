library(odkr)
context("Check get_briefcase function")

# Test warning for no when specific version of ODK is downloaded but no new name is applied.
test_that("Warning for default naming with custom verison", {

  # create flexible naming components for file
  briefcase_test_name <- "test_odk"
  test_odk_path <- paste0(dirPath, "/odkBriefcase_latest.jar")

  # cleanup the downloaded example
  on.exit(rm(briefcase_test_name, test_odk_path), add = TRUE, after = FALSE)
  on.exit(file.remove(test_odk_path), add = TRUE, after = FALSE)

  # run test
  expect_warning(get_briefcase(destination = dirPath,
                               version_dl_url = "https://github.com/getodk/briefcase/releases/download/v1.11.3/ODK-Briefcase-v1.11.3.jar"),
               "WARNING: You have specified a url for briefcase download, but have not changed the 'odkBriefcase_latest' label. It is recommended that you use a unique name for this version of briefcase.")
})

# test error handling for missing destination path
test_that("Warning for missing destination path", {
  expect_error(get_briefcase(),
                 "No destination path for ODK Briefcase specified. Try again.")
})

# test error for incorrect custom URL
test_that("Error for wrong URL", {
  expect_error(get_briefcase(dirPath,
                             briefcase = briefcase_test_name,
                             version_dl_url = "https://mistakenurl.com/thisiswrong"),
               "This does not appear to be a proper URL for downloading briefcase. It should be a .jar file.")
})


# test that for an error when using an incorrect url
test_that("Warning for missing destination path", {
  expect_error(get_briefcase(version_dl_url = "https://errortestwarning.com"),
               "No destination path for ODK Briefcase specified. Try again.")
})


# test working code, with all functions
test_that("Successful download", {

  # create flexible naming components for file
  briefcase_test_name <- "test_odk"
  test_odk_path <- paste0(dirPath, "/", briefcase_test_name, ".jar")

  # cleanup the downloaded example
  on.exit(rm(briefcase_test_name, test_odk_path), add = TRUE, after = FALSE)
  on.exit(file.remove(test_odk_path), add = TRUE, after = FALSE)

  expect_equal(get_briefcase(dirPath,
                             briefcase = briefcase_test_name,
                             version_dl_url = "https://github.com/getodk/briefcase/releases/download/v1.11.3/ODK-Briefcase-v1.11.3.jar"),
  0)
})
