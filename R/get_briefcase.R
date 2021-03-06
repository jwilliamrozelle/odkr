################################################################################
#
#' Updates pre-installed ODK Briefcase \code{jar} file to the latest version
#' downloaded from \url{https://opendatakit.org}.
#'
#' @param destination Path to directory where ODK Briefcase \code{.jar} file
#'     will be downloaded into
#' @param briefcase Filename of the downloaded ODK Briefcase \code{.jar} file.
#'     Default is \code{"odkBriefcase_latest"}
#' @param version_dl_url This is the URL to a specific version of briefcase. Will
#'     download the most recent version of briefcase by default.
#'
#' @return NULL
#'
#' @examples
#'   # Get latest version of ODK Briefcase and save in a temporary directory
#'     \dontrun{
#'       dirPath <- tempdir()
#'       get_briefcase(destination = dirPath)
#'     }
#' @export
#'
#
################################################################################

get_briefcase <- function(destination = "",
                          briefcase = "odkBriefcase_latest",
                          version_dl_url = NULL) {

  ## Check if appropriate Java runtime version is available
  rJava::.jinit()
  jv <- rJava::.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
  if(substr(jv, 1L, 2L) == "1.") {
    jvn <- as.numeric(paste0(strsplit(jv, "[.]")[[1L]][1:2], collapse = "."))
    if(jvn < 1.8) stop("Java >= 8 is needed for this package but not available")
  }

  ## Check that destination has been specified
  if(destination == "") {
    stop("No destination path for ODK Briefcase specified. Try again.", call. = TRUE)
  }

  # Detect that the download is not a proper URL for briefcase
  if(!is.null(version_dl_url) &&
     !endsWith(version_dl_url, ".jar") &&
     !startsWith(version_dl_url, "https://github.com/getodk/briefcase")) {
    stop("This does not appear to be a proper URL for downloading briefcase. It should be a .jar file.", call. = TRUE)
  }

  # Warn users to change file name of download
  if(!is.null(version_dl_url) & briefcase == "odkBriefcase_latest") {
    warning("WARNING: You have specified a url for briefcase download, but have not changed the 'odkBriefcase_latest' label. It is recommended that you use a unique name for this version of briefcase.")
  }

  ## If a specific dl url is specified, use that, otherwise, get the most recent version of odk.
  if(!is.null(version_dl_url)) {
    download.url <- version_dl_url

  } else {

  ## Get the url for latest release download of Briefcase from GitHub
  x <- curl::curl("https://api.github.com/repos/getodk/briefcase/releases/latest")

  ## Close connection to Briefcase download URL
  on.exit(expr = close(x))

  y <- readLines(x, warn = FALSE)
  z <- unlist(stringr::str_split(y, pattern = ","))
  download.url <- stringr::str_extract(string = z[stringr::str_detect(z, "browser_download_url")],
                                       pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+")
  }

  ## Download Briefcase from GitHub
  ## Errors handled by tryCatch
  tryCatch({
  download.file(url = download.url,
                destfile = paste0(destination, "/", briefcase, ".jar"),
                mode = "wb")
  },

  error = function(err) {
    return(structure(err, class = "try-error"))
    message("Download failed.")
    if (!is.null(version_dl_url)) {
      message("This is likely due to an error in the url.")
    }

  })
}
