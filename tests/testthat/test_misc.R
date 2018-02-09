context("Miscellaneous Tests")

is_url_readable <- function(url) {
  # test connection by trying to read first line of url
  test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
  
  # return FALSE if test inherits 'try-error' class
  !inherits(test, "try-error")
}

# test if online
is_online <- function(url = "http://www.google.com") {
  # test the http capabilities of the current R build
  if (!capabilities(what = "http/ftp"))
    return(FALSE)
  
  is_url_readable(url)
}

test_that("COPASI_BIN_VERSION is count", {
  expect_type(COPASI_BIN_VERSION, "integer")
  expect_length(COPASI_BIN_VERSION, 1L)
  expect_gt(COPASI_BIN_VERSION, 0L)
})

test_that("COPASI_BIN_HASHES are valid hashes", {
  hashes <- unlist(COPASI_BIN_HASHES)
  # for now expect 3 entries
  expect_length(hashes, 3L)
  expect_match(hashes, "^[A-Fa-f0-9]{64}$")
})

test_that("libs on server are accessible", {
  skip_if_not(is_online())
  
  for (arch in names(COPASI_BIN_HASHES)) {
    for (os in names(COPASI_BIN_HASHES[[arch]])) {
      expect_true(
        is_url_readable(dl_url_former(os = os, arch = arch)),
        paste0("Cannot access file for os '", os, "' and arch '", arch, "'.")
      )
    }
  }
})