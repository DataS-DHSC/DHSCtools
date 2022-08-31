
#' Set git http and https proxies for DHSC
#'
#' Configure http and https proxy settings for our corporate firewall when using
#' git on the DHSC estate through a wired connection.
#'
#' Function uses \code{curl::ie_get_proxy_for_url} to read the Windows proxy
#' settings for \url{http://github.com} and \url{https://github.com} then uses
#' these to configure git with the \code{usethis::use_git_config} helper
#' function.
#'
#' NOTE: This requires that projects are configured / cloned from
#' GitHub using the HTTPS (not SSH) link.
#'
#' The function also needs to disable SSL verification but this
#' represents a minimal security risk when using the corporate firewall.
#'
#' @export
configure_git_proxy <- function() {
  usethis::use_git_config(
    http.proxy =
      stringr::str_split(
        curl::ie_get_proxy_for_url("http://github.com"),
        ";",
        simplify = TRUE
      )[1],
    http.Sslverify = "false",
    https.proxy =
      stringr::str_split(
        curl::ie_get_proxy_for_url("https://github.com"),
        ";",
        simplify = TRUE
      )[1],
    https.Sslverify = "false"
  )
}
