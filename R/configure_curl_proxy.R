
#' Set internet proxy when on DHSC estate
#'
#' Configure proxy settings for functions based on curl using Windows settings.
#' Needed when on the DHSC estate using a wired connection due to corporate
#' firewall.
#'
#' While R uses the \code{wininet} download method that correctly obtains
#' proxy details from Windows via the WinINet functions, other packages
#' such as \pkg{httr} are based on \pkg{curl} and so need the proxy settings
#' configured when using a wired connection on the DHSC estate due to
#' the corporate firewall. This function uses \code{curl::ie_get_proxy_for_url}
#' to read these settings and then applies them using \code{httr::set_config}.
#'
#' @param target_url The url to get the proxy settings for, defaults to \url{https://www.google.co.uk}.
#'
#' @export
#'
#' @examples
#' configure_curl_proxy()
#' httr::content(httr::GET("https://api.ipify.org"), encoding = "UTF-8")
configure_curl_proxy <- function(target_url = "https://www.google.co.uk") {
  httr::set_config(
    httr::use_proxy(
      url = curl::ie_get_proxy_for_url(target_url = target_url),
      auth = "ntlm"
    )
  )
}
