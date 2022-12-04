#' mynbin - a function to calculate a binomial distribution
#'
#' @param y Number of trials until r successes are observed
#' @param r Number of successes to be observed
#' @param p Probability of success
#'
#' @return A negative binomial probability distribution
#' @export
#'
#' @examples
#' \dontrun{mynbin(10,3,0.4)}
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
