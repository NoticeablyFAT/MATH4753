#' ntickets - a function that calculates optimal passengers booked for a flight
#'
#' @param N Number of seats available
#' @param gamma Desired probability that a flight is overbooked
#' @param p Probability that a passenger will no-show
#'
#' @return Prints requested values, and also plots the results
#' @export
#'
#' @examples
#' \dontrun{ntickets(200,.02,0.95)}
ntickets=function(N,gamma,p){
  nd = 0
  nc = 0
  upper = N*1.1
  lower = N

  for(i in lower:upper){
    dnumpass = qbinom(1-gamma,i, p)
    if(dnumpass == N){
      nd = i
      cat("Number of passengers required calculated using binomial distribution nd = ")
      cat(nd)
      cat("\n")
      break
    }
  }


  for(i in lower:upper){
    cnumpass = qnorm(1-gamma,i*p,sqrt(i*p*(1-p))) -.5

    if(cnumpass>N){
      nc =  i-1
      cat("Number of passengers required calculated using normal distribution nc = ")
      cat(nc)
      cat("\n")

      break
    }
  }

  cat("P = ")
  cat(p)
  cat("\n")

  cat("Gamma = ")
  cat(gamma)
  cat("\n")



  n <-seq(N,floor(N+N/10),by =1)
  ObjectiveD <- 1- gamma -pbinom(q=N,size = n,prob = p)
  ObjectiveC <- 1-pnorm(N,n*p,sd=sqrt((n)*p*(1-p)))



  plot(n, ObjectiveD)
  title("Plot made using discrete distribution")
  abline(v=nd, col="red")
  abline(h=0, col="red")
  plot(n, ObjectiveC)
  title("Plot made using continuous distribution")
  abline(v=nc, col="red")
  abline(h=0, col="red")

}
