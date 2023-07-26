#' Probability Density Function of Binomial Distribution
#'
#' Compute the probability density function of binomial distribution
#' @param x is the random variable
#' @param n is the number of samples
#' @param p is the success probability of each trial
#' @return the probability density function of binomial distribution
#' @examples
#' pdBinom1 <- pdBinom(1, 5, 0.6);
#' pdBinom2 <- pdBinom(6, 10, 0.25);
#' @export
pdBinom = function(x, n, p){
  binom = (factorial(n)/(factorial(n-x)*factorial(x)))*(p^x)*((1-p)^(n-x))
  return(binom)
}

#' Probability Density Function of Hypergeometric Distribution
#'
#' Compute probability density function of hypergeometric distribution
#' @param x is the random variable
#' @param n is the number of sample without replacement taken from N
#' @param M is the number of items desired characteristic in the population
#' @param N is the population size
#' @return The probability density function of hypergeometric distribution
#' @examples
#' pdHiper1 <- pdHiper(12, 15, 50, 60);
#' pdHiper2 <- pdHiper(1, 5, 3, 40);
#' @export
pdHiper = function(x, n, M, N){
  Hiper = ((factorial(M)/(factorial(M-x)*factorial(x)))*(factorial(N-M)/(factorial(N-M-(n-x))*factorial(n-x))))/(factorial(N)/(factorial(N-n)*factorial(n)))
  return(Hiper)
}

#' Probability Density Function of Poisson Distribution
#'
#' Compute probability density function of poisson distribution
#' @param x is the random variable
#' @param miu is the mean number of events
#' @return The probability density function of poisson distribution
#' @examples
#' pdPois1 <- pdPois(3, 2);
#' pdPois2 <- pdPois(1, 0.8);
#' @export
pdPois = function(x, miu){
  Pois = (exp(-miu)*(miu^x))/(factorial(x))
  return(Pois)
}

#' Probability Density Function of Geometric Distribution
#'
#' Compute probability density function of geometric distribution
#' @param x is the random variable
#' @param p is the success probability of each trial
#' @return The probability density function of geometric distribution
#' @examples
#' pdGeom1 <- pdGeom(5, 0.01);
#' pdGeom2 <- pdGeom(3, 0.8);
#' @export
pdGeom = function(x, p){
  Geom = p*((1-p)^(x-1))
  return(Geom)
}

#' Probability Density Function of Negative Binomial Distribution
#'
#' Compute probability density function of negative binomial distribution
#' @param x is the random variable
#' @param r is the number of successes
#' @param p is the success probability of each trial
#' @return The probability density function of negative binomial distribution
#' @examples
#' pdNB1 <- pdNB(15, 5, 0.2);
#' pdNB2 <- pdNB(5, 2, 0.25);
#' @export
pdNB = function(x, r, p){
  NB = (factorial(x-1)/(factorial(x-1-(r-1))*factorial(r-1)))*(p^r)*((1-p)^(x-r))
  return(NB)
}
