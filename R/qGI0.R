#' Quantile  function of the GI0 distribution
#'
#' @param p_alpha negative value that controls the roughness
#' @param p_gamma positive value that controls the scale
#' @param p_Looks value larger than 1 that controls the number of looks
#' @param lower.tail optional logical parameter that computes only the lower tail
#' @param log.p optional logical parameter that switches to the logarithm
#' @keywords Distribution, Quantile
#' @export
#' @examples
#' p <- seq(0,1,length.out=100)
#' plot(p,qGI0(p, p_alpha=-1, p_gamma=2, p_Looks=2), type='l')


qGI0 <- function(p, p_alpha, p_gamma, p_Looks, lower.tail=TRUE, log.p=FALSE) {
  if(p_alpha < 0 & p_gamma > 0 & p_Looks >= 1){

    #p is a vector of probabilities, i.e.  0<=p <1
    return(ifelse(p>=0 & p<1,
                  qf(p, df1=2*p_Looks, df2=-2*p_alpha,log.p=log.p, lower.tail=lower.tail), #note: swapped order of last two parameters
                  NaN))
  }else{
    print("Invalid parameters: check alpha is less than 0, gamma is greater than 0, and looks is greater than or equal to 1")
  }
}


