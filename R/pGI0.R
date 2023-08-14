#' Cumulative density function of the GI0 distribution
#'
#' @param p_alpha negative value that controls the roughness
#' @param p_gamma positive value that controls the scale
#' @param p_Looks value larger than 1 that controls the number of looks
#' @param log.p optional logical parameter that switches to the logarithm
#' @keywords Distribution
#' @export
#' @examples
#' x <- seq(0,5,length.out=500)
#' plot(x,pGI0(x, p_alpha=-1, p_gamma=2, p_Looks=2), type='l')

pGI0 <- function(x, p_alpha, p_gamma, p_Looks,log.p = FALSE) {
  if(p_alpha < 0 & p_gamma > 0 & p_Looks >= 1){

    #if x is 0 or negative, the function returns 0, else return the cdf
    return(ifelse(x<=0, 0,
                  pf(-p_alpha * x / p_gamma, df1 = 2*p_Looks, df2=-2*p_alpha, log.p=log.p)))
  }else{
    print("Invalid parameters: check alpha is less than 0, gamma is greater than 0, and looks is greater than or equal to 1")
  }
}

