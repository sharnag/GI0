#' Pseudorandom random number generator for the GI0 distribution
#'
#' @param n the sample size
#' @param p_alpha negative value that controls the roughness
#' @param p_gamma positive value that controls the scale
#' @param p_Looks value larger than 1 that controls the number of looks
#' @param from.F optional logical parameter that sets the type of generator
#' @export
#' @examples
#' set.seed(123)
#' rGI0(10, p_alpha=-1, p_gamma=2, p_Looks=2)


rGI0 <- function(n, p_alpha, p_gamma, p_Looks, from.F=FALSE){
  if(p_alpha < 0 & p_gamma > 0 & p_Looks >= 1){
    ifelse(from.F==TRUE,
           return(rf(n, df1=2*p_Looks, df2=-2*p_alpha)),
           return(
             rgamma(n, shape=p_Looks, rate=p_Looks) /
               rgamma(n, shape=-p_alpha, rate=p_gamma)
           )
    )
  }else{
    print("Invalid parameters: check alpha is less than 0, gamma is greater than 0, and looks is greater than or equal to 1")
  }


}
