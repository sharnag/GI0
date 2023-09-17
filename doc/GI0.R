## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GI0)

## ---- fig.width=6, fig.height=5, fig.align='center', warning=FALSE------------
# Create the sequence of input values
x <- seq(0.001, 10, length=100)
# Apply the dGI) function and plot the output in a line graph
plot(x, dGI0(x, p_alpha=-1, p_gamma=1, p_Looks=1), type = "l", lwd = 2,
     xlab = expression(italic(z)), ylab = "Density",
     main="GI0 Densities")
lines(x, dGI0(x, p_alpha=-2, p_gamma=1, p_Looks=1), col="red",lty=1, lwd = 2)
lines(x, dGI0(x, p_alpha=-1, p_gamma=5, p_Looks=1), col="blue",lty=1, lwd = 2)
lines(x, dGI0(x, p_alpha=-1, p_gamma=1, p_Looks=3), col="green",lty=1, lwd = 2)

l1<- expression(paste(alpha, "=-1 ", gamma, "=1 L=1"))
l2<- expression(paste(alpha, "=-2 ", gamma, "=1 L=1"))
l3<- expression(paste(alpha, "=-1 ", gamma, "=5 L=1"))
l4<- expression(paste(alpha, "=-1 ", gamma, "=1 L=3"))

legend(x = "topright",          
       legend = c(l1, l2, l3, l4),  
       lty = c(1,1,1,1),          
       col = c("black", "red","blue","green"),        
       lwd = 2)   
# Semi log scale
plot(x, dGI0(x, p_alpha=-1, p_gamma=1, p_Looks=1), type = "l", lwd = 2, 
     xlab = expression(italic(z)), ylab = "log(Density)", 
     main="GI0 Densities", log="y")
lines(x, dGI0(x, p_alpha=-2, p_gamma=1, p_Looks=1), col="red",lty=1, lwd = 2)
lines(x, dGI0(x, p_alpha=-1, p_gamma=5, p_Looks=1), col="blue",lty=1, lwd = 2)
lines(x, dGI0(x, p_alpha=-1, p_gamma=1, p_Looks=3), col="green",lty=1, lwd = 2)

legend(x = "topright",          
       legend = c(l1, l2, l3, l4),  
       lty = c(1,1,1,1),          
       col = c("black", "red","blue","green"),        
       lwd = 2) 

## ---- fig.width=6, fig.height=5, fig.align='center', warning=FALSE------------
plot(x, pGI0(x, p_alpha=-1, p_gamma=1, p_Looks=1), type = "l", lwd = 2, 
     xlab = expression(italic(z)), ylab = "Probability", 
     main="Cumulative Distribution Function for GI0")
lines(x, pGI0(x, p_alpha=-2, p_gamma=1, p_Looks=1), col="red",lty=1, lwd = 2)
lines(x, pGI0(x, p_alpha=-1, p_gamma=5, p_Looks=1), col="blue",lty=1, lwd = 2)
lines(x,pGI0(x, p_alpha=-1, p_gamma=1, p_Looks=3), col="green",lty=1, lwd = 2)

legend(x = "bottomright",          
       legend = c(l1, l2, l3, l4),  
       lty = c(1,1,1,1),          
       col = c("black", "red","blue","green"),        
       lwd = 2) 
#Semi-log scale
plot(x, pGI0(x, p_alpha=-1, p_gamma=1, p_Looks=1), type = "l", lwd = 2, 
     xlab = expression(italic(z)), ylab = "log(Probability)", 
     main="Cumulative Distribution Function for GI0", log="y")
lines(x, pGI0(x, p_alpha=-2, p_gamma=1, p_Looks=1), col="red",lty=1, lwd = 2)
lines(x, pGI0(x, p_alpha=-1, p_gamma=5, p_Looks=1), col="blue",lty=1, lwd = 2)
lines(x, pGI0(x, p_alpha=-1, p_gamma=1, p_Looks=3), col="green",lty=1, lwd = 2)

legend(x = "bottomright",          
       legend = c(l1, l2, l3, l4),  
       lty = c(1,1,1,1),          
       col = c("black", "red","blue","green"),        
       lwd = 2) 

## ---- fig.width=6, fig.height=5, fig.align='center', warning=FALSE------------
p <- seq(0,1,length=50)
plot(p,qGI0(p, p_alpha=-1, p_gamma=2, p_Looks=2), type = "l", lwd = 2, 
     xlab = expression(italic(p)), ylab = "Quantile", 
     main="Quantile Function for GI0")
lines(p, qGI0(p, p_alpha=-2, p_gamma=2, p_Looks=2), col="red",lty=1, lwd = 2)
lines(p,qGI0(p, p_alpha=-1, p_gamma=2, p_Looks=3), col="green",lty=1, lwd = 2)

legend(x = "topleft",          
       legend = c(l1, l2, l4),  
       lty = c(1,1,1),          
       col = c("black", "red","green"),        
       lwd = 2) 
# Semi-log scale
plot(p,qGI0(p, p_alpha=-1, p_gamma=2, p_Looks=2), type = "l", lwd = 2, 
     xlab = expression(italic(p)), ylab = "log(Quantile)", 
     main="Quantile Function for GI0", log="y")
lines(p, qGI0(p, p_alpha=-2, p_gamma=2, p_Looks=2), col="red",lty=1, lwd = 2)
lines(p,qGI0(p, p_alpha=-1, p_gamma=2, p_Looks=3), col="green",lty=1, lwd = 2)

legend(x = "topleft",          
       legend = c(l1, l2, l4),  
       lty = c(1,1,1),          
       col = c("black", "red","green"),        
       lwd = 2) 

## ---- fig.width=6, fig.height=5, fig.align='center'---------------------------
# Sample from the GI0 distirbution
set.seed(111)
z<- rGI0(1000, p_alpha=-2, p_gamma=4, p_Looks=3)
df <- data.frame(z)

# Plot a histogram of the random draws, overlay the kernel density
# Use Freedman-Diaconis equation  to calculate the number of bins

hist(df$z, breaks = "Freedman-Diaconis", prob=TRUE, 
     xlab=expression(italic(z)), ylab="Density",
     main="Distribution of 1000 random draws from GI0 distribution") 
lines(density(df$z), col = "blue", lwd=2)

## ---- fig.width=6, fig.height=5, fig.align='center'---------------------------
hist(df$z, breaks = "Freedman-Diaconis", prob=TRUE, 
     xlab=expression(italic(z)), ylab="Density", 
     main="Distribution of 1000 random draws from GI0 distribution",
     xlim = c(0, 20)) 
lines(density(df$z), col = "blue", lwd=2)

## ---- fig.align='center', out.width="90%"-------------------------------------
# set.seed(123)
# rGI0(1000, p_alpha=-3, p_gamma=2, p_Looks=3)
knitr::include_graphics("CullenFreyPlot.png")

