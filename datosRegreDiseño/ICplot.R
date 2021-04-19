ICplot <- function(modelo,fac,alpha=0.05)
# Programmed by E.Caro - Version 27/02/2011
{
tabla <- model.tables(modelo, type = "mean")
xbar <- tabla$table[[fac]]
num_dat <-tabla['n']
num <- num_dat$n[fac]

numall = 1
for(i in 1:length(num[[fac]]))  numall[i] = num[[fac]][i]

t <- qt((1-alpha/2),modelo$df.residual)
sr2 <- sum((modelo$residuals)^2)/modelo$df.residual
sr <- sqrt(sr2)
ancho <- t*sr/sqrt(numall)

ncol  = length(xbar)
xlabel =  modelo$xlevels[[fac]]

plot(c(1:ncol, 1:ncol), c(xbar+ancho, xbar-ancho), col = 0, 
xlab = fac, xaxt = "n", ylab = "medias")
   axis(side=1, at=seq(1,ncol), paste(xlabel))

arrows(1:ncol,xbar+ancho,1:ncol,xbar-ancho,angle=90,code=3,length=.1,
lwd = 2, col = 259)
points(1:ncol, xbar, lwd = 10, col = "white")
points(1:ncol, xbar, lwd = 3, col = "blue")

}
