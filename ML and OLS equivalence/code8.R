
l = read.csv(file.choose(),head=T)


model = lm(l$Sales.Value ~ l$Bounce.rate + l$Views...Campaign.A )
logLik(model)

x    = model.matrix(model)
Beta = solve(t(x)%*%(x)) %*% (t(x)%*%l$Sales.Value) 
###exercise for the student .... CovMatrix = residual * solve(t(x)%*%(x))


curs  <- c(1.136e+03 , -0.850e+02  ,  0.0818606 , 900)
maxlik             = optim(par = curs, f1,
                     control=list(maxit=1000,fnscale = -1,
			    reltol=0.0000000000000001), hessian=TRUE)
fisher_info_matrix = -solve(maxlik$hessian)
prop_sigma<-sqrt(diag(fisher_info_matrix))
 

curs  <- c(2135.9262123, -485.3764433  ,  0.0818606)
optim(par = curs, f2, control=list(maxit=1000))





##############################MAXIMUM LIKELIHOOD###################################################################################
f1 <- function(x){
      b0    = x[1]
	b1    = x[2]
	b2    = x[3]
	sigma = x[4]
	ds    =  log(dnorm(l$Sales.Value - (b0 + b1 * l$Bounce.rate + b2 * 
	l$Views...Campaign.A),0,sigma)) 
	return(sum(ds))
}
##################################################################################################################################



 
##############################NUMERICALLY - SUM OF SQUARES###################################################################################
f2 <- function(x){
      b0    = x[1]
	b1    = x[2]
	b2    = x[3]
	ds    =  (l$Sales.Value - (b0 + b1 * l$Bounce.rate + b2 * l$Views...Campaign.A))^2
	return(sum(ds))
}
############################################################################################
##variance##################################################################################
f3 <- function(x){
      d = f3(curs)/335
	return(d)
}
###########################################################################################
