
N = 160
G = 40


randeff = rnorm(G,0,4)
resid   = rnorm(N,0,1.4 )
group   = 0
y       = numeric(N)
groups  = numeric(N)

for (i in 1:N){

	group    = group + 1
	groups[i]= group
	y[i]     = randeff[group] + resid[i] + 10 

	if (group == G){
		group = 0;
	} 
}


fm2 <- lmer(y ~  1 + (1|groups),REML=FALSE)
summary(fm2)





dlm = optim(par=c(1,11,10),loglikelihood,control=c(fnscale=-1))



loglikelihood <- function( xx){

      xx     = c(2.17,10.34,10.31)  
	sigma1 = xx[1]
	sigma2 = xx[2]
	beta   = xx[3]
	together     =  data.frame(groups,y)
	together     <- together     [order(groups),]
	T = matrix(nrow=N/G,ncol=N/G,sigma2)
	diag(T) =  sigma1 + sigma2
	 
	I = matrix(0,G,G)
	I = diag(G)
	V = kronecker(I,T)
	x = rep.int(1, N)
	
      
	L = -0.5*log(det(V)) -0.5* t(together$y-x * beta ) %*% solve(V) %*% (together$y-x * beta) + -N/2*log(2* pi)
	return(L)
}

 