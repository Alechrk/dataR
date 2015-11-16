 d = read.csv(file.choose(),head=T)


model  = lm(d$Y ~ d$X - 1)
summary(model)






montecarloexample <- function(){

x = runif(100,0,1)*6
y = numeric(100)

beta = 2

	for (i in 1: 100){

		y[i]=rnorm(1,0,1) + beta*x[i]
	
		if ( i == 11){
			y[i] = 1000
		}

	}
	D = data.frame(y,x)
	return (D)
}

rt = montecarloexample()
model  = lm(rt$y ~ rt$x - 1)
summary(model)
