
N = 16000
G = 4000


randeff1 = rnorm(G,0,0.7)
randeff2 = rnorm(G,0,1.2)
resid   = rnorm(N,0,1)
price   = runif(N,0,17)
group   = 0
y       = numeric(N)
groups  = numeric(N)

for (i in 1:N){
	group    = group + 1
	groups[i]= group
	y[i]     = randeff1[group] + price[i]*(-3 + randeff2[group]) + resid[i] + 10 

	if (group == G){
		group = 0;
	} 
}
 

 
 

library(lme4)
fm2 <- lmer(y ~  price + ( 1 + price|groups),REML=FALSE)
summary(fm2)


library(lmerTest)



plot(fm2)
plot(fm2, groups~ resid(., scaled=TRUE))
plot(fm2, groups~ fitted(., scaled=TRUE))
hist(residuals(fm2))
qqnorm(residuals(fm2), main="Q-Q plot for residuals")
rr2 <- ranef(fm2)
library(lattice)
dotplot(ranef(fm2, postVar=T))
contrast.matrix <- rbind("Time:Diet1 vs. Time:Diet2" =  c(0, 0, 0, 0, 0, 1, 0, 0))
summary(glht(m, contrast.matrix))

