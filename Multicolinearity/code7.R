
l = read.csv(file.choose(),head=T)


model = lm(l$Sales.Value ~ l$Bounce.rate + l$Views...Campaign.A + l$Views...Campaign.B + l$Website.Type + l$Promotion.yes.no)
summary(model)


X = model.matrix(model)


I = solve(t(X)%*%X)%*%t(X)%*%l$Sales.Value

eigen(t(X)%*%X)


VIFj = 1/(1-0.9)

>10


library(usdm)

vifcor(X, th=0.9) 

vifstep(X, th=6) 

 
