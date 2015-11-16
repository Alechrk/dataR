
l = read.csv(file.choose(),head=T)

#We will use libraries lmtest / sandwich
#################################################################################

model1 = lm(l$Logins ~ l$Traffic.inK + l$WebsiteConfig )
summary(model1)

plot(model1)
bptest(model1)

Breusch-Pagan test
 

################################################################################

model2 = lm(l$Value ~ l$Traffic + l$WebsiteConfig , weight = 1/(l$Traffic^2) )
plot(model2)

################################################################################




################################################################################
l$Traffic_log   = log(l$Traffic.inK)
l$Traffic_1     = (l$Traffic.inK)^(1/1.2)
model2 = lm(l$Logins ~ l$Traffic_1 + l$WebsiteConfig )

plot(model2)
bptest(model2)

################################################################################

coeftest(model1,vcovHC(model1,type="HC2"))
coeftest(model1,vcovHC(model1,type="HC0"))
coeftest(model1,vcov(model1)) 

################################################################################

