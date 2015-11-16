library(lmerTest)



m1 <- lmer(Informed.liking ~ Product*Information + (1|Product:Consumer) , data=ham)



m2 <- lmer(Informed.liking ~ Product + Information + Product*Information + (1|Product:Consumer) , data=ham)



m3 <- lmer(Informed.liking ~  Product:Information + (1|Consumer )  , data=ham)


 
m4 <- lmer(Informed.liking ~  Product + Information + Product:Information + (1|Consumer )  , data=ham)






#Product
m1 <- lmer(Informed.liking ~ Product +  (1|Information) , data=ham)

#Product + Product-Consumer
m1 <- lmer(Informed.liking ~ Product/Consumer +  (1|Information) , data=ham)

#Product + Consumer + Product-Consumer
m1 <- lmer(Informed.liking ~ Product*Consumer +  (1|Information) , data=ham)



#Product + Consumer + Product-Consumer / Numerical problems
m1 <- lmer(Informed.liking ~ Product*Information + (0 + Age|Consumer) + ( 1 |Consumer), data=ham)

ham2 = ham
ham2$Age = log(ham$Age)

#Product + Consumer + Product-Consumer
m1 <- lmer(Informed.liking ~ Product*Information + (0 + Age|Consumer) + ( 1 |Consumer), data=ham2)

#Product + Consumer + Product-Consumer
m1 <- lmer(Informed.liking ~ Product*Information + (1 + Age|Consumer), data=ham2)


#Product + Consumer + Product-Consumer
m1 <- lmer(Informed.liking ~ Product*Information + (1 + Age|Consumer), data=ham2)
plot(difflsmeans(m1))
difflsmeans(m1)


m1 <- lmer(Informed.liking ~ Product*Information+ (1 + Age|Consumer), data=ham2)
plot(difflsmeans(m1))
difflsmeans(m1)

rand(m1)
s <- step(m)
s$model

=0.05/6 = 






