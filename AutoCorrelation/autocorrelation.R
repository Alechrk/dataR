
l = read.csv(file.choose(),head=T)

 
x = lm(l$Cereal.yield..kg.per.hectare.~Land.under.cereal.production..hectares.+
						   Agricultural.land..sq..km.             +
						   Arable.land..hectares. , data =l)	

summary(x)
   		
acf(x$residuals,type="correlation")   
dwtest(l, alt="two.sided")