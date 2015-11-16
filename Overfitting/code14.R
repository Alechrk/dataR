d = read.csv(file.choose(),head=T)

library(DAAG)

model = lm(User.posts ~ Website_type  + User.fb.g.login + Cookie_F_M + Cookie_Age,data = d)
summary(model)

CVlm(data=d, model , m=3)

resido1 = Cookie_F_M * Cookie_Age
resido2 = Cookie_F_M * Website_type 
resido3 = Website_type *  Cookie_Age
resido4 = Website_type * Cookie_Age *Cookie_Age
resido5 = Cookie_F_M * Website_type * User.fb.g.login


q = data.frame(resido1,resido2,resido3,resido4,resido5,d)
 


modelx = lm(User.posts ~ Website_type  + User.fb.g.login + Cookie_F_M + Cookie_Age + resido1 + resido2 + resido3 + resido4 + resido5,data = q)
summary(modelx)

CVlm(data=q, modelx , m=3)