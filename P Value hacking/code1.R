summary(lm(mtcars$mpg~mtcars$cyl+mtcars$hp))

"H0:cylinder = -2.26469"

tm1 = (-2.26469 - -2.26469)/0.57589

pvalue1 = 2*(1 - pt(tm1,29))




tm2 = (-2.2 - -2.26469)/0.57589

pvalue2 = 2*(1 - pt(tm2,29))




tm3 = (-12.2 - -2.26469)/0.57589

pvalue3 = 2*(pt(tm3,29))