###
#Results from the linear models asignment (week 6)
##
#Results from the regression (just having Month and watershed as factors)
Month of Septmeber above 0, why not showing october? 
NBi and NBE above 0, why not showing NBR?

#results from model comparing the average MeHg values in each watershed
> pr(lm1 <- lm(MeHg~watershed-1,data=waterMeHg))
Estimate Std. Error t value Pr(>|t|)
watershedNBR  0.02739    0.00774    3.54  0.00097
watershedNBE  0.09496    0.00774   12.26  8.6e-16
watershedNBi  0.03865    0.00800    4.83  1.7e-05

#CI
#predict(lm1,newdata=data.frame(watershed=c("NBR","NBE","NBi")),
#        +         interval="confidence")
fit        lwr        upr
1 0.02738645 0.01178296 0.04298994
2 0.09495560 0.07935212 0.11055909
3 0.03864744 0.02253222 0.05476265

#results from emmeans
watershed emmean      SE df lower.CL upper.CL
NBR       0.0274 0.00774 44   0.0118   0.0430
NBE       0.0950 0.00774 44   0.0794   0.1106
NBi       0.0386 0.00800 44   0.0225   0.0548

#results from the pr(lm(MeHg~watershed,data=water,contrasts=list(oxygen=contr.sum)))
Estimate Std. Error t value Pr(>|t|)
(Intercept)    0.0267     0.0102    2.63  0.01880
watershedNBE   0.0600     0.0144    4.18  0.00081
watershedNBI   0.0116     0.0144    0.81  0.43065
Warning message:
  In model.matrix.default(mt, mf, contrasts) :
  variable 'oxygen' is absent, its contrast will be ignored