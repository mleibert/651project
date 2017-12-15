
# F test for regression relation

(sum(anova( lm(  log(wait) ~.,data=calls[,-2] ) )[-25,2])/24)/
anova( lm(  log(wait) ~.,data=calls[,-2] ) )[25,2]

anova( lm(  log(wait) ~.,data=calls[,-2] ) )

p=24
qf(.95,p-1,nrow(calls)-p)

