rm(list=ls())


if (dir.exists("C:/Users/Administrator/Documents/311")){
	setwd("C:/Users/Administrator/Documents/311") } else if
	(dir.exists("G:/math/651project")) { setwd("G:/math/651project")
	} else { setwd("C:/Users/michael/Documents/311") }

source("pdata.R")


calls<-as.data.frame(reg_dat)
calls<-calls[which(calls$wait < 15 & calls$wait > 0.08  ),]

head( calls  );ncol(calls)
summary(fit)
summary( lm(  log(wait) ~.,data=calls[,-2] ) )
anova( lm(  log(wait) ~.,data=calls[,-2] ) )

calls[2533,]

resids<-summary( lm(  log(wait)^2~.,data=calls[,-2] ) )$residuals
names(summary( lm( log(wait)~.,data=calls[,-2] ) ))
plot(lm(  (wait)~.,data=calls[,-2] )$fitted.values,
	lm(  (wait)~.,data=calls[,-2] )$residuals )
 
par(mfrow=c(2,2))
plot(lm(  log(wait)~.,data=calls[,-2] ) , which=c(1,1) )



######

boxplots<-list()
for ( i in 2: length(as.numeric(which(sapply(calls, is.numeric))) ) ){
	j<-	as.numeric(which(sapply(calls, is.numeric))) [i]
	boxplots[[i-1]]<-ggplot(calls, aes_string(x=factor(0),
		names(calls)[j] ) )  + geom_boxplot() +	 
		scale_x_discrete(breaks = NULL)  

	setwd( "C:/Users/Administrator/Documents/311/pics")
	ggsave( paste0("boxplot_",names(calls)[j] ,".pdf") ,
		plot=boxplots[[i-1]] ,width = 6, height = 6, 
		units = "in"   )  
	setwd( "C:/Users/Administrator/Documents/311/")
}


 
par(mar=c(4,4,4,4) )
plot(lm(  log(wait)~.,data=calls[,-2] ) , which=c(2,2) )
 


### nonindependence of error terms

resids<-summary( lm(  log(wait)^2~.,data=calls[,-2] ) )$residuals
tracts<- as.numeric( substr ( calls$block  ,8, 11) )/100
tracts<-data.frame(tracts,calls$wait)
tracts<-(tracts[order(tracts$tracts),] )

head(tracts) 

plot(tracts[,1],resids);abline(h=0, col="red")

## nonnormality of error terms
#distribution plots
boxplot(resids)

#comparison of frequencies
par(mfrow=c(2,2))
plot(lm(  log(wait)~.,data=calls[,-2] ))


#breusch pagan test
 
anova( lm(  log(wait) ~.,data=calls[,-2] ) ) 

resids<-summary( lm(  log(wait) ~.,data=calls[,-2] ) )$residuals
tc<-calls[,-c(1,2)]
tc$Y<-resids^2

((anova(lm(Y~.,data=tc))) )



SSR<-  sum( (anova(lm(Y~.,data=tc))) [-25,2] )
SSE<- anova( lm(    log(wait) ~.,data=calls[,-2] ) )[25,2]
(SSR/2 ) / (SSE/nrow(calls))^2

qchisq(.95,24)


#########



cr<-summary( lm(  log(wait)~., data=  calls[,-2])  )$residuals
anova( lm(  (wait)~., data=  calls[,-2])  )

CALLS<-calls[,-c(1,2)]
CALLS$Y<-cr^2
SSRstar<-sum(anova( lm(  (Y)~., data=CALLS )  )[-25,2])

(SSRstar/2) /

(anova( lm(  log(wait)~., data=  calls[,-2])  )[25,2]/nrow(calls))^2

qchisq(.99,24)


##### multicolianrity

head(calls)
X<-calls[,-1]

X<-X[,sapply(X, is.numeric)]
str(X)
head(X);nrow(X)
cX<-cor(X)

for ( i in 1:ncol(X)){	cX[,i][abs( cX[,i] ) < .4 ] <- 0 }
cX<-round(cX,3)
cX<-as.data.frame(cX)
names(cX)<-LETTERS[1:ncol(cX)]

pairs(X)

#vif
vif(fit) 
fitz<-lm(cyl~.,data=mtcars)
influence.measures(fitz)[[2]]
as.data.frame(influence.measures(fitz))


vifs<-rep(NA,length( ncol(X) ) )
for( i in 1:ncol(X) ){
	A<-paste( names(X)[i] ,"~",paste(names(X)[-i], collapse="+"),sep = "")

	vifs[i]<-(1-summary( lm(	A , data=X	) )$r.squared)^(-1)}

data.frame(vifs

