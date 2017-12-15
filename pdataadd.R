rm(list=ls())


if (dir.exists("C:/Users/Administrator/Documents/311")){
	setwd("C:/Users/Administrator/Documents/311") } else if
	(dir.exists("G:/math/651project")) { setwd("G:/math/651project")
	} else { setwd("C:/Users/michael/Documents/311") }

source("pdata.R")

#
calls<-as.data.frame(reg_dat)

head( calls  );ncol(calls)
summary(fit)
summary( lm(  log(wait) ~.,data=calls[,-2] ) )
anova( lm(  log(wait) ~.,data=calls[,-2] ) )


resids<-summary( lm(  log(wait)^2~.,data=calls[,-2] ) )$residuals
names(summary( lm( log(wait)~.,data=calls[,-2] ) ))
plot(lm(  (wait)~.,data=calls[,-2] )$fitted.values,
	lm(  (wait)~.,data=calls[,-2] )$residuals )
 
par(mfrow=c(2,2))
plot(lm(  log(wait)~.,data=calls[,-2] ) , which=c(1,1) )

#write.csv(calls,"ph.csv",row.names=F)

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

resids<-summary( lm(  log(wait) ~.,data=calls[,-2] ) )$residuals
tracts<- as.numeric( substr ( calls$block  ,8, 11) )/100
tracts<-data.frame(tracts,calls$wait)
tracts<-(tracts[order(tracts$tracts),] )

head(tracts) 

plot(tracts[,1],resids);abline(h=0, col="red")

## nonnormality of error terms
#distribution plots
boxplot(resids)
boxplot(resids ,ylim = c(-10,4))

#comparison of frequencies
par(mfrow=c(2,2))
plot(lm(  log(wait)~.,data=calls[,-2] ))


#breusch pagan test
 
cr<-summary( lm(  log(wait)~., data=  calls[,-2])  )$residuals

#new data set with the squared residuals and only the X variables
CALLS<-calls[,-c(1,2)];CALLS$Y<-cr^2   #$

tail(anova( lm(  Y~., data=  CALLS)  ) ,1)

q<-nrow(anova( lm(  Y~., data=  CALLS)  ))-1

SSRstar<-sum( anova( lm(  (Y)~., data=CALLS )  )[-(q+1),2] );SSRstar
SSE<-anova( lm(  log(wait) ~., data=  calls[,-2])  ) [q+1,2];SSE

(SSRstar/2)/(SSE/nrow(calls))^2
qchisq(.99,q)


#Y resids
head(rstudent( lm(log(wait)~.,data=calls[,-2]) ) );head(rstudent(fit))

Yresids<-data.frame(resids, hatvalues(fit) ,rstudent(fit) );head(Yresids)
n=nrow(calls);p=q+1
qt( 1-(.3/ (2*n) ) , n-p-1	)
Yresids$outlier<- abs(Yresids[,3]) < qt( 1-(.01/ (2*n) ) , n-p-1	)
which(Yresids$outlier == F)
summary( lm( log(wait) ~.,data=calls[-c( which(Yresids$outlier == F) ) ,
	-2] ) )$r.squared
summary( lm( log(wait) ~.,data=calls[-c( which(Yresids$outlier == F) ) ,
	-2] ) )

#X outliers
2*sum(hatvalues(fit))/nrow(calls)
p/nrow(calls)*2

Xout<-data.frame( hatvalues(fit), p/nrow(calls)*2 < hatvalues(fit))
names(Xout)<-c("hii","outlier")
tail(Xout)
summary( lm( log(wait) ~.,data=calls[-c( which(Xout[,2] == T) ) ,
	-2] ) )$r.squared

#influential value

#fitz<-lm(cyl~.,data=mtcars)
#influence.measures(fitz)[[2]]
fit.IM<-as.data.frame(influence.measures(fit)[[2]])
dim(fit.IM)
fit.IM<-apply(fit.IM,1  , function(x)	 T %in% x	)
fit.IM<-as.numeric(ifelse( fit.IM== T , 1, 0))
sum(fit.IM);
which(fit.IM == 1)


paste0("Original Model R^2: ", 
	summary( lm(  log(wait) ~.,data=calls[,-2] ) )$r.squared) 
paste0("Remove Y outliers model R^2: ", 	summary( lm(  
	log(wait) ~.,data=calls[which(Yresids$outlier == T),-2] ) )$r.squared) 
paste0("Remove X outliers model R^2: ", 	summary( lm(  log(wait) ~.,
	data=calls[  -which(Xout[,2] == T) ,-2] ) )$r.squared) 
paste0("Remove Influential Cases model R^2: ", 	summary( lm(  log(wait) ~.,
	data=calls[  -which(fit.IM == 1) ,-2] ) )$r.squared) 

summary( lm( log(wait) ~.,data=calls[-c(which(fit.IM == 1) ),-2] ) )$r.squared
 
OUT<-unique(c( which(Xout[,2] == T) , which(Yresids$outlier == F) ))

OUT<-unique(c( which(fit.IM == 1) , which(Yresids$outlier == F) ))

summary( lm( log(wait) ~.,data=calls[-c( which(Yresids$outlier == F) ) ,
	-2] ) )$r.squared
summary( lm(  log(wait) ~.,data=calls[,-2] ) )$r.squared
summary(lm(  log(wait)~.,data=calls[-OUT,-2] )   )$r.squared
summary(lm(  log(wait)~.,data=calls[,-2] )   )$r.squared



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

par(mfrow=c(2,3))
plot(calls$commute.time,calls$Prop.commuters)
plot(calls$pPoverty,calls$Prop.drivers)
plot(calls$Rent,calls$Income)
plot(calls$Prop.commuters,calls$hours.worked)
plot(calls$Pmoved,calls$pOwned )
plot(calls$pPoverty,calls$hours.worked)



#vif

vifs<-rep(NA,length( ncol(X) ) )
for( i in 1:ncol(X) ){
	A<-paste( names(X)[i] ,"~",paste(names(X)[-i], collapse="+"),sep = "")

	vifs[i]<-(1-summary( lm(	A , data=X	) )$r.squared)^(-1)}

require("QuantPsyc")
lm.beta(fit)

str(calls)
SRC<- lm.beta(fit)
SRC<- SRC[ -c(2,3,4,(length(SRC)-1):length(SRC))]

SRC<-c(0.078217698,-0.002703794,0.045347705,-0.023879212,-0.004266351,
	-0.004954046,-0.012441205,-0.014638336,-0.010452721,-0.069736623,
	0.076610637,-0.002186268,0.001522267,-0.018816095,-0.001020040,
	0.001100885)

length(SRC)

SRC<-data.frame(SRC,vifs)


SRC<-cbind( SRC[1:4,], SRC[5:8,]	,SRC[9:12,]	,SRC[13:16,]			)
names(SRC)<-c("VIF.1-4","SRC.1-4",	"VIF.5-8","SRC.5-8",
"VIF.9-12","SRC.9-12","VIF.13-16","SRC.13-16"	)
SRC