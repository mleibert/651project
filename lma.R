rm(list=ls())
require("data.table")
require("ggcorrplot")
require("ggplot2")


if( dir.exists("C:/Users/Administrator/Documents/311")  ) {
		setwd("C:/Users/Administrator/Documents/311")
	} else if (  dir.exists("G:/math/311")  ) {
		setwd("G:/math/311")
	} else    {   setwd("C:/Users/michael/Documents/311")} 

 
if( dir.exists("C:/Users/Administrator/Documents/311")  ) {
	calls<-fread("C:\\Users\\Administrator\\documents\\311v4.csv") 
	} else ( calls<-fread("311v4.csv") )
calls<-as.data.frame(calls)

head(calls)
CALLS<-calls
#calls<-CALLS

dnw<-c("ï..X","Y","SERVICECALLCOUNT","SERVICEORDERDATE","INSPECTORNAME",
	"STATUS_CODE","PRIORITY","STREETADDRESS","XCOORD","YCOORD",
	"CITY","STATE","DETAILS", "INSPECTIONDATE", "SERVICEORDERSTATUS" )
calls<-calls[ , -which(names(calls) %in% dnw )]

head(calls)

SC<-as.data.frame(sort((table(calls$SERVICECODEDESCRIPTION))))
which( names(calls) == "wait")
calls.lms<-list()

sum(is.na(calls$wait))
calls<-calls[-which(is.na(calls$wait)),];sum(is.na(calls$wait))
sc<-as.data.frame(sort((table(calls$SERVICECODEDESCRIPTION))))
sc<-merge(SC,sc,"Var1");names(sc)<-LETTERS[1:3]
 attach(sc)	
sc<- sc[order(B ),] 
detach(sc)



dnw<-c( 64235, 35438, 121265, 91609, 57565, 153705, 94496, 35438, 
	25588, 33990, 80250, 65387, 48847, 25588, 153992, 24735, 29542) 
b<-unique(calls[which(calls$Income == 0),]$block)

options(scipen=999)

for(i in 1:length(unique(calls[which(calls$Income == 0),]$block)) ){
	calls[which( calls$block == b[i] ),  ]$Income<-dnw[i] }


SC
calls.lms<-list()
calls.cor<-list()
calls.plot<-list()
correlations<-data.frame(rep(1,30))
for(i in 82:113){

	callss<-calls[which(calls$SERVICECODEDESCRIPTION== SC[i,1]),]
	callss<-callss[complete.cases(callss), ]

	head(callss)
	b<-which(names(callss) == "wait"):which(names(callss) == "late")
	dnw<-which(names(callss) == "precip_daily"):ncol(callss)

	callss<-(callss[,c(	b,dnw	)])
	head(callss)
	b<-which(names(callss) == "pBlack"):which(names(callss) == "pTwo.more")
	callss<-callss[which(callss$wait < 15 &  callss$wait > 0.04 ),]

	head(data.matrix(callss[,-c(2:3,b,6:8)]))
	calls.cor[[i]]<-cor(	data.matrix(callss[,-c(2:3,b,6:8)])	)
	dnw<-cor(	data.matrix(callss[,-c(3,b,6:8)]))
	calls.cor[[i]]<-as.data.frame(calls.cor[[i]][,1])
	calls.cor[[i]][-1,1][ abs( calls.cor[[i]][-1,1] ) <.35 ] <-as.integer(0)
	calls.cor[[i]]$Cor<-ifelse( calls.cor[[i]][,1] != 0 , "*" , "")
	calls.cor[[i]]<-calls.cor[[i]][-1,]
 	calls.cor[[i]][,1]<-round(calls.cor[[i]][,1],4)
	names( calls.cor[[i]] )[1]<-as.character(SC[i,1])
	
	if(  all( calls.cor[[i]][,1] == 0 ) & i==nrow(SC) ) {
		calls.plot[[i]]<-qplot(1,1);break}

	if( all( calls.cor[[i]][,1] == 0  & i!=nrow(SC) ) ){ 
		calls.plot[[i]]<-qplot(1,1); next }
	

	calls.plot[[i]]<-subset(dnw, select=c("wait",row.names(
		calls.cor[[i]][which(calls.cor[[i]][,1] != 0),])));rm(dnw)
	calls.plot[[i]]<-calls.plot[[i]][which( rownames(calls.plot[[i]]) %in% 
		colnames( calls.plot[[i]] ) ),]
	calls.plot[[i]]<-ggcorrplot(calls.plot[[i]], hc.order = T,
		 type = "lower",   lab = T, title=as.character(SC[i,1]) )
}

 calls.plot[[84]]

setwd("G:/Math/311/p")
 






# FEMS - Community Events

DCGI<-calls
DCGI<-DCGI[which(DCGI$SERVICECODEDESCRIPTION== 
	"FEMS - Community Events"  ),]
nrow(DCGI)
head(DCGI)

#DCGI<-nrow(DCGI[complete.cases(DCGI), ])


b<-which(names(DCGI) == "wait"):which(names(DCGI) == "late")
dnw<-which(names(DCGI) == "precip_daily"):ncol(DCGI)

	DCGI<-  ( (DCGI[,c(	b,dnw	)])	)
	head(DCGI[, -which(names(DCGI) %in%  names(DCGI)[c(13,15)] ) ])
	b<-which(names(DCGI) == "pBlack"):which(names(DCGI) == "pTwo.more")
	DCGI<-DCGI[which(DCGI$wait < 115 &  DCGI$wait > 0.04 ),]


	DCGI<- DCGI[,-c(2:3,b,6:8)]
	DCGI$pWhite<-as.numeric(DCGI$pWhite)
 	DCGI<- DCGI[, -which(names(DCGI) == "new.residents") ]
 	DCGI<- DCGI[, -which(names(DCGI) == "old.residents") ]
	DCGI$Occupied<-DCGI$Occupied/DCGI$Households
 	DCGI<- DCGI[, -which(names(DCGI) %in%  names(DCGI)[c(13,15)] ) ]

 
	summary(lm( wait~. , data=DCGI ))








