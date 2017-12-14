calls.lms<-list()
calls.cor<-list()
calls.plot<-list()
correlations<-data.frame(rep(1,30))

dat<-as.data.frame(dat)

dats<-dat[c(1:10, (nrow(dat)-10):nrow(dat) ),]
write.csv(dats,"datss.csv")


which(names(dat) == "wait")

dats<-dat[,34:ncol(dat)]
as.data.frame(names(dats))

nrow(dats[,c(1,11:13,17,24,31,32,40,)])
dats<-dats[,c(1,11:13,17,24,31,32,40,50)]
nrow(dats[complete.cases(dats),])
dats<-dats[complete.cases(dats),]

round(cor(dats ),3)

par(mfrow=c(2,3))
plot(dats$rain_interval,dats$wait)
plot(dats$Income,dats$wait)
plot(dats$Population,dats$wait)
plot(dats$pWhite,dats$wait)
plot(dats$pPoverty,dats$wait)
plot(dats$ADDDATE_hour,dats$wait)

##### condition

dats<-as.data.frame(sort((table(dat$SERVICECODEDESCRIPTION))))

 

n=70
for(i in n:113){ 

	callss<-calls[which(calls$SERVICECODEDESCRIPTION== SC[i,1]),]
	callss<-callss[complete.cases(callss), ]

	head(callss)
	b<-which(names(callss) == "wait"):which(names(callss) == "late")
	dnw<-which(names(callss) == "precip_daily"):ncol(callss)

	callss<-(callss[,c(	b,dnw	)])
	head(callss)
	b<-which(names(callss) == "pBlack"):which(names(callss) == "pTwo.more")
	callss<-callss[which(callss$wait < 15  &  callss$wait > 0 ),]
	callss$wait	<- log(callss$wait	)

	head(data.matrix(callss[,-c(2:3,b,6:8)]))
	calls.cor[[i]]<-cor(	data.matrix(callss[,-c(2:3,b,6:8)])	)

	b<-which(as.numeric(apply( calls.cor[[i]] , 2 , function(x) sum(
		is.na(x)))) > 1)
 	if ( length(b) != 0 ) {calls.cor[[i]]<-calls.cor[[i]][-b,-b]}

	dnw<-calls.cor[[i]]
	calls.cor[[i]]<-as.data.frame(calls.cor[[i]][,1])
	calls.cor[[i]][-1,1][ abs( calls.cor[[i]][-1,1] ) <.2 ] <-as.integer(0)
	calls.cor[[i]]$Cor<-ifelse( calls.cor[[i]][,1] != 0 , "*" , "")
	calls.cor[[i]]<-calls.cor[[i]][-1,]
 	calls.cor[[i]][,1]<-round(calls.cor[[i]][,1],4)
	names( calls.cor[[i]] )[1]<-as.character(SC[i,1])
	
	if ( length(b) != 0 ) { calls.cor[[i]]<- rbind(calls.cor[[i]][1:(b-1),
		], c(NA,"") ,  calls.cor[[i]][b:nrow(calls.cor[[i]]),])
		rownames(calls.cor[[i]])[b]<-as.character(
		rownames( calls.cor[[i-1]] )[(b-1)] ) }
	
	
	correlations[, i+1-n   ]<-calls.cor[[i]][,1]
	names(correlations)[ i+1-n   ]<- as.character(SC[i,1])

	if(  all(  calls.cor[[i]][,1][!is.na(calls.cor[[i]][,1])]  == 0 ) & 
		i==nrow(SC) ) {	calls.plot[[i]]<-qplot(1,1);break}

	if( all(calls.cor[[i]][,1][!is.na(calls.cor[[i]][,1])] == 0  &
		 i!=nrow(SC) ) ){ calls.plot[[i]]<-qplot(1,1); next }
	

	calls.plot[[i]]<-subset(dnw, select=c("wait",row.names(
		calls.cor[[i]][which(calls.cor[[i]][,1] != 0),])));rm(dnw)
	calls.plot[[i]]<-calls.plot[[i]][which( rownames(calls.plot[[i]]) %in% 
		colnames( calls.plot[[i]] ) ),]
	calls.plot[[i]]<-ggcorrplot(calls.plot[[i]], hc.order = T,
		 type = "lower",   lab = T, title=as.character(SC[i,1]) )
}

correlations<-correlations[,-which( sapply(correlations, class) ==
	 "character")]

correlations<-correlations[,which(colSums(abs(correlations)) != 0)]

t(as.matrix(correlations))

namez<-names(correlations)
names(correlations)<-LETTERS[1:length(names(correlations))]

sink("corre.txt")
paste0(LETTERS[1:length(names(correlations))],": ",namez)
correlations
sink()

which(sapply(correlations,  function(x) length(which(x>0) )) >5)

setwd("G:/math/651project")
for(i in 1:length( names(correlations) )  ){
	ggsave( paste0("sqrt",names(correlations)[i] ,".pdf") ,
		plot= calls.plot[[ which(SC == names(correlations)[i] )]]) }
SC
i=74

callz<-(calls[ which( calls$SERVICECODEDESCRIPTION ==
	 as.character( SC[i,1] ) ), ] )
head(callz);nrow(callz)
 

 mean(callz$wait)
table(round(callz$wait,0))

as.data.frame(names(callz))
callzz<-callz[,c(20,25,26,30:ncol(callz))] 
callzz$pWhite<-as.numeric(callzz$pWhite)
callzz<-callzz[,sapply(callzz, is.numeric)]
nrow(callzz)
callzz<-callzz[complete.cases(callzz),]
head(callzz)

callzz$Vacant

summary( lm( wait~. ,data=callzz) )

table(callz$wait)