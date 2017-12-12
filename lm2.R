calls.lms<-list()
calls.cor<-list()
calls.plot<-list()
correlations<-data.frame(rep(1,30))



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
	callss$wait	<- sqrt(callss$wait	)

	head(data.matrix(callss[,-c(2:3,b,6:8)]))
	calls.cor[[i]]<-cor(	data.matrix(callss[,-c(2:3,b,6:8)])	)

	b<-which(as.numeric(apply( calls.cor[[i]] , 2 , function(x) sum(
		is.na(x)))) > 1)
 	if ( length(b) != 0 ) {calls.cor[[i]]<-calls.cor[[i]][-b,-b]}

	dnw<-calls.cor[[i]]
	calls.cor[[i]]<-as.data.frame(calls.cor[[i]][,1])
	calls.cor[[i]][-1,1][ abs( calls.cor[[i]][-1,1] ) <.35 ] <-as.integer(0)
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

for(i in 1:length( names(correlations) )  ){
	ggsave( paste0("sqrt",names(correlations)[i] ,".pdf") ,
		plot= calls.plot[[ which(SC == names(correlations)[i] )]]) }

 
