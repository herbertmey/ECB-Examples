


 	library(urca)	
  	library(pracma)  	
 	library(MASS)
	library(lmtest)
	library(car)
	library(caret)
	library(relaimpo)
	library(gvlma)
	library(lubridate)
	

	VAR=read.csv("C:\\Users\\YZ93ZT\\Desktop\\UK_Test\\MacroEco.csv",sep=";", quote="", header= T, stringsAsFactors=F)
	Date=VAR[,1]	#DD/MM/YYYY
	Quarters <- quarter(Date,with_year = FALSE)
	VAR<-cbind(VAR,Quarters)
	VAR$Q1<-ifelse(VAR$Quarters==1,1,0)
	VAR$Q2<-ifelse(VAR$Quarters==2,1,0)
	VAR$Q3<-ifelse(VAR$Quarters==3,1,0)
	VAR$Q4<-ifelse(VAR$Quarters==4,1,0)	
	
	R0R4_3M=ts(VAR[,2])		#R0R4_3M
	R0R4_6M=ts(VAR[,3])		#R0R4_6M
	R0R4_12M=ts(VAR[,4])	#R0R4_12M
	X1=ts(VAR[,5])		#GDP_Q
	X2=ts(VAR[,6])		#GDP_Y
	X3=ts(VAR[,7])		#Unemployment
	X4=ts(VAR[,8])		#Inflation
	X5=ts(VAR[,9])		#Home_prices_Y


 	par(mfrow=c(3,3))
	plot(R0R4_3M, type="l")
	plot(X1,type="l")
	plot(X2,type="l")
	plot(X3,type="l")
	plot(X4,type="l")
	plot(X5,type="l")


	Seas1=lm(R0R4_3M~VAR$Q1)
	summary(Seas1)
	
	Seas2=lm(R0R4_3M~VAR$Q2)
	summary(Seas2)
	
	Seas3=lm(R0R4_3M~VAR$Q3)
	summary(Seas3)
	
	Seas4=lm(R0R4_3M~VAR$Q4)
	summary(Seas4)
	

	stationarity=function(base)
		{
			n=length(base[1,])
			res=matrix(NA,n*3,6)
			i=1
			j=1
			while(i<=((n*3)-2))
			{
  				indice=base[,j]
   				indice=indice[!is.na( indice)]
  				df=ur.df(indice,type="trend",lags=1)
  				#tau
  				 res[i,1]=df@teststat[1]
  				 res[i,2]=df@cval[1,2]
  				#Phi3
  				 res[i+1,1]=df@teststat[3]
  				 res[i+1,2]=df@cval[3,2]
  				#B0
  				 res[i+2,1]=df@testreg$coefficients[3,3]
  				 res[i+2,2]=df@testreg$coefficients[3,4]
  				 df=ur.df(indice,type="drift",lags=1)
  				#tau
  				 res[i,3]=df@teststat[1]
 				 res[i,4]=df@cval[1,2]
 				#Phi2
 				 res[i+1,3]=df@teststat[2]
 				 res[i+1,4]=df@cval[2,2]
				#C0
 				 res[i+2,3]=df@testreg$coefficients[1,3]
 				 res[i+2,4]=df@testreg$coefficients[1,4]
 				 df=ur.df(indice,type="none",lags=1)
  				#tau
 				 res[i,5]=df@teststat[1]
				 res[i,6]=df@cval[1,2]
 				 i=i+3
				 j=j+1
			}
			return(res)
		}

	base=cbind(R0R4_3M,R0R4_6M,R0R4_12M,X1,X2,X3,X4,X5,diff(X3) )

	res=stationarity(base)
	res
	write.table(res, "C:\\Users\\YZ93ZT\\Desktop\\UK_Test\\Stationarity.csv", sep=';',dec=',')



L=length(R0R4_3M)
A=5
B=4

step(lm(R0R4_12M[A:L]~X2[A:L]+ X5[A:L] 
        +X2[B:(L-1)]+ X5[B:(L-1)] + diff(X3)[B:(L - 1)]  +  R0R4_12M[B:(L - 1)]  
),direction="both")


modelOK=lm(R0R4_12M[A:L] ~ diff(X3)[B:(L - 1)]   + X2[B:(L - 1)] + R0R4_12M[B:(L - 1)] )
summary(modelOK)


  importance<-varImp(modelOK, scale = FALSE)

  importance[1,1]/sum(importance[,1]) * 100

  importance[2,1]/sum(importance[,1]) * 100

  importance[3,1]/sum(importance[,1]) * 100
  

		residR=resid(modelOK)
		plot(residR,type="l")
		stat_residus = stationarity(cbind(residR,residR))
		write.table(stat_residus, "C:\\Users\\YZ93ZT\\Desktop\\UK_Test\\Stationarity_Residus.csv", sep=';',dec=',')

		par(mfrow=c(1,2))
		acf(residR)
		pacf(residR)

		shapiro.test(residR) 
	
		bptest(modelOK)

		vif(modelOK)
		
