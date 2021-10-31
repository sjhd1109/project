setwd("C:/Users/seung/OneDrive - 서강대학교/data")
MHdata<-read.csv("MH.csv")
str(MHdata)
summary(MHdata)
library(tseries)
library(forecast)
MH_rate<-MHdata$癤풹pproval.rating
MH_rate<-ts(MH_rate,frequency = 12,start = c(2003,3))
MH_rate

#Decomposing time series
##Decomposing non-Seasonal Data
##시계열의 개별 구성 요소인 추세 요소와 random 요소를 분리, 이를 위해 단순한 이동평균을 사용하여 시계열 데이터를 평활화함. n 차수의 이동 평균을 계산함.
## n=12일 때 그림으로 보면, 지지율은 2003년부터 2007년 초중반까지 하락하다가 그 이후 시점으로 증가한다. 
library(TTR)
x11();par(mfrow =c(2,2))
plot.ts(MH_rate)
SMA3_MH<-SMA(MH_rate,n=3);plot.ts(SMA3_MH)
SMA6_MH<-SMA(MH_rate,n=6);plot.ts(SMA6_MH)
SMA12_MH<-SMA(MH_rate,n=12);plot.ts(SMA12_MH)

##Decomposition of time series
decomp_MH<-decompose(MH_rate)
x11();plot(decomp_MH)

##seasonally adjusting
##seasonally 삭제
SeasonAdj_MH<-MH_rate - decomp_MH$seasonal;x11();par(mfrow =c(2,1));plot.ts(MH_rate);plot.ts(SeasonAdj_MH)

#autocorrelations
MH_ACF<-Acf(MH_rate);MH_ACF[["acf"]]
MH_PACF<-pacf(MH_rate,lag.max = 20)


#단위근 검정
adf.test(MH_rate)
nsdiffs(MH_rate)
install.packages("fUnitRoots")
library(fUnitRoots)

adfTest(MH_rate,lags=0,type = c("c"))
adfTest(MH_rate,lags=0,type = c("nc"))
adfTest(MH_rate,lags=1,type = c("ct"))
adfTest(MH_rate,lags=0,type = c("ct"))

 