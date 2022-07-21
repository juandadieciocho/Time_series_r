require("forecast")
rm(list=ls(all=TRUE))
set.seed(500) # Beginning of Heptarchy: Kent, Essex, Sussex,
# Wessex, East Anglia, Mercia, and Northumbria.
data = arima.sim( list(order = c(1,0,1), ar =.7, ma=.2), n = 1000000)
par(mfcol = c(3,1 ))
plot(data, main="ARMA(1,1) Time Series: phi1=.7, theta1=.2", xlim=c(0,400)) #first terms
acf(data, main="Autocorrelation of ARMA(1,1), phi1=.7, theta1=.2")
acf(data, type="partial", main="Partial Autocorrelation of ARMA(1,1), phi1=.7, theta1=.2")

modelo=arima(data,order=c(1,0,1),include.mean=FALSE)
modelo$coef

#(1-0.7B)x(t)=(1+0.2B)z(t)
# Ley geometrica ar^(n-k)=sum(k=1;inf)a/(1-r) si |r|< 1  prueba ley geometrica
#(1-0.7B)x(t)*(1/(1+0.2B))=z(t)
# 1/(1+0.2B)=1-0.2B+0.04B^2+0.08B^3... #segundo termino
# pi(b)=(1-0.7B)*(1-0.2B+0.04B^2-0.08B^3...)
# pi(b)=(1 ??? 0.9???? + 0.18????^2 ??? 0.036B^3+...)
#Is there a way to get a closed form expression for these ???? weights rather than the infinite series?Then we could express the autocorrelation function as a formula. Lets work a little more generally and develop a formula for a generic ARMA(1,1) process, then come back and apply it here.
#
#pi(B)= 1 ??? (???? + ????)B + (???? ??? ???? + ????)B^2???(???? ??? ???? ^2 + ????^3)B^ 3+(???????? 3 + ????)B^4
#pi(B)= 1 ??? (???? + ????)(???? ??? ???? B^2 + ????^2*B^3 ??? ????3B^4) +...
# phi(B_k)=(???1)^k(???? + ????) ????^(k???1)


#AR term phi
#MA term theta
#mixed_ARMA_autorregresive
B_k.ar=function(phi,theta,k){
  B_k=(-1)^(k)*(phi + theta)*theta^(k-1)
  print(B_k)
  }
#mixed_ARMA_movingAverage
B_k.ma=function(phi,theta,k){
  B_k=(phi + theta)*phi^(k-1)
  print(B_k)
}

for(i in 1:4){
  B_k.ar(phi=0.7,theta=0.2,k=i)
}
for(i in 1:4){
  B_k.ma(phi=0.7,theta=0.2,k=i)
}
#ARMA moving Average phi term estimation
phi_est=function(aut_cor,lag){
  phi=aut_cor[lag]/aut_cor[lag-1]
  print(phi)
}

#ejercicio
#(1-0.4B+0.16B^2-0.64B^3)*(1-0.6B)
# 1-0.4B+0.16B^2-0.64B^3-0.6B+0.24B^2+0.096B^3
# 1-0.16B+0.08B^2-0.032B^3

################################################################
plot(discoveries,
     main = "Time Series of Number of Major Scientific Discoveries in a Year")
stripchart(discoveries, method = "stack", offset=.5, at=.15,pch=19,
           main="Number of Discoveries Dotplot",
           xlab="Number of Major Scientific Discoveries in a Year",
           ylab="Frequency")

par(mfcol = c(2,1 ))
acf(discoveries, main="ACF of Number of Major Scientific Discoveries in a Year")
acf(discoveries, type="partial", main="PACF of Number of Major Scientific Discoveries
in a Year")
arima( discoveries, order=c(1,0,1) ) 
library(forecast)
auto.arima(discoveries, d=0, approximation=FALSE)
auto.arima(discoveries, d=0, ic="aic", approximation=TRUE)


#########################PARCIAL
#(1-0.5B)x(t)=(1+0.2)z(t)
#(1-0.5B)*(1-0.2B+0.04B^2-0.008B^3)
#(1-0.2B+0.04B^2-0.008B^3-0.5B+0.1B^2-0.02B^3+0.004B^4)
# 1-0.7B+0.14B^2-0.028B^3+0.004B^4


#(1+0.2B)*(1+0.5B+0.025B^2+0.125B^3)
#(1+0.5B+0.025B^2+0.125B^3+ 0.2B+0.1B^2+0.025B^3+0.025B^4)
# 1+0.7B+0.125B^2+0.15

for(i in 1:4){
  B_k.ma(phi=0.2,theta=0.5,k=i)
}
