require(datasets)
data("BJsales")
serie=BJsales
plot(BJsales)
plot(diff(serie))#GRAFICA UNA DIFERECIA
plot(diff(serie,differences=2))
abline(h=mean(diff(serie,differences=2)),col="blue")
serie_dif=diff(serie,differences=2)
pacf_1=(pacf(serie_dif))
acf_1=(acf(serie_dif))
c1=0+qnorm(0.05/2)*(1/sqrt(150))
c2=0-qnorm(0.05/2)*(1/sqrt(150))


which(pacf_1$acf<c1)
which(acf_1$acf<c1)
which(acf_1$acf>c2)

prueba=arima.sim(n=100,list(ma=c(.2,.4)))
prueba2=arima.sim(n=100,list(ma=.4))
acf(prueba)
acf(prueba2)
#se toma el ultimo lag significativo
