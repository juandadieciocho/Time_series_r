require(datasets)
data("USAccDeaths")
series=USAccDeaths
par(mfrow=c(1,1))
plot(series)
muestra=series[1:12]
which(muestra==max(muestra))


data_1=diff(series,differences = 12)
data_2=diff(diff(series),differences = 12)
acf_1=(acf(data_2))
     
c1=0+qnorm(0.05/2)*(1/sqrt(72))
c2=0-qnorm(0.05/2)*(1/sqrt(72))

which(acf_1$acf>c1 | acf_1$acf>c2)
