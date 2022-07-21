require(datasets)
require(forecast)
data(sunspots)
serie=sunspots

forecast(HoltWinters(serie),20)
predict(m1,1)


#Aug 1985-38.65281
tail(serie)

seriemod[2808]
