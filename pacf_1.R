rm( list=ls( all = TRUE ) )
par(mfrow=c(3,1))
phi.1 = .6; phi.2 = .2; data.ts = arima.sim(n = 500, list(ar = c(phi.1, phi.2)))
plot(data.ts, main=
       paste("Autoregressive Process with phi1=",phi.1," phi2=",phi.2) )
acf(data.ts, main="Autocorrelation Function")
acf(data.ts, type="partial", main="Partial Autocorrelation Function")

#You should run the code several times. You will observe that, while the actual time series itself
#changes from simulation to simulation, the ACF and PACF are relatively constant. Now run the
#code again, this time with the following two lines substituted into the obvious place:

phi.1 = .9; phi.2 = -.6; phi.3 = .3;
data.ts = arima.sim(n = 500, list(ar = c(phi.1, phi.2, phi.3)))
plot(data.ts, main= paste("Autoregressive Process with phi1=",
                          phi.1," phi2=",phi.2," phi3=",phi.3) )

######BEVERAGE
beveridge = read.table("beveridge.txt", header=TRUE)
beveridge.ts = ts(beveridge[,2], start=1500)
plot( beveridge.ts, ylab="price", main="Beveridge Wheat Price Data")
beveridge.MA = filter(beveridge.ts, rep(1/31, 31), sides = 2) #escalamiento sobre el número de datos
lines(beveridge.MA, col="red")
par(mfrow=c(3,1))
Y = beveridge.ts/beveridge.MA
plot( Y, ylab="scaled price", main="Transformed Beveridge Wheat Price Data")
acf(na.omit(Y),
    main="Autocorrelation Function of Transformed Beveridge Data")
acf(na.omit(Y), type="partial",
    main="Partial Autocorrelation Function of Transformed Beveridge Data")
#estimacion coeficientes 
ar(na.omit(Y), order.max = 5)
#conclusion An autoregressive process of order p, an AR(p), has a PACF that cuts off after p lags

#install.packages("isdals",dependenncies=TRUE)
library(isdals)
data(bodyfat)
#It probably won't shock you that these variables are related. We can see this #with a pairs plot.
attach(bodyfat)
pairs( cbind( Fat, Triceps, Thigh, Midarm) )
cor( cbind( Fat, Triceps, Thigh, Midarm) )
#Since Triceps and Thigh are also clearly related r= 0.9238425, we wonder if we can measure the correlation of Fat and Triceps, after controlling for or "partialling out" Thigh. We first try to account for the effect of Thigh on both Fat and Triceps by regressing them on Thigh. After we remove the contribution of Thigh, we then find the correlation of Fat and Triceps. This is pretty easy to do; just use the lm() command we have previously discussed.
Fat.hat = predict(lm(Fat~Thigh))
Triceps.hat = predict( lm(Triceps~Thigh) )
cor( (Fat- Fat.hat), (Triceps- Triceps.hat) ) #returns 0.1749822

#So, a great deal of the correlation between Fat and Triceps is accounted for 
#by controlling for Thigh circumference. What happens when we control for both Thigh and Midarm? We do the calculation ourselves, and confirm by using the
#pcor() routine. 
Fat.hat = predict(lm(Fat~Thigh+Midarm))
Triceps.hat = predict( lm(Triceps~Thigh+Midarm) )
cor( (Fat- Fat.hat), (Triceps- Triceps.hat) ) #returns 0.33815
pcor( cbind( Fat, Triceps, Thigh, Midarm) )

#formula de autocorrelaciones
#lag 1
# pacf(1)=cov(x(t),x(t-1))/(v(x(t))*v(x(t-1)))
# pacf(k)=cov(x(t),x(t-k)|x(t-1),x(t-2),x(t-(k-1)))/v(x(t),v(x(t-k))|x(t-1),x(t-2),x(t-(k-1)))*v(x(t),v(x(t-k))|x(t-1),x(t-2),x(t-(k-1)))

# x_est(t+h)= beta1*x(t+h-1) + b2*x(t+h-2)+..+b(h-1)*x(t+1)
# x_est(t)= beta1*x(t+1) + b2*x(t+2)+..+b(h-1)*x(t+h-1)

# phi(h,h)=cov(x(t+h)-x_est(t+h),x(t)-x_est(t))
# pacf(h)=cov(x(t+h)-x_est(t+h),x(t)-x_est(t))

#We can calculate (estimate, really) these quantities from a given time series and have another plot to use in our quest to understand the stochastic process that generated the data at hand. We will plot the Partial Auto Correlation Function (PACF). The call in R is simple, we just give anargument to the acf() routine.
acf( ts, type="partial")
#It produces the following plot for some data I created with arima.sim(). We have already seen that if we know that we have an autoregressive process and are looking to determine the order of the process, we produce a PACF and observe where it "cuts off": We would conclude for the time series exhibited here that we have a second order model.
phi.1 = .6;
phi.2 = -.6;
data.ts = arima.sim(n = 1000, list(ar = c(phi.1, phi.2)))
acf(data.ts, type="partial",
    main=paste("PACF of Time Series Data, phi1=",phi.1,", phi.2=",phi.2) )





#########################################################################
phi.1 = .6;
phi.2 = -.6;
set.seed(1)
data_1 = arima.sim(n = 20, list(ar = c(phi.1, phi.2)))



#datos_12=matrix(rep(0,20),ncol=20,nrow=20)


#esquema primer lag
#for(i in 1:18){
#  k=1
#  datos_12[i]=data_1[i+k]
#  }
#esquema k lag

#datos_12[,1]=data_1

datos_12=matrix(rep(0,20),ncol=19,nrow=20)
for(k in 1:19){
  for(i in 1:20){
   datos_12[i,k]=data_1[i+k]
  }}
datos_12[is.na(datos_12)]=0

test_datax=cbind(data_1,datos_12)


acf_2=function(data,lag_data){
  n=length(data)
  c0=(1/n)*sum((data-0)^2)
  c1=(1/n)*sum((lag_data-0)*(data-0))
  cor_1=c1/c0
  print(cor_1)
}

aut_cor=rep(0,20)
for(i in 1:20){
  aut_cor[i]=acf_2(data_1,test_datax[,i])
}


acovf_2=function(data,lag_data){
  n=length(data)
  c1=(1/n)*sum((lag_data-0)*(data-0))
  print(c1)
}

aut_cov=rep(0,20)
for(i in 1:20){
  aut_cov[i]=acovf_2(data_1,test_datax[,i])
}

aut_cor_1=aut_cor[2:19]

m1=lm(test_datax[,1]~test_datax[,2])
m1$residuals
m2=lm(test_datax[,3]~test_datax[,2])
m2$residuals
cor(data_1,m2$residuals)/(var(m1$residuals)*var(m2$residuals))

install.packages("ppcor")
require(ppcor)

getAnywhere(pcor)

m1=predict(lm(datos_12[,3]~datos_12[,1:2]))
m2=predict(lm(test_datax[,1]~datos_12[,1:2]))
cor(data_1-m2,m1-datos_12[,3])
cor(data_1-m2,m1-datos_12[,3])








#a_cov=(1/20)*(test_datax %*% t(test_datax))


################Ejemplo yule walker
v_a=aut_cov[1]
aut_cov_1=aut_cov[2:20]
a_cov2=matrix(rep(0,20),ncol=20,nrow=20)
for(i in 1:20){
  a_cov2[i,1:i]=aut_cov[i:1]  
  }
for(i in 1:19){
  a_cov2[i,(i+1):20]=aut_cov_1[1:(20-i)]
}





#m1=list()
#pacf_2=0
#for(i in 1:18){
#  m1[[i]]=solve(a_cor2[1:i,1:i])%*%(aut_cor_1[1:i])
#  pacf_2[i]=m1[[i]][i]
#}

#solve(a_cov2[1:1,1:1])%*%(aut_cov_1[1:1])
#solve(a_cor2[1:1,1:1])%*%(aut_cor[1:1])
#solve(a_cov2[1:2,1:2])%*%(aut_cov_1[1:2])
#solve(a_cor2[1:3,1:3])%*%(aut_cor_1[1:3])
#solve(a_cor2[1:18,1:18])%*%(aut_cor_1[1:18])
#solve(a_cor2[1:19,1:19])%*%(aut_cor_1[1:19])



yule_w_1_cov=function(matrix){
  acovf_2=function(data,lag_data){
    n=length(data)
    c1=(1/n)*sum((lag_data-0)*(data-0))
    }
  n=dim(matrix)[1]
  aut_cov=rep(0,n)
  for(i in 1:n){
    aut_cov[i]=acovf_2(matrix[,1],matrix[,i])  
  }
  sigma_est=0
  matrices_pacf=list()
  matrices_pacf1=list()
  v_a=aut_cov[1]
  solution_vector=list()
  c_a=1
  pacf_1=rep(0,(n-2))
  aut_cov_1=aut_cov[2:n]
  n2=length(aut_cov_1)
  res_cov1=list()
  a_cov2=matrix(rep(0,n),ncol=n,nrow=n)
  for(i in 1:n){
    a_cov2[i,1:i]=aut_cov[i:1]  
  }
  for(i in 1:(n-1)){
    a_cov2[i,(i+1):n]=aut_cov_1[1:(n-i)]
  }
  for(i in 1:(n-1)){
    matrices_pacf[[i]]=a_cov2[1:i,1:i]
  }
  for(i in 1:n2){
    res_cov1[[i]]=aut_cov_1[1:i]
  }
  for(i in 1:(n-1)){
    solution_vector[[i]]=solve(matrices_pacf[[i]])%*%(res_cov1[[i]])
  }
 for(i in 2:(n-2)){
  pacf_1[i]=solution_vector[[i]][i]}
  pacf_1[1]=aut_cov[2]/v_a
 # sigma_est=v_a*(1-sum(pacf_1*matrices_pacf1))
list(res=pacf_1,matriz=matrices_pacf)}




yule_w_1_cor_all=function(matrix){
  acf_2=function(data,lag_data){
    n=length(data)
    c0=(1/n)*sum((data-0)^2)
    c1=(1/n)*sum((lag_data-0)*(data-0))
    cor_1=c1/c0
    print(cor_1)
  }
  acovf_2=function(data,lag_data){
    n=length(data)
    c1=(1/n)*sum((lag_data-0)*(data-0))
  }
  n=dim(matrix)[1]
  aut_cov=rep(0,n)
  aut_cor=rep(0,n)
  for(i in 1:n){
    aut_cov[i]=acovf_2(matrix[,1],matrix[,i])  
    aut_cor[i]=acf_2(matrix[,1],matrix[,i])
  }
  aut_cor_1=aut_cor[2:n]
  sigma_est=0
 # matrices_pacf=list()
 # matrices_pacf1=list()
  v_a=aut_cov[1]
  #solution_vector=list()
  c_a=1
  pacf_1=rep(0,(n-2))
  aut_cov_1=aut_cov[2:n]
  aut_cor_1=aut_cor_1[2:n]
  phi_est=matrix(rep(0,(n-1)),ncol=1)
  a_cor2=matrix(rep(0,n),ncol=n,nrow=n)
  f_mat=NULL
  for(i in 1:n){
    a_cor2[i,1:i]=aut_cor[i:1]  
  }
  for(i in 1:(n-1)){
    a_cor2[i,(i+1):n]=aut_cor[1:(n-i)]
  }
  f_mat=a_cor2[,]
  phi_est=solve(f_mat,aut_cor_1)
  var_est=v_a*(1-sum(aut_cor*phi_est))
  print(c(phi_est,var_est))
  
}



#serie

x=0
x[1]=0.01
x[2]=0.02
x[3]=0.03
for(i in 4:100){
  x[i]=(1/2)*x[i-1]+(1/9)*x[i-2]-(1/18)*x[i-3]+rnorm(1,0,1)
}
x=ts(x)
plot(x)
abline(h=mean(x))

r=(acf(x),plot=FALSE)$acf
r2=r[2:4]
a_cor2=matrix(rep(0,3),ncol=3,nrow=3)
for(i in 1:3){
  a_cor2[i,1:i]=r2[i:1]  
}
for(i in 1:(3-1)){
  a_cor2[i,(i+1):3]=r2[1:(3-i)]
}
diag(a_cor2)=1
a_cor2
solve(a_cor2,r[2:4])

#serie=arima.sim(n = 63, list(ar = c(0.8897, -0.4858)


?arima.sim


r=c(0.8,0.6,0.2)
n=3
a_cov2=matrix(rep(0,n),ncol=n,nrow=n)
a_cov[2,1]=r[1]
v_1=
for(i in 1:n){
  a_cov2[i,1:i]=1
  a_cov2[i,1:(i-1)]=r[(i-1):1]
}
a_cov2
for(i in 1:(n-1)){
  a_cov2[i,(i+1):n]=r[1:(n-i)]
}
diag(a_cov2)=1
phi_est=solve(a_cov2,r)
c0=5
var_est=c0*(1-sum(r*phi_est))






yule_w_1_cor_all(test_datax)
  # sigma_est=v_a*(1-sum(pacf_1*matrices_pacf1))
  list(res=pacf_1,matriz=matrices_pacf)}























result=yule_w_1_cov(matrix=test_datax) 
p_1=result$res
length(p_1)
length(bien_pacf)
matrices_1=result$matriz
bien_pacf[1:13]-p_1[1:13]



c_i_bartlett=function(acf_2,alpha){
 acf_1=round(acf_2,3)
 ro_1=mean(acf_1)
 n=length(acf_1)
 sd1=sd(acf_1)
 ci1=0-qnorm(1-alpha)*(1/(sqrt(n)))
 ci2=0+qnorm(1-alpha)*(1/(sqrt(n)))
 res=c(ci1,ci2)
}

c_i=c_i_bartlett(acf_1=aut_cor,alpha=0.05)

plot(p_1,type="h", ylim=c(-1,1))
abline(h=0,col="black")
abline(h=c_i,col="blue")


m1=lm(test_datax[,1]~test_datax[,2:3]-1)
m2=lm(test_datax[,3]~test_datax[,2]-1)


p1_alt=function(matrix,lag){
  mdat=lm(matrix[,1]~matrix[,2:lag]-1)
  maut=lm(matrix[,lag]~matrix[,2:(lag-1)]-1)
  diferencia1=(mdat$fitted.values-maut$fitted.values)
  res=-cor(diferencia1,matrix[,1])
  print(res)}

p2_alt=function(matrix,lag){
  mdat=lm(matrix[,1]~matrix[,2:lag]-1)
  maut=lm(matrix[,lag]~matrix[,2:(lag-1)]-1)
  diferencia1=(mdat$residuals-maut$residuals)
  res=-cor(diferencia1,matrix[,1])
  print(res)}
#################################################
lag_mat=datos_11
lag_mat[is.na(lag_mat)]=0
model1=matrix(rep(0,20),ncol=19,nrow=20)
for(i in 1:19){
  model1[,i]=predict(lm(data_1~lag_mat[,i]-1))
}
residual_matrix=matrix(rep(0,20),ncol=19,nrow=20)
for(i in 1:19){
  residual_matrix[,i]=data_1-model1[,i]
}
residual_datf=data.frame(residual_matrix)
m=list()
for(i in 1:19){
  m[[i]]=lm(residual_matrix[,i]~residual_matrix[,1:(i-1)]-1)
}
coeficientes=list()
for(i in 1:19){
 coeficientes[[i]]=matrix(m[[i]]$coefficients[1:i],ncol=1)
}
pacf_alt=0
for(i in 2:19){
  pacf_alt[i]=as.numeric(coeficientes[[i]][(i-1),1])
}

cbind(pacf_alt[1:13],bien_pacf)



m$coefficients[1]
m2$coefficients[2]


bien_pacf
coef=list()
for(i in 1:19){
  coef[[i]]=model2[[i]][i]$coefficients
 }
pacf_alt=0

View(coef)

dim(predictions)





#y=cbind(text_data[,1],text_data[,2])
#x=
#beta=

cor(resid(lm(test_datax[,2]~test_datax[,3])))







model_data=data.frame(test_datax)
names(model_data)=c("data_1","lag1","lag2",
                    "lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10",
                    "lag11","lag12","lag13","lag14","lag15","lag16","lag17","lag18","lag19")

cor(resid(lm(cbind(data_1,lag3)~lag2,model_data)))
cor(resid(lm(cbind(data_1,lag2,lag3)~lag2,model_data)))
bien_pacf[1:3]




m1$coefficients
head(iris)
cor(resid(lm(cbind(Sepal.Length, Sepal.Width) ~., iris)))
m1=lm(cbind(Sepal.Length, Sepal.Width) ~.,iris)
names(m1)
m1$terms
cor(iris[1:3])
######################################################

data(iris)


#https://stats.stackexchange.com/questions/129052/acf-and-pacf-formula

cbind(p2[2:14],bien_pacf[1:13])  
  
p2=0
for(i in 2:20){
  p2[i]=p1_alt(test_datax,i)
}

p3=0
for(i in 2:20){
  p3[i]=p2_alt(test_datax,i)
}



cbind(bien_pacf,p2[2:14])









a1=sum((m1$fitted.values-0)*(m2$fitted.values-0))
a/(20*b)
a1/(20*b)

sum(m1$fitted.values-m2$fitted.values)/sum(m2$fitted.values)

bien_pacf[1]







aut_cov



matrices_1=list()
matrices_1=result$matriz
names(result)  
matrices_1[[]]



for(i in 2:(19-1)){
  print(i)
}

det(a_cor2[1:2,1:2])







solve(a_cor2)%*%(-aut_cor)










aut_cov[(i+1):20]
aut_cov[1:(20-1)]
aut_cov[1:(20-1)]



a_cov2=aut_cov 




a_cor=(1/aut_cov[1])*a_cov

m1=lm(-aut_cor~a_cor-1)
m1$coefficients

lm(a_cor~-1)

b/v

av=matrix(a_cov[1,],ncol=1)
cbind(av,aut_cov)

datos_12[20,1]=data_1[1]
largo=0
for(k in 2:19){
  largo[k]=20-k+1
  datos_12[largo[k]:20,k]=data_1[1:k]
}

tabla=cbind(data_1,datos_12)

largo2_acf=0
acf_1=0
data_x=matrix(rep(0,20),nrow=20,ncol=19)
for(i in 1:19){
  largo2_acf[i]=20-i
  data_x[,i]=matrix(c(datos_12[1:largo2_acf[i],i],rep(0,20-largo2_acf[i])))
  acf_1[i]=cor(data_1,data_x[,i])
}

cor(tabla[,1],tabla[,2])
a=(acf(data_1))
b=(pacf(data_1))
bien_acf=a$acf
bien_pacf=b$acf

tabla_mod=matrix(rep(0,20),ncol=20,nrow=20)
for(i in 1:20){
  tabla_mod[,i]=tabla[,i]-mean(data_1)
}

pacf_1=function(data,lag){
  m1=lm(data[,1]~data[,1:(lag-1)]-1)
  m2=lm(data[,lag]~data[,1:(lag-1)]-1)
  res_1=cov(m1$residuals,m2$residuals)/(sd(m1$residuals)*sd(m2$residuals))
  res_2=cov(m1$fitted.values,m2$fitted.values)/(sd(m1$fitted.values)*sd(m2$fitted.values))
  list(coef_m_data=m1$coefficients,coef_m_auto=m2$coefficients,
  result=c(res_1,res_2))
}

x_1=lag(data_1,1)
x_2=lag(data_1,2)

cor(data_1,x_1)

x_1=test_datax[,2]
x_1[20]=data_1[1]

cor(data_1,x_1)
cor(data_1,test_datax[,2])

###########################################ACF

acf_1=function(data,lag_data,lag){
  n=length(data)
  n2=sum(lag_data!=0)
  miu1=0
  #miu2=mean(lag_data)
  den1=(n)^(-1)
  den2=(n2)^(-1)
  cv=den2*sum((data-miu1)*(lag_data-miu1))
  vdata=den1*sum((data-miu1)^2)
  vlagd=den2*sum((lag_data-miu1)^2)
  acf_11=cv*(sqrt(vlagd)*sqrt(vdata))^(-1)
  print(c("acf is:",acf_11))
}
min(20-2,20)

miu1=mean(data_1)




acf_a=rep(0,20)
for(i in 1:20){
  acf_a[i]=acf_2(data_1,test_datax[,i])
}
cbind(acf_a,bien_acf)

acf_3=function(data,lag_data){
  n=length(data)
  m1=mean(data)
  m2=mean(lag_data)
  c0=(1/n)*sum((data-m1)^2)
  c1=(1/n)*sum((lag_data-m2)*(data-m1))
  cov_1=c1/c0
  print(cov_1)
}

acf_b=rep(0,20)
for(i in 1:20){
  acf_b[i]=acf_3(data_1,test_datax[,i])
}

(acf_a[1:14]-bien_acf[1:14])/14
(acf_b[1:14]-bien_acf[1:14])/14

##################################################PACF

for(k in 1:20){
for(i in 1:20){
  print(acf_2(test_datax[,i],test_datax[,i+k]))
}





bien_acf[2]


x_1=c(0,test_datax[1:19,2])



a=test_datax[-20,1:2]
length(l1)


acf_2(a[,1],a[,2])

c0=(1/20)*sum((data_1-0)^2)
c1=(1/20)*sum((test_datax[,2]-0)*(data_1-0))
cov_1=c1/c0

c0=(1/20)*sum((data_1-miu1)^2)
c1=(1/20)*sum((test_datax[,2]-miu1)*(data_1-miu1))
cov_1=c1/c0



bien_acf[2]


cbind(test_datax[,1],test_datax[,2])
acf_1(data_1,test_datax[,2],1)
cor(data_1[1:19],test_datax[1:19,2])

mean(data_1)

bien_acf[2]

cov(data_1,test_datax[,2])/var(data_1)
?cor

#var(x_1)/var(data_1)



cov(data_1,test_datax[,2])
var(data_1)
var(test_datax[,2])
acf_1(data_1,test_datax[,2],1)
acf_1(data_1,x_1,1)

bien_acf

pacf_1(datos_22,4)

#################################################################################





pos11=0
intervalo=list()
for(i in 1:20){
 pos11[i]=round(20/i,0)
 intervalo[[i]]=i*(1:pos11[i])
}


intervalo_cor=list()

for(i in 1:20){
  intervalo_cor[[i]]=intervalo[[i]][which(intervalo[[i]]<=20)]
}


intervalo_cor[[2]]

datos_22=matrix(rep(0,20),ncol=20,nrow=20)


datos_22[intervalo_cor[[2]],2]=data_1[intervalo_cor[[2]]]  

for(i in 1:20){
  datos_22[intervalo_cor[[i]],i]=data_1[intervalo_cor[[i]]] 
}

cor(datos_22[,1],datos_22[,2])

datos_22[intervalo_cor[[2]],2]=data_1[intervalo_cor[[2]]]


 
      







20/3  
3*(1:7)
2*(1:10)

/10
(20-10)/2 
 
(2*19)

datos_2






test_datax=matrix(c(data_1,data_x),ncol=20)

pacf_1(test_datax,2)
bien_pacf[1]
pacf_1(test_datax,3)
bien_pacf[2]

pacf_1(tabla_mod,2)
bien_pacf[1]

pacf_1(tabla_mod,3)
bien_pacf[2]


pacf_1(tabla_mod,3)

cbind(matriz[1:14,1],bien_acf)



lag(data_1,1)






b=aut_corr1
solve(a,b)



m1=lm(tabla_mod[,1]~tabla_mod[,2]+tabla_mod[,3]-1)
md=lm(tabla_mod[,3]~tabla_mod[,2]+tabla_mod[,1]-1)
m1$coefficients
m2$coefficients

pacf_1(tabla_mod[,1],m1$fitted.values,m2$fitted.values)



m2=lm(data_1~datos_12[,1]-1)

cor(data_1,m1$fitted.values)

bien_pacf[1]



m1=lm(tabla[,1]~tabla[,2:20])
m2=lm(tabla[,1])
m1$coefficients

(pacf(data_1))




aut_corr1=matrix(rep(0,19),ncol=1)
for(i in 1:19){
  aut_corr1[i]=cor(data_1,datos_12[,i])
}

aut_corr1

(acf(data_1))



print()
print(datos_12[19:20,2])
    
datos_12[1:(k-1),k]=data_1[1:(k-1)]
datos_12[k:20,k]=data_1[k:20]    
    
    for(k in 1:20){
  datos_12[k:(k-1),k]=datos_12[1:k]
}

#data_1[1:(k-1)]
  datos_12[(k+1):20 ,k]=data_1[(k+1):(20-k)]

data_1[1:2]



data_1[1:k] 
datos_12[i:20,k]
View(cbind(data_1,datos_12[,1]))


for(k in 1:19){
for(i in 1:19){
  datos_12[i,k]=data_1[i+k]
}}

datos_12[is.na(datos_12)]=0

for(k in 1:19){
  datos_22[(k+1):19 ,k]=datos_12[1:(19-k),k]}

#datos_122=matrix(rep(0,20),ncol=19,nrow=20)

#for(k in 1:19){
#    datos_122[(k+1):19,k]=datos_12[1:(19-k),k] 
#    }

#datos_122[20,1]=datos_12[19,1]
#datos_122[20,19]=0

#View(cbind(datos_12[,1],datos_122[,1]))









#for(i in 1:19){
#  datos_12[,i]=rev(datos_12[,i])
#}

#datos_2=rbind(datos_12,rep(0,19))

datos_2=cbind(data_1,datos_12)
#datos_2_122=cbind(data_1,datos_122)





#aut_corr2=matrix(rep(0,20),ncol=1)
#for(i in 1:20){
#  aut_corr2[i]=cor(data_1,datos_2_122[,i])
#}
aut_corr1=matrix(rep(0,19),ncol=1)
for(i in 1:19){
  aut_corr1[i]=cor(data_1,datos_12[,i])
}


aut_cov1=matrix(rep(0,20),ncol=1)
for(i in 1:20){
  aut_cov1[i]=cov(data_1,datos_2[,i])
}

#num=matrix(rep(0,20),ncol=20,nrow=20)
#for(i in 1:19){
#  num[(i+1):20,i]=aut_corr1[(i+1):20]
#  }



View(datos_12[,1])

length(data_1[3:19])
length(datos_12[2:18,1])
length(datos_12[3:17])
length(datos_12[1:16,2])



datos_22=matrix(rep(0,19),ncol=19,nrow=20)
for(k in 1:19){
    datos_22[(k+1):19 ,k]=datos_12[1:(19-k),k]}

datos_22=datos_22[-20,]

length(datos_22[1:19,2])

length(data_1[3:20])
length(datos_22[2:19,1])
length(datos_22[1:19,2])

matrix1=cbind(data_1[3:20],datos_22[2:19,1],datos_22[1:19,2])

lm(matrix1[,1]~matrix1[,2:3]-1)

a=(pacf(data_1))
a$acf[2]

pacf_1=function(data,matrix_1,lag_1){
  rows_1=as.integer(dim(matrix_1)[1])
  rows_2=as.integer(length(data))
  columns_1=as.integer(dim(matrix_1)[2])
  len_lag_data=length(data[lag_1:(rows_2)])
  zeros=columns_1-len_lag_data
  zeros_1=0
  rows_11=0
  datos_12=matrix_1
  v_mayorcero=list()
  largo_vm0=rep(0,19)
  datos_22=matrix(rep(0,rows_1),ncol=columns_1,nrow=rows_1)
  triangular_m=matrix(rep(0,columns_1),ncol=columns_1,nrow=columns_1)
  model=0
  for(k in 1:columns_1){
    datos_22[(k+1):columns_1 ,k]=datos_12[1:(columns_1-k),k]}
  rows_11=as.integer(dim(datos_22)[1])  
  datos_22=datos_22[-rows_11,]
  for(i in 1:columns_1){
    v_mayorcero[[i]]=datos_22[which(datos_22[,i]!=0),i]
    largo_vm0[i]=length(v_mayorcero[[i]])
  }
 triangular_m[,1]=c(rep(0,zeros),data[lag_1:rows_2])#
 triangular_m[,2]=c(rep(0,(columns_1-(largo_vm0[1]))),v_mayorcero[[1]])
 #for(i in 3:lag_1){
  #triangular_m[,i]=c(rep(0,(lag_1-(i-1))),v_mayorcero[[2]][(lag_1-1):largo_vm0[i]])
  #}
  print(triangular_m[,1:2])
}

pacf_1(data=data_1,matrix=datos_12,lag_1=3)

a_a=list()
l=0
for(i in 1:19){
  a_a[[i]]=datos_22[which(datos_22[,i]!=0),i]
  l[i]=length(a_a[[i]])
}

ceros=0
for(i in 1:19){
  ceros[i]=lag_1-l[i]  
}

lag_1=4
value=data_1[lag_1]
pos_1=0
for(i in 1:lag_1){
  pos_1[i]=which(a_a[[i]]==value)
}


View(t(datos_12))

a_a[[1]][1:l[1]]
a_a[[2]][1:l[2]]

a_a[[3]],a_a[[2]],a_a[[1]]

a1=matrix(c(0,a_a[[3]],rep(0,2)),ncol=1)
a2=matrix(c(0,a_a[[2]],rep(0,1)),ncol=1)
a3=matrix(c(0,a_a[[1]]),ncol=1)
data_a=matrix(c(data_1[lag_1:20],rep(0,2)),ncol=1)
matrix1=matrix(c(data_a,a1,a2,a3),ncol=4)
m1=lm(matrix1[,1]~matrix1[,2:4]-1)
m1$coefficients
(pacf(data_1))

a=lag(data_1,1)
datos_121=lag(data_1,k=-19:-1)
datos_121_1=lag(data_1,k=2)
datos_121_2=lag(data_1,k=1)

a=data.frame(cbind(data_1,datos_121_1,datos_121_2))
a[is.na(a)]=0
res=matrix(c(data_1[4:20],rep(0,1)),ncol=1)
b=a[c(-1,-2,-22,-21),]
b$data_1=res
m1=lm(b$data_1~b$datos_121_1+b$datos_121_2-1)
m2=lm(b$datos_121_1~b$datos_121_2-1)
View(b)
m1$coefficients

cov(res,m2$fitted.values)/(sd(m1$fitted.values)*sd(m2$fitted.values))
(pacf(data_1))


lag
length()
a_a[[3]]

?lag


a_a[[2]]
a1
a2

datos_22[19:19,1]
for(i in 3:19){
  t[,i]=c(rep(0,),v_mayorcero[[2]][(lag_1-1):largo_vm0[i]])
}



length(data_1[3:20])
lag_1=3
length(data_1[3:20])
length(data_1[lag_1:19])

19-l2
largo_vm0[1]

lag_1=3
long=length(data_1[lag_1:20])
(zeros=19-long)



t=matrix(rep(0,19),nrow=19,ncol=19)
t[,1]=c(rep(0,zeros),data_1[lag_1:20])

length(a_a[[2]][(lag_1-2):l2])
rep(0,(lag_1-1))


#######################################################################
zeros_1=0
for(i in 1:19){
  zeros_1[i]=19-sum(datos_22[,i]!=0)
}


19-(19-3)


a_1=datos_22[which(datos_22[,1]!=0),1]
a_2=datos_22[which(datos_22[,2]!=0),2]
a_a=list(a_1,a_2)

l1=length(a_a[[1]])
t[,2]=c(rep(0,(19-(l1+0))),a_a[[1]])
l2=length(a_a[[2]])
t[,3]=c(rep(0,(19-l2+1)),a_a[[2]][1:(l2-1)])
View(t)
19-17
View(t)

19-l2+1

c(rep(0,19-l2),datos_22[which(datos_22[,1]!=0)]

rm(t)

lag_1=2
t[,2]=c(rep(0,(lag_1-1)),a_a[[2]][][(lag_1-1):18])







for(i in 1:19){
  if(lag)
}
#1=lag+3

dim(a)

t[,1]=c(0,data_1[3:20])

for(i in)
t



a=1

as.integer(a)





num=matrix(rep(0,20),ncol=20,nrow=20)
for(i in 1:19){
  num[1:i,i]=0
  num[(i+1):19,i]=aut_corr1[2:(19-i+1)]}

num_1=num
for(i in 2:19){
  num_1[1:(i-1),i]=rev(aut_corr1[2:i])
}

num_2=num_1

for(i in 1:19){
  num_2[i,i]=1
}

v_1=rep(0,19)
for(i in 1:19){
  v_1[i]=var(datos_12[,i])
}
var_1=sum(v_1)
iden_1=diag(1,nrow=20)
cov_1=cov(datos_2)
cov_i1=cov(iden_1,cov_1)
cov_i_i=cov(iden_1,iden_1)
cov_i_y=cov(iden_1,data_1)
cov_i_y2=diag(x=c(cov_i_y),nrow=20)
p_c=cov_1-cov_i1*(cov_i_i/cov_i_y2)


cor_1=solve(cor_1[1:2,1:2],aut_corr1[1:2])
cor_1


gamma_cero=sum(diag(var(cov_1)))
cov_1/gamma_cero

solve(cor(datos_12),aut_corr1[1:19])
solve(num_2)




model_1=matrix(rep(0,19),ncol=19,nrow=20) #x(t-h)|x(t-h-1)...

for( i in 1:dim(model)[2]){
  model_1[,i]=predict(lm(datos_12[,i]~datos_12[,1:(i-1)]))
}

model_2=matrix(rep(0,19),ncol=19,nrow=20)

for( i in 1:dim(model)[2]){     
  model_2[,i]=predict(lm(data_1~datos_12[,1:i])) #x(t)|x(t-h),x(t-h-1)
}


a=(pacf(data_1))

numerador_pacf=rep(0,dim(datos_12)[2])
denominador_pacf=rep(0,dim(datos_12)[2])
pacf_1=rep(0,dim(datos_12)[2])

for(i in 2:dim(datos_12)[2]){
  numerador_pacf[i]=cov(data_1,model_1[,i])
  denominador_pacf[i]=sqrt(var(model_2[,(i-1)])*var(model_1[,i]))
  pacf_1[i]=numerador_pacf[i]/denominador_pacf[i]
}
pacf_1


#1
a1=predict(lm(datos_12[,2]~datos_12[,1]-1))
b1=predict(lm(data_1~datos_12[,1:2]-1))
cov(data_1,a1)/sqrt(aut_cov1[1]*var(a1))

m1=lm(datos_12[,2]~datos_12[,1])
cor(data_1,m1$fitted.values,method="pearson")

cov(data_1,m1$fitted.values)/(sd(data_1)*sd(m1$fitted.values))

#2


matriz=cbind(data_1,datos_12[,1],datos_12[,2],datos_12[,3])
matriz1=matriz[1:17,1:4]

m1=lm(matriz1[,1]~matriz1[,2:3])#data
m2_1=lm(matriz1[,4]~matriz1[,2:3])

gamma=(aut_cov1)%*%t(aut_cov1)
solve(gamma)

a1=cov(matriz1[,1],m2_1$fitted.values)
d1=sd(m2_1$fitted.values)*sd(m2_1$fitted.values)
a1/d1





m1=lm(data_1[3:19]~datos_12[1:17,1:2]-1)
var(m1$fitted.values)


var(m1$fitted.values)
m2_1=lm(datos_12[,3]~datos_12[,1:2]-1)

cov(data_1,m2_1$fitted.values)
-0.6481912*cov(data_1,m2_1$fitted.values)

cov(data_1,m2_1$fitted.values)/var(m1$fitted.values)

a[2]

a1=cov(data_1,m2_1$fitted.values)
d1=(sd(m1$fitted.values)*sd(m2_1$fitted.values))
a1/d1




m1=lm(data_1~datos_12[,1:3]-1)
m2_1=lm(datos_12[1:17]~datos_12[1:17,1:2]-1)


solve(cor(datos_12)%*%t(cor(datos_12)))%*%t(cor(datos_12))%*%aut_corr1[1:19]
anova(m1)

a1=cov(data_1[3:19],m2_1$fitted.values)
d1=(sd(data_1[3:19])*sd(m1$fitted.values))
a1/d1


m1$coefficients


m2_1=lm(datos_12[1:17,3]~datos_12[1:17,1:2])

cbind(data_1[3:19],datos_12[1:17,1:3])
cbind(data_1[1:17],datos_12[1:17,1:3])

cov(data_1[3:19],m2_1$fitted.values)/(sd(m2_1$fitted.values)*sd(m1$fitted.values))


a1=cov(data_1[1:17],m2_1$fitted.values)
d1=sd(m2_1$fitted.values)*sd(m1$fitted.values)

a1/d1

cor(m1$residuals,m2_1$residuals)

cov(data_1[1:17],m2_1$fitted.values)/(sd(m1$fitted.values))

a1=cov(m1$fitted.values,m2_1$fitted.values)
d1=sqrt(var(m1$fitted.values)*var(m2_1$fitted.values))
a1/d1  


m1$coefficients[3]

(pacf(data_1))[3]



m2=lm(datos_12[1:17,3]~datos_12[1:17,1:2]-1)
m2_1=lm(data_1[3:19]~datos_12[1:17,1:3]-1)
m2_1$coefficients


a1=cov(m2_1$fitted.values,m2$fitted.values)
d1=sqrt(var(m2_1$fitted.values)*var(m2$fitted.values))
a1/d1  


m2=lm(datos_12[,3]~datos_12[,2:1]-1)
m3=lm(datos_12[,2]~datos_12[,1])
m2_1=lm(data_1~datos_12[,1:2])

r1=data_1-m2$fitted.values
r2=datos_12[,3]-m2_1$fitted.values
r3=datos_12[,2]-m3$fitted.values
cov(r1,r2)/(sd(r1)*sd(r2)*sd(r3))

solve(aut_cov1




 

a1=cov(m2_1$residuals,m2$residuals)
d1=sqrt(var(m2_2$residuals)+var(m2_1$residuals)+var(m2$residuals))
a1/d1  
 

rm(d_1)


a[2]

var(m2$residuals)

cov(data_1,m2$fitted.values)/(sd(m2_1$fitted.values)*sd(m2$fitted.values))


a1=predict(lm(datos_12[,3]~datos_12[,1:3]-1))
#a2=predict(lm(datos_12[,2]~datos_12[,1]-1))
b1=predict(lm(data_1~datos_12[,1:2]-1))
cov(data_1,a1)/sqrt(var(a1)*var(b1))





a1=predict(lm(datos_12[,3]~datos_12[,2:1)]-1))#x3|x1,x2
a2=predict(lm(datos_12[,2]~datos_12[,1]-1))#x2|x1
b21=predict(lm(data_1~datos_12[,1]-1))#xt|x1,x2,x3

cov(data_1,a1)/sqrt(var(b21)*var(a2)*var(a1))

a1=predict(lm(datos_12[,4]~datos_12[,1:3]-1))#x4|x1,x2,x3
a2=predict(lm(datos_12[,3]~datos_12[,1:2]-1))#x3|x1,x2
#a3=predict(lm(datos_12[,2]~datos_12[,1:2]-1))#x2|x1
b21=predict(lm(data_1~datos_12[,1:2]-1))#xt|x1,x2,x3

cov(data_1,a1)/sqrt(var(b21)*var(a2)*var(a1))

(pacf(data_1))


a1=predict(lm(datos_12[,3]~datos_12[,1:3]-1))#x3|x1,x2
a2=predict(lm(datos_12[,2]~datos_12[,1:2]-1))#x2|x1
b21=predict(lm(data_1~datos_12[,1:3]-1))#xt|x1,x2,x3

cov(data_1,a1)/sqrt(var(b21)*var(a2)*var(a1))

#cov(data_1,a1)/sqrt(aut_cov1[2]*var(a2)*var(a1))

#4
a1=predict(lm(datos_12[,4]~datos_12[,1:4]-1))#x4|x1,x2,x3
a2=predict(lm(datos_12[,3]~datos_12[,1:3]-1))#x3|x1,x2
a3=predict(lm(datos_12[,2]~datos_12[,1:2]-1))#x2|x1
b21=predict(lm(data_1~datos_12[,1:4]-1))#xt|x1,x2,x3,x4

cov(data_1,a1)/sqrt(var(b21)*var(a2)*var(a1)*var(a3))

cov(data_1,a1)/sqrt(var(b21)*var(a2)*var(a1)*var(a3))

a[3]



a







cov(data_1,a1)/sqrt(var(b21)*aut_cov1[1]*var(a1)*var(a2))


var(b21)*aut_cov1[1]*var(a2)*var(a3)


cov(data_1,a2)/sqrt(aut_cov1[2]*var(a2))

cov(data_1,a1)/sqrt(aut_cov1[1]*var(a1))

pacf_1[3]
    
    
    
a=a$acf





vars_1=rep(0,)







num=matrix(rep(0,20),ncol=20,nrow=20)
for(i in 1:19){
  num[1:i,i]=0
  num[(i+1):19,i]=aut_corr1[2:(19-i+1)]}

num_1=num
for(i in 2:19){
  num_1[1:(i-1),i]=rev(aut_corr1[2:i])
}

num_2=num_1

for(i in 1:19){
  num_2[i,i]=1
}




m_1=lm(aut_corr1[2:20]~num_2-1)
m_1$coefficients

A=cor(datos_2)

length(aut_corr1[2:20])


A_1=num_2
A_2=list()
A_2[[1]]=num_2
for(i in 2:19 ){
  A_1[,i]=aut_corr1[2:20]
  A_1[,-i]=num_2[,-i]
  A_2[[i]]=A_1
  }



pacf_1=matrix(rep(0,19),ncol=1)
d_1=det(num_2)

for( i in 1:19){
  pacf_1[i]=det(A_2[[i]])/d_1
}

det(A_2[[4]])


Aj_0=A[,2:19]
Aj_1=Aj_0
Aj_2=list()
p_1=0

Aj_2[[1]]=Aj_1
for (i in 2:19){
  Aj_1[,i]=aut_corr1
  Aj_1[,-i]=Aj_0[,-i]
  Aj_2[[i]]=Aj_1
} 


solve(a=num_2[1:18,1:18],b=aut_corr1[2:19])


m_1=lm(datos_2[,1]~datos_2[,2]-1)




#betas=(x^T*x)^(-1)(x^t)*y

#solve(t(datos_12)%*%datos_12)%*%t(datos_12)%*%data_1



for(i in 1:20){
  m_i1[,i]=predict(lm(data_1~datos_2[,2:i]-1))} #x_est(t+h)



pacf_1=matrix(rep(0,19),ncol=1)
for(i in 1:20){
  pacf_1[i]=cor(datos_12_h1[,i]-m_i1[,i],data_1-m_i3)
}

plot(pacf_1,type="h")

bien=(pacf(data_1))

cor(datos_12_h1[,1]-m_i1[,1],data_1-m_i2[,19])
bien$acf

getAnywhere(UseMethod("pacf"))


getAnywhere(ARMAacf)

v1=matrix(rep(0,19),ncol=1)
for(i in 1:19){
  v1[i]=var(m_i1)[i,i] 
}

v2=matrix(rep(0,19),ncol=1)
for(i in 1:19){
  v2[i]=var(m_i2)[i,i] 
}

pacf_1=matrix(rep(0,19),ncol=1)
for(i in 1:19){
  pacf_1[i]=cov_pacf_1[i]/(v2[i]*v1[i])
}

cov(data_1,m_i2[,1])/(var(m_i2[,1])*var(m_i1[,1]))
(pacf(data_1))
pacf_1


(pacf(data_1))



getAnywhere(C_pacf1$CallRoutine )


for(i in 1:20){
  m_i1[,i]=predict(lm(data_1~datos_2[,2:i]-1))}

cov(data_1,datos_12[,1])/var(data_1)



a=matrix(rep(0,20),ncol=20,nrow=20)
for(i in 1:19){
  a[,i]=predict(lm(data_1~datos_12[,-i]-1))
}

b=matrix(rep(0,20),ncol=20,nrow=20)
for(i in 1:19){
  b[,i]=predict(lm(datos_12[,i]~datos_12[,-i])) 
}

(var(data_1)*var(datos_12[,1]))


bien$acf
dato_rez_1=matrix(c(0,0,datos_12[1:18,1]),ncol=1)

cov(data_1,dato_rez_1)/(var(data_1)*var(dato_rez_1))


cbind(data_1,datos_12[,1])
cov(data_1,)



pacf_1=0
for(i in 1:20){
  pacf_1[i]=cov(data_1,a[,i])/v()
}





for(i in 1:20){
  print(cor(data_1-a[,i],b[,i]))
}


cor(datos_12)
bien$acf
xt_h=predict(a)



solve(t(x)%*%x)%*%t(x)%*%y
  

m_i1

   
 
 
  
}



num[1,1]=aut_corr[2,1]
num[k,2]=aut_corr[1,1]
num[1,3]=aut_corr[2,1]
nume[1,k]=aut_crr[2:k-i]


View(cbind(aut_corr1,A[,2]))






p_a=0
for(i in 2:19){
  p_a[i]=det(Aj_2[[i]])
}

p_2=det(A)

p_a/p_2


plot(p_a/p_2,type="h")

(pacf(data_1))


p_1=0
for(k in 2:20){
  p_1[k]=det(Aj_2[[k]][1:k,1:k])  
}

p_1[1]=det(Aj_2[[1]][1:1,1:1]


p_2=0
for(k in 2:19){
  p_2[k]=det(A[1:k,1:k])  
}



p_1[2]=det(Aj_2[[3]][1:3,1:3])


p_1[i]=det(Aj_2[[i]][1:i,1:i])


Aj_1=A
Aj_1[,2]=aut_corr1
Aj_1[,-2]=A[,-2]

Aj_1[,1]=aut_corr1
det(Aj_1[1:2,1:2])


p_1=0
p_1[2]=det(Aj_2[[2]][1:2,1:2])








for (i in 1:20){
  print(Aj_1[1:i,1:i])
}
      

Aj_1=cbind(vector_resultado,A[,2:20])
Aj=rbind(Aj_1[1:17,],vector_resultado)

?lapply

p_1=0
for(i in 2:18){
p_1[i]=det(A[1:i,1:i])
}







length(p_1)





det(Aj[1:(i+k),1:(k+i)])/
rm(A)
rm(Aj)

for(i in 2:19){
  for(k in 1:10){
    print(dim(Aj[1:(i),1:(i)]))
  }}

for(i in 2:19){
    print(dim(Aj[1:(i),1:(i)]))
  }



Aj[1:(1+1),1:(1+1)]

solve(autocor_matrix)%*%diag(x=aut_corr1[1:20])

(pacf(data_1))




aut_corr1=matrix(rep(0,20),ncol=1)
for(i in 1:19){
  aut_corr1[i]=cov(data_1,datos_2[,i])/var(data_1)
}

(acf(data_1))

#remplazar na
datos_12[is.na(datos_12)] <- 0
datos_12=rbind(datos_12,rep(0,19))





datos_12[is.na(datos_12)]=0
datos_22[is.na(datos_22)]=0
datos_22=rbind(datos_22,rep(0,19))
datos_22=datos_22[,19:1]

datos_12_=cbind(data_1,datos_12)
datos_22_=cbind(data_1,datos_12)
autcor_matriz=cor(datos_12_,datos_22_)

autcor_matriz[,1]
cor_1=matrix(rep(0,19),ncol=1)
cor_1[1]=(cov(data_1,datos_22_[,1]))/var(data_1)
for(i in 2:18){
  cor_1[i]=(cor(datos_12_[,i],datos_22_[,i]))
}

datos_12_[,2]-datos_22_[,2]
plot(cor_1,type="h")

?acf
(acf(data_1,type="correlation"))
cor_1

solve(a=autcor_matriz,b=cor_1)

#https://stats.stackexchange.com/questions/129052/acf-and-pacf-formula


#x(t+h)
#modelo=lm(data_1~datos_12)#modelo lineal

#hallando x estimados (t+h) y quitando nas
datos_12_est=matrix(rep(0,19),nrow=20,ncol=19)

modelo=lm(data_1~datos_12-1)
coeficientes=matrix(modelo$coefficients,nrow=length(modelo$coefficients))

for(k in 1:20){
for (i in 1:19){
datos_12_est[k,i]=coeficientes[i]*datos_12[k,i] #x_est(t+h)
}
}


#calculando x_t_est

datos_12_est_x_t=predict(modelo)


# calculando correlaciones




cor(errores,data_1-datos_12_est_x_t)

x <- diff(AirPassengers)
# autocorrelations
sacf <- acf(x, lag.max = 10, plot = FALSE)$acf[,,1]
sacf

# solve the system of equations
res1 <- solve(toeplitz(sacf[1:5]), sacf[2:6])
res1

sacf[2:6]




pacf_1=rep(0,19)
for(i in 1:19){
  pacf_1[i]=cor((datos_12[,i]-datos_12_est[,i]),(data_1-datos_12_est_x_t)) 
}

(pacf(data_1,lag.max=19))

?pacf
(pacf_1=cor(data_1,errores))
(acf(data_1,type="partial"))
pacf(data_1)







