##KELOMPOK 4 (GANJIL)##
#NAMA : FATCHI ARIFATUL ULYA  (B2A020021)
#       YUSRISMA ASYFANI      (B2A020023)
#       NUR HANIFAH IBRAHIM   (B2A020036)
#ADDITIVE & EKSPONENSIAL

Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n) 
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
  X<-xi[,3]
  i<-1000
  lambda<-4
  U<-runif(i)
  X<--log(U)/lambda
  hist(X)
  X<- rexp(16,4)
  X
}
Additive_RNG(35,11123,437,138,100)
