# This file is used to produce quarterly data for the unemployment rates

# load library
library(tempdisagg)

# import annual unemployment rate data
UN<-read.table("Data/UN.txt",header=TRUE,sep="")

Values<-UN$VALUE

UNts<-ts(Values,start=2002,end=2012, frequency=1)
UNq<-td(UNts~1,conversion="average",to="quarterly",method="denton-cholette")

zeval<-print(predict(UNq))
val<-data.frame("UNEMPLOYMENT-RATE"=zeval)

# save to file values (quarterly)
write.table(val, 'Data/UN-quarterly.txt', sep="\t", quote=FALSE)
