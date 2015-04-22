# load libraries
library(tempdisagg)
library(psych)
library(dyn)
library(car)
library(RColorBrewer)

########################################################
######   CONSTRUCT UNEMPLOYMENT RATES AND FED DATA #############
########################################################

# Import annual unemployment rate data and convert to quartely data for use cases
UN<-read.table("Data/UN.txt",header=TRUE,sep="")
FED<-read.csv("Data/FED.csv",header=FALSE,sep=",")

Values<-UN$VALUE
values_FED<-FED$V2

UNts<-ts(Values,start=2002,end=2012, frequency=1)
UNq<-td(UNts~1,conversion="average",to="quarterly",method="denton-cholette")

FEDts<-ts(values_FED,start=2002,end=2012, frequency=1)
FEDq<-td(FEDts~1,conversion="average",to="quarterly",method="denton-cholette")

zeval<-print(predict(UNq))
val<-data.frame("UNEMPLOYMENT-RATE"=zeval)

ze_fed_val<-print(predict(FEDq))
val_fed<-data.frame("GOVERNMENT-SPENDING"=ze_fed_val)

# save to file values (quarterly)
write.table(val, 'Data/UN-quarterly.txt', sep="\t", quote=FALSE, row.names=FALSE)

write.table(val_fed, 'Data/FED-quarterly.txt', sep="\t", quote=FALSE, row.names=FALSE)

########################################################
######   CONSTRUCT ALL DATA ############################
########################################################

# Skip first year of results - because of model 2 which needs previous years data.
header_gdp<-read.table("Data/GDP.txt",header=FALSE,sep="",nrows=1,stringsAsFactors=FALSE)
header_un<-read.table("Data/UN-quarterly.txt",header=FALSE,sep="",nrows=1,stringsAsFactors=FALSE)
header_m2<-read.table("Data/M2.txt",header=FALSE,sep="",nrows=1,stringsAsFactors=FALSE)
header_nse<-read.table("Data/NSE.txt",header=FALSE,sep="",nrows=1,stringsAsFactors=FALSE)

# Skip first year worth of data, Used for Model 2
gdp<-read.table("Data/GDP.txt",header=FALSE,sep="",skip=5)
un<-read.table("Data/UN-quarterly.txt",header=FALSE,sep="",skip=5)
m2<-read.table("Data/M2.txt",header=FALSE,sep="",skip=5)
nse<-read.table("Data/NSE.txt",header=FALSE,sep="",skip=5)

# Assign headers to data
colnames(gdp)<-unlist(header_gdp)
colnames(un)<-unlist(header_un)
colnames(m2)<-unlist(header_m2)
colnames(nse)<-unlist(header_nse)

# Convert into data frames
datas<-data.frame(gdp,un,nse,m2)
datas

###########################################################################
######   Save Model 1 Descriptive Stats to file - 'Results/' ##############
###########################################################################

descriptive<-describe(datas)
write.csv(descriptive, 'Results/Descriptive-Analyses.csv', quote=FALSE)

regres<-lm(datas$NSE200~datas$GDP+datas$UNEMPLOYMENT.RATE+datas$M2, data=datas)

# summary -R-squared
regression<-summary(regres)

# Write analysis to file
sink('Results/Relationship-Analysis.txt')
regression
sink()


################################################################################
#################### Model 2 1st stage #########################################
################################################################################

money<-read.table("Data/M2.txt",header=TRUE,sep="")
unemployment<-read.table("Data/UN-quarterly.txt", header=TRUE, sep="")
gross<-read.table("Data/GDP.txt", header=TRUE, sep="")
stock<-read.table("Data/NSE.txt", header=TRUE, sep="")
fed<-read.table("Data/FED-quarterly.txt", header=TRUE, sep="")

d<-data.frame(money,unemployment,gross,stock,fed)
dmoney<-d$M2
unemp<-((d$UNEMPLOYMENT.RATE)/100)
grossy<-d$GDP
stocky<-d$NSE
fedy<-d$GOVERNMENT.SPENDING

n<-c(1:44)

DMt<-0
for (i in n){
  DMt[i]<-dmoney[i+3]-dmoney[i]
}
DMt

UNt<-0
for (i in n) {
  UNt[i]<-log((unemp[i]/(1-unemp[i])), base=10)
}
UNt

FED_t<-0
FEDVt<-0
for (i in n) {
  FED_t[i]<-0.2*(log10(fedy[i+1]))+0.8*(log10(fedy[i]))
  FEDVt[i]<-log10(fedy[i])-FED_t[i]
}
FEDVt

################################################################################
#################### 1st stage regression of Modle 2 ###########################
################################################################################

dmt<-zoo(DMt)
unt<-zoo(UNt)
model<-dyn$lm(dmt~lag(dmt,-1)+lag(dmt,-2)+lag(dmt,-3)+lag(dmt,-4)+lag(dmt,-5)+lag(dmt,-6)+lag(unt,-1)+lag(unt,-2)+lag(unt,-3)+FEDVt)
model
modell<-summary(model)

# Write analysis to file
sink('Results/Model2-Regression-Lag-Analysis.txt')
modell
sink()


################################################################################
#################### Unanticipated & Anticipated money supply###################
################################################################################

unan_m2<-0
for (i in n) {
  unan_m2[i]<-dmoney[i]-DMt[i]
}
unan_m2

################################################################################
###### Descriptive Stats for Anticipated and Unanticipated Money Supply#########
################################################################################

dataz<-data.frame(unan_m2, DMt)
anti_unanti<-describe(dataz)

write.csv(descriptive, 'Results/Descriptive-Anticipated-Unanticipated-Analysis.csv', quote=FALSE)

################################################################################
####################### Final Regression #######################################
################################################################################

unemploy<-d$UNEMPLOYMENT.RATE

final_model<-lm(stocky~DMt+unan_m2+grossy+unemploy)
model_final<-summary(final_model)

ny<-4
dates<-rep(2002:2012, each=ny)

final_data<-data.frame(dates,stocky,DMt,unan_m2,dmoney)
col_headings <- c('Years','NSE20','Anticipated.M2','UnAnticipated.M2','Actual.M2')
names(final_data) <- col_headings
final_data

spm(~NSE20+Anticipated.M2+UnAnticipated.M2+Actual.M2 | Years,
      data=final_data,
      diagonal=c("none"),
      col=brewer.pal(12, "Paired"),
      main="ScatterPlot Matrix for all Variables")

# Write to file
sink('Results/Final-Analysis.txt')
model_final
# plot(final_model)
sink()
