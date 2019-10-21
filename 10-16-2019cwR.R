library(tidyverse)
install.packages("plyr")
library(plyr)

load("fish_data.Rdata")

d=fish[fish$depth_fac=="Deep",]

#ddply

nd=ddply(.data=fish,.variables="depth_fac",function(x){
  
z=unique(d$depth_fac)
#ifelse(test = z == "Deep", yes = 50, no =25)

depth_condition=function(y){
  if(y=="Deep")
    q<-50
  else if(z=="Mid")
    q<-25
  else
    q<-15
}
x$depth_z=depth_condition(y=z)
return(x)
},.inform = T,.progress = "test")


nd = ddply(.data = fish, .variables = "depth_fac", function(x){
  
  z = unique(x$depth_fac)
  
  #ifelse(test = z == "Deep", yes = 50, no =25)
  
  depth_condition = function(y){
    if(y=="Deep")
      q = 50
    else if(y=="Mid")
      q = 25
    else
      q = 15
  }
  
  x$depth_z = depth_condition(y = z)
  
  return(x)
  
}, .inform = T, .progress = "test")
 

#Test if depth_condition works
test = depth_condition(y = "Mid")
test

#Test return of correct values
unique(nd$depth_z)
print(depth_condition(y="Deep"))





nd=ddply(.data=fish,.variables="depth_fac",function(x){
  
}

d$depth_m



#dlpy-------------

#list all the physical dat files in a given directory
batch_data=list.files(paste0("batch_data"),full=TRUE,pattern="ISIIS")

phy=adply(.data=batch_data,.margins=1,function(file){
#read the data  
d=read.table(batch_data[1],sep="\t",skip=10,header=TRUE,fileEncoding = "ISO-8859-1",
             stringsAsFactors = FALSE,quote="\"",check.names = FALSE,encoding ="UTF-8",
             na.strings = "9999.99")
#clean names
head=names(d)
head=str_replace(head, "\\(.*\\)","")
head=str_trim(head)
head=make.names(head)
head=tolower(head)
head=str_replace(head,fixed(".."),".")
#assign names
names(d=head)
#create a proper date + time format
date=scan(batch_data[1], what="character",skip=1,nlines = 1,quiet=TRUE)
d$date=date[2]
d$dateTime=str_c(d$date,d$time,sep="")
d$dateTime=as.POSIXct(strptime(x=d$dateTime,
                               format="%m/%d/%y %H:%M:%OS", tz="America/New_York"))

return(d)
},.inform = T, .progress="text")


# list all the physical data files in a given directory
batch_data <- list.files(paste0("batch_data"), full=TRUE, pattern = "ISIIS")

phy <- adply(batch_data, 1, function(file) {
  
  # read the data
  d <- read.table(batch_data[1], sep="\t", skip=10, header=TRUE, fileEncoding="ISO-8859-1",
                  stringsAsFactors=FALSE, quote="\"", check.names=FALSE, encoding="UTF-8",
                  na.strings="9999.99")
  
  # clean names
  head <- names(d)
  head <- str_replace(head, "\\(.*\\)", "")
  head <- str_trim(head)
  head <- make.names(head)
  head <- tolower(head)
  head <- str_replace(head, fixed(".."), ".")
  # assign names
  names(d) <- head
  
  # create a proper date + time format
  date <- scan(batch_data[1], what="character", skip=1, nlines=1, quiet=TRUE)
  
  d$date <- date[2]
  
  d$dateTime <- str_c(d$date, d$time, sep=" ")
  
  d$dateTime <- as.POSIXct(strptime(d$dateTime, format="%m/%d/%y %H:%M:%OS", tz="America/New_York"))
  
  return(d)
  
}, .progress="text")


# for loop
#1) starting with temperature conversion

t<-data.frame(f_deg=seq(0,100,1))
t
t$c_deg<-NA
t$k_deg<-NA


for(i in 1:nrow(t)) {
  t[i,]$c_deg<-(t[i,]$f_deg - 32)*(9/5)
  t[i,]$k_deg<-t[i,]$c_deg + 273.15
 }

for(i in 1:10){
  t[i, ]$c_deg<-(t[i,]$f_deg -32)*(9/5)
  t[i, ]$k_deg<-t[i,]$c_deg + 273.15
}


rm(t)

#2) combine with if else, set afloor for temp

t<-data.frame(f_deg=seq(0,100,1))
t
t$c_deg<-NA
t$k_deg<-NA
t$rel_temp<-NA

for(i in 1:nrow(t)){
  t[i,]$c_deg<-(t[i,]$f_deg -32)*(9/5)
  t[i,]$k_deg<-t[i,]$c_deg + 273.15
  t[i,]$rel_temp<-ifelse(test=t[i,]$c_deg<0,
                         yes="cold",
                         no="not cold")
}

rm(t)

#3) use a conditional statement(if <0, but >1)
#2) combine with if else, set a floor for temp

t<-data.frame(f_deg=seq(0,100,1))
t
t$c_deg<-NA
t$k_deg<-NA
t$rel_temp<-NA

goldilocks<-function(x){
  if(x<=0)
    t[i,]$rel_temp="frozen"
  else if(x>0 & x<=50)
    t[i,]$rel_temp="cold"
  else if(x>50 & x<=70)
    t[i,]$rel_temp="warm"
  else
    t[i,]$rel_temp="hot"
}

for(i in 1:nrow(t)){
  t[i,]$c_deg<-(t[i,]$f_deg -32)*(9/5)
  t[i,]$k_deg<-t[i,]$c_deg + 273.15
  t[i,]$rel_temp=goldilocks(x=t[i,]$c_deg)
}

m=data.frame(mg_mass=seq(0,100,1))


# Nexting for loop
y=seq(1,10,0.5)
x=seq(1,20,1)
d=data.frame(interaction=seq(1,10,0.5))

for(i in 1:length(y)){
  for(k in 1:19){
    d$output=y[i] + x[k]
  }
}
