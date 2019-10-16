library(tidyverse)
install.packages("plyr")
library(plyr)

load("fish_data.Rdata")

d=fish[fish$depth_fac=="Deep",]

#ddply

nd=ddply(.data=fish,.variables="depth_fac",function(x){
  
z=unique(x$depth_fac)
z


depth_condition=function(y){{}
  if(y=="Deep")
    q<-50
  else if(z=="mid")
    q<-25
  else
    q<-15
}
x$depth_z=depth_condition(y=z)
return(x)
}, 

test=depth_condition((y="mid"))
test
head(d)

nd=ddply(.data=fish,.variables="depth_fac",function(x){
  
}

d$depth_m


#dlpy-------------

#list all the physical dat files in a given directory
batch_data=list.files("batch_data",full=TRUE,pattern="ISIIS")

phy=adply(.data=batch_data,.margins=1,function(file){
#read the data  
d=read.table(file,sep="\t",skip=10,header=TRUE, fileEncoding = "ISO-8859-1",
             stringsAsFactors = FALSE, quote="\"",check.names = FALSE,encoding ="UTF-8",
             na.strings = "9999.99")
#clean names
head=names(d)
head=str_replace(head,"\\(.*\\","")
head=str_trim(head)
head=make.names(head)
head=tolower(head)
head=str_replace(head,fixed(".."),".")
#assign names
names(d=head)
#create a proper date + time format
date=scan(file, what="character",skip=1,nlines = 1,quiet=TRUE)
d$date=date[2]
d$dateTime=str_c(d$date,d$time,sep="")
d$dateTime=as.POSIXct(strptime(x=d$dateTime,
                               format="%m/%d/%y %H:%M:%OS", tz="America/New_York"))

return(d)
},.inform = T, .progress=("text"))
