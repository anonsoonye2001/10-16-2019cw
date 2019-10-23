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

patch.list=list()

max.brks.index=nrow(brks)
max=max(brks$no)-1

for(k in brks$no[1]:max){
  p.mid=in.patch[in.patch$r.index>=brks$r.index[k] &
                   in.patch$r.index<brks$r.index[k+1],]
  if(nrow(p.mid)>0){
    p.mid$patch.id<-k+1
    patch.list[[k]]<-p.mid
  }
}

patch.df=do.cal()




## plotting
#using ggplot 2
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

load("fish_data.Rdata")

# Non "ggplot2" 
fish.deep=fish[fish$deep_fac=="Deep"]

plot(x=fish.deep$parcel.start.lon,
     y=fish.deep$parcel.start.lat)


## Histogram
hist(log10(fish$parcel.density.m3))

#------

#ggplot2 functions----
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point()



ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(colour="blue")

head(mpg)
unique(mpg$class)

# assigning colours
ggplot(data=mpg, aes(x=displ, y=hwy, colour=class)) +
  geom_point()
length(unique(mpg$class))

# assigning clours manually
ggplot(data=mpg, aes(x=displ, y=hwy, colour=class)) +
  geom_point() +
  scale_colour_manual(values = c("firebrick","dodgerblue","darkgreen","goldenrod",
                                 "ivory","chocolate2","deeppink1"))

#ggplot2: line geom-----
ggplot(data=mpg, aes(x=displ, y=hwy, colour=class)) +
  geom_line()

ggplot(data=mpg, aes(x=displ, y=hwy, colour=class)) +
  geom_line() +
  scale_colour_manual(values = c("firebrick","dodgerblue","darkgreen","goldenrod",
                                 "ivory","chocolate2","deeppink1"))

# facets----
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_line() +
  facet_wrap(~class)

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  facet_wrap(~class)

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_boxplot() +
  facet_wrap(~class)

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  facet_wrap(~class, nrow=4)

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  facet_wrap(~class, ncol=1)

# add a smoother----, lm=linear model

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  geom_smooth()

ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  geom_smooth(method = "lm")

#histogram

ggplot(data=mpg, aes(displ, fill=drv)) +
  geom_histogram(binwidth=0.5) 


ggplot(data=mpg, aes(displ, fill=drv)) +
  geom_freqpoly(binwidth=0.5) 

ggplot(data=mpg, aes(displ, colour=drv)) +
  geom_freqpoly(binwidth=0.5) 

