install.packages("plyr")
library(tidyverse)
library(plyr)

load("fish_data.Rdata")

#test subset to make sure all functions inside the "ddply" umbrella function work
d=fish[fish$depth_fac=="Deep",]

#ddply

nd<- ddply(.data=fish, .variables="depth_fac",function(x){
  z<-unique(x$depth_fac)
  #ifelse(test=z=="Deep",yes=50,no=25)
  depth_condition<-function(y){
    if(z=="Deep")
      q<-50
    else if(z=="Mid")
      q<-25
    else
      q<-15
  }
  
  x$depth_z<-depth_condition(y=z)
  return(x)
}, .inform=T,.progress = "text")

#Test if depth_condition works

test=depth_condition(y="Mid")
test


#Test return of correct values
unique(nd$depth_z)
print(depth_condition(y="Deep"))

d$depth_m



#dlpy-------------

#list all the physical data files in a given directory
batch_data=list.files(paste0("batch_data"),full=TRUE,pattern="ISIIS")


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
  
}, .inform=T,.progress="text")

#23Oct2019

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

#2) combine with if else, set a floor for temp

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

# Non "ggplot2" plotting function
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

# 28Oct2019

library(ggplot2)

#One geom:
data(economics)
e<- economics

unemploy<- ggplot(data=e, aes(x=date,y=unemploy)) +
  geom_line()
unemploy


#multiple geoms-----

data("presidential")
pres<-presidential

caption <- paste(strwrap("Unemployment rates in the U.S. have varied a lot over the years",40),
                 collapse = "\n")
yrng <- range(e$unemploy)
xrng <- range(e$date)
date <- as.Date("1960-01-01")

ggplot(e) +
  geom_line(aes(x = date, y=unemploy)) +
  geom_rect(data = pres, aes(xmin = start,
                             xmax = end, fill = party),
            ymin=-Inf, ymax =Inf, alpha = 0.2) +
  scale_fill_manual(values = c("dodgerblue","firebrick3")) +
  geom_vline(data = pres,
             aes(xintercept=as.numeric(start)),
             colour="grey50", alpha=0.5) +
  # #geom_text(data=pres, aes(x = start, y = 2500,
  #                          label = name), size = 3,
  #           vjust = 0, hjust = 0, nudge_x = 50) +
  annotate("text", x=date, y=yrng[2],label=caption,
           hjust=0,vjust =1, size = 4)



ggplot(e) +
  geom_rect(data=pres, aes(xmin= start, xmax= end, fill= party),
            ymin=-Inf, ymax=Inf, alpha=0.2) +
  geom_vline(data=pres,
             aes(xintercept=as.numeric(start)),colour="grey50",alpha=0.5)+
  geom_text(data=pres, aes(x=start, y=2500,
                           label=name), size=3, vjust=0, hjust=0,nudge_x=50)


ggplot(e) +
  geom_rect(data=pres, aes(xmin= start, xmax= end, fill= party),
            ymin=-Inf, ymax=Inf, alpha=0.2) +
  scale_fill_manual(values=c("dodgerblue", "firebrick3"))
geom_vline(data=pres,
           aes(xintercept=as.numeric(start)),colour="grey50",alpha=0.5)+
  geom_text(data=pres, aes(x=start, y=2500,
                           label=name), size=3, vjust=0, hjust=0,nudge_x=50)

caption<-paste(strwrap("Unemployment rates in the U.S. have varied a lot
over the years",20),
               collapse="\n")
yrng<-range(e$unemploy)
xrng<-range(e$date)

ggplot(e) +
  geom_rect(data=pres, aes(xmin= start, xmax= end, fill= party),
            ymin=-Inf, ymax=Inf, alpha=0.2) +
  scale_fill_manual(values=c("dodgerblue", "firebrick3")) +
  geom_vline(data=pres,
             aes(xintercept=as.numeric(start)),colour="grey50",alpha=0.5)+
  #geom_text(data=pres, aes(x=start, y=2500,
  #label=name), size=3, vjust=0, hjust=0,nudge_x=50) +
  annotate("text", x=xrng[1], y=yrng[2],label=caption, hjust=0, vjust=1,
           size=4)

caption<-paste(strwrap("Unemployment rates in the U.S. have varied a lot
over the years",40),
               collapse="\n")
yrng<-range(e$unemploy)
xrng<-range(e$date)
date<-as.Date("1960-01-01")

ggplot(e) +
  geom_line(aes(x=date,y=unemploy))+
  geom_rect(data=pres, aes(xmin= start, xmax= end, fill= party),
            ymin=-Inf, ymax=Inf, alpha=0.2) +
  scale_fill_manual(values=c("dodgerblue", "firebrick3")) +
  geom_vline(data=pres,
             aes(xintercept=as.numeric(start)),colour="grey50",alpha=0.5)+
  #geom_text(data=pres, aes(x=start, y=2500,
  #label=name), size=3, vjust=0, hjust=0,nudge_x=50) +
  annotate("text", x=xrng[1], y=yrng[2],label=caption, hjust=0, vjust=1,
           size=4)

#Bar graph challenge----
#Stacked bar with grouped bar

load("fish_data.Rdata")
library(tidyverse)

fs<-fish %>% group_by(area_fac, depth_fac, yr_fac) %>%
  summarise(parcel.count=length(parcel.id))
fs


ggplot(data = fs) +
  geom_bar(aes(x=area_fac, y=parcel.count, fill= depth_fac),
           position="stack",
           stat="Identity")

#For getting up and down
ggplot(data = fs) +  
  geom_bar(aes(x=area_fac, y=parcel.count, fill= depth_fac),
           position="stack",
           stat="Identity")  +
  facet_grid(yr_fac~.)

#grouped bar-----
ggplot(data = fs) +  
  geom_bar(aes(x=area_fac, y=parcel.count, fill= depth_fac),
           position="dodge",
           stat="Identity")

#Using the 'ddply' function to create multiply plots----

