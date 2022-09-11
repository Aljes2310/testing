library(ncdf4)
library(lubridate)
library(spData)
library(kableExtra)
library(cowplot)
data("world")
require(rerddap)
require(tidyverse)
library(metR)
library(cmocean)
library(viridis)
setwd("~/datos netcdf tesis/sea velocity")
library(metR)
library(cmocean)
library(reshape2)

#unidades:m/s, vo: nortward velocity, so: eastward velocity
v="global-reanalysis-phy-001-030-monthly_1634163480120.nc"
n=nc_open(v)
vo=ncvar_get(n,"vo")
uo=ncvar_get(n,"uo")
depth=ncvar_get(n,"depth")
lat=ncvar_get(n,"latitude")
lon=ncvar_get(n,"longitude")
time=ncvar_get(n,"time")
time=(time/24)+as.Date("1950-01-01")


vo= melt(vo)
vo$Var1=as.factor(vo$Var1)
vo$Var2=as.factor(vo$Var2)
vo$Var3=as.factor(vo$Var3)
vo$Var4=as.factor(vo$Var4)
levels(vo$Var1)=as.numeric(lon)
levels(vo$Var2)=as.numeric(lat)
levels(vo$Var3)=as.numeric(depth)
levels(vo$Var4)=time
colnames(vo)=c("Longitud", "Latitud", "Depth", "DATE", "VO")
vo$Latitud=as.numeric(as.character(vo$Latitud))
vo$Longitud=as.numeric(as.character(vo$Longitud))
vo$Depth=as.numeric(as.character(vo$Depth))
vo$DATE=as.Date(vo$DATE)

uo=melt(uo)
names(uo)[5]="uo"
df=cbind(vo, uo$uo)
colnames(df)=c("Longitud", "Latitud", "Depth", "DATE", "vo","uo")
df=as_tibble(df)

summary(df$uo)
#month
df=df%>% mutate(month = month(DATE), year=year(DATE))
df=na.omit(df)
df$vo=df$vo*100
df$uo=df$uo*100
df=na.omit(df)
df$velocity = sqrt((df$uo)^2 + (df$vo)^2)

#ver solo 50m
Current0_50m=df%>%filter(Depth<=50)
Current50_100m=df%>%filter(Depth>=50 & Depth<=100)

Current50_100m=Current50_100m%>%select(velocity)%>%
  group_by(year=Current50_100m$year, month=Current50_100m$month, Latitud=Current50_100m$Latitud, Longitud=Current50_100m$Longitud)%>% 
  summarise_if(is.numeric, mean , na.rm=TRUE)



colnames(superf)[5]="Current_Velocity"
colnames(superf)[1:2]=c("Longitud","Latitud")
world


ggplot(data = superf%>%filter(year==2017),aes(x = Longitude, y = Latitude)) + 
  geom_raster(aes(fill = uo), interpolate = FALSE)+
  geom_contour(aes(z = uo), color="black")+
  geom_text_contour(aes(z = uo), skip=0, stroke = 0.1)+
  geom_sf(data =spData::world, col = "black", fill = "ivory",inherit.aes = FALSE)+
  coord_sf(xlim = c(-78,-70), ylim = c(-20,-15))+
  scale_fill_gradientn(name = "cm/s", colours=c("navyblue","blue","cyan", "white","red")
                       , na.value = "White")+
  theme_bw()+
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 11, colour = 1),
        panel.grid = element_line(colour = NA),
        legend.position = "right",
        legend.background = element_rect(colour = NA, fill = NA),
        legend.key.height = unit(2.5, "lines"),
        legend.key.width = unit(1, "lines"),
        panel.spacing.x = unit(8, "mm"),
        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(-78,-70,2))+
  labs(x = NULL, y = NULL)+
  facet_wrap(~month, nrow = 3)+
  ggtitle("Zonal Current velocity(cm/s) 2017")



