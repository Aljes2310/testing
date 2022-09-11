library(ncdf4)
library(lubridate)
library(spData)
library(ggsn)
library(kableExtra)
library(cowplot)
data("world")
require(rerddap)
require(tidyverse)
library(metR)
library(cmocean)
library(viridis)
setwd("~/datos netcdf tesis/salinidad mensual")
library(MBA)
library(metR)
library(cmocean)
library(reshape2)
library(ggpubr)
n1="global-reanalysis-phy-001-030-monthly_1632455121232.nc"
n2="global-reanalysis-phy-2016-001-030-monthly_1632958859536.nc"
n3="global-reanalysis-phy-2017-001-030-monthly_1633050075101.nc"

f=c(n1,n2,n3)
listadedataframes=list()
for(i in 1:3){
n=nc_open(f[i])
sal=ncvar_get(n,"so")
depth=ncvar_get(n,"depth")
lat=ncvar_get(n,"latitude")
lon=ncvar_get(n,"longitude")
time=ncvar_get(n,"time")
time=(time/24)+as.Date("1950-01-01")

#dim(sal)
rerr= melt(sal)
rerr$Var1=as.factor(rerr$Var1)
rerr$Var2=as.factor(rerr$Var2)
rerr$Var3=as.factor(rerr$Var3)
rerr$Var4=as.factor(rerr$Var4)
levels(rerr$Var1)=as.numeric(lon)
levels(rerr$Var2)=as.numeric(lat)
levels(rerr$Var3)=as.numeric(depth)
levels(rerr$Var4)=time
colnames(rerr)=c("Longitud", "Latitud", "Depth", "DATE", "sal")

rerr$Latitud=as.numeric(as.character(rerr$Latitud))
rerr$Longitud=as.numeric(as.character(rerr$Longitud))
rerr$Depth=as.numeric(as.character(rerr$Depth))
rerr$DATE=as.Date(rerr$DATE)
rerr=as_tibble(rerr)
listadedataframes[[i]]=list(rerr)}

que=bind_rows(listadedataframes)

#VISUALIZACION
#SECCION TEMPERATURA-LONGITUD
df=que%>%
  mutate(month = month(DATE), day=day(DATE)) %>%
  dplyr::select(DATE, month, Latitud, Longitud, Depth, sal)

df=na.omit(df)
df=df %>% distinct()

#seccion longitud
list1=list()

for(i in 1:12){
primero=df%>%filter(month==i & Latitud<=-16 & Latitud>=-17)

mba <- mba.surf(primero[,c('Longitud', 'Depth', 'sal')], 500, 500)
dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y) 
df2 <- melt(mba$xyz.est$z, varnames = c('Longitud', 'Depth'), value.name = 'sal')

list1[[i]]=list(df2)}

plots <- list()
for(i in 1:12){
  plots[[i]]=ggplot(data=as.data.frame(list1[[i]]), aes(Longitud, Depth))+
  geom_raster(aes(fill = sal),interpolate = FALSE, hjust = 0.5, vjust = 0.5) +
  geom_contour(aes(z = sal), breaks=seq(34.2,35.8, 0.1), color="black") + 
  stat_contour(aes(z = sal),breaks = c(35.1), colour = "red")+
  geom_text_contour(aes(z = sal), breaks=seq(34.2,35.8, 0.1), skip=0, stroke = 0.1)+
  scale_y_reverse(limits=c(400,0),breaks=seq(0,400,25)) + 
  scale_fill_gradientn(name= "Salinidad(PSU)",colors = viridis(14), breaks=seq(34.2,35.6, 0.1))+
  coord_cartesian(expand = F, xlim=c(-78,-70.8))+
  scale_x_continuous(breaks = seq(-78,-70,0.5))+
  guides(fill = guide_colourbar(barwidth = 0.7, barheight = 15))+
  theme(text = element_text(size=10),
        axis.title.y = element_text(margin = margin(r = 7)),
        axis.title.x = element_text(margin = margin(t = 7)),
        legend.title = element_text(vjust = 4),
        plot.title = element_text(hjust = 0.5))+
  labs(x="Longitud", y="Profundidad(m)") }

graphs=plots[c(1:6)]


figures=do.call(ggpubr::ggarrange, c(graphs, common.legend = TRUE, legend="right", list(labels = c("A", "B", "C", "D","E","F"),
                                                                                     ncol = 3, nrow = 2)))
      
annotate_figure(figures, top = text_grob("Seccion de Salinidad(16-17°S) ENERO-JUNIO 2017", face = "bold", size = 12))


#SALINIDAD SUPERFICIAL PROMEDIO
Sal0_50m=df%>%filter(Depth<=50)
Sal50_100m=df%>%filter(Depth>=50 & Depth<=100)
Sal50_100m= Sal50_100m%>%dplyr::select(sal)%>% 
  group_by(year=year(Sal50_100m$DATE), month=Sal50_100m$month, Longitud=Sal50_100m$Longitud, Latitud=Sal50_100m$Latitud) %>% 
  summarise_if(is.numeric, mean , na.rm=TRUE)

ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
ODV_colours=rev(ODV_colours)

ggplot() + 
  geom_raster(data = shallow, 
              aes(x = Longitud, y = Latitud, fill = sal), interpolate = FALSE)+
  geom_contour(data = shallow, aes(x = Longitud, y = Latitud, z = sal), breaks=seq(34.6,36, 0.1), color="black")+
  stat_contour(data = shallow, aes(x = Longitud, y = Latitud,z = sal),breaks = c(35.1), colour = "white", size=1)+
  geom_text_contour(data = shallow, aes(x = Longitud, y = Latitud, z = sal), breaks=seq(34.8,35.8, 0.1), skip=0, stroke = 0.1)+
  geom_sf(data = spData::world, col = "black", fill = "ivory")+
  coord_sf(xlim = c(-78,-70), ylim = c(-20,-15))+
  scale_fill_gradientn(name = "Salinidad(PSU)", limits = c(34.4,36), breaks=seq(34.2,36,0.2),
                       colours = ODV_colours, na.value = "White")+
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
  ggtitle("Salinidad superficial 2017")

#correlacion ASS y pota

SS=list()

for(i in unique(as.list(shallow$DATE))){
ASS=shallow%>%filter(DATE==i & Latitud<=-17 & Latitud>=-18 & 
                      Longitud<=-71 & Longitud>=-74)

SS1=ASS%>% dplyr::select(sal)%>%
  group_by(AÑO=ASS$DATE, MES=ASS$month) %>%
  summarise_if(is.numeric, mean , na.rm=TRUE)

SS[[i]]=list(SS1)}

Sal0=bind_rows(SS)

Captura=ts17$captura_kg[1:36]
soi=ts (Sal0$sal[1:36])
rec = ts(Captura)
ccf (soi, rec) 
plot(soi,rec)
cor.test(soi,rec, method = "spearman")

Sal0$captura=Captura
h=ggplot(data=Sal0, aes(x=AÑO, y=sal))+geom_line()
p=ggplot(data=Sal0, aes(x=AÑO, y=Captura))+geom_line()
ggarrange(p,h)




-------------------------------------------------------------
#perfil
profile=df%>%filter(Latitud<=-16 & Latitud>=-18 & Longitud<=-72 & Longitud>=-76)
ggplot(data=profile,aes(x=sal, y=Depth))+ 
  geom_point()+
  #geom_path()+
  scale_y_reverse(breaks=seq(0,400,25), limits=c(400,0))+
  scale_x_continuous(breaks=seq(34.4,35.6,0.2),limits = c(34.4,35.6))+
  theme(panel.background = element_rect(colour = 1, fill = "white"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid=element_line(colour="grey50"))+ 
  labs(x="SALINIDAD (PSU)", y="PROFUNDIDAD (m)")+ 
  guides(colour = guide_legend(override.aes = list(size=5)))+
  facet_wrap(~month, nrow = 3)


#nucleo de ATSA= minima de salinidad subsuperficial

core=NULL
for(i in 1:12){
  profile=df%>%filter(month==i & Latitud<=-16 & Latitud>=-16 & Longitud<=-74.43 & Longitud>=-75)
core[i]=min(profile$sal[which(profile$Depth>=50 & profile$Depth<=200)])}

core=round(core,3)

profile$Depth[profile$sal==core & profile$Depth<=300]

prof=c(34.72, 34.68, 34.6, 34.67, 34.5, 34.55, 34.59, 34.6, 34.6, 34.49, 34.56, 34.5,
       core)
prof1=prof[1:24]
Captura=ts18$captura_kg[25:36]
soi=ts (core)
rec = ts(Captura)
ccf (soi, rec) 
cor.test(soi,rec, method = "spearman")

#seccion latitud
segundo=df%>%filter(month==4, Latitud<=-15 & Latitud>=-20 & Longitud<=-71 & Latitud>=-75)
mba <- mba.surf(segundo[,c('Latitud', 'Depth', 'sal')], 300, 300)
dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y) 
df3 <- melt(mba$xyz.est$z, varnames = c('Latitud', 'Depth'), value.name = 'sal')

ggplot(data=df3, aes(Latitud, Depth))+
  geom_raster(aes(fill = sal),interpolate = F, hjust = 0.5, vjust = 0.5) +
  geom_contour(aes(z = sal),
               breaks=seq(34.2,35.6, 0.1), color="black") + stat_contour(aes(z = sal),breaks = c(34.9, 35.1), colour = "red")+
  geom_text_contour(aes(z = sal), breaks=seq(34.2,35.6, 0.1), skip=0, stroke = 0.1)+
  scale_y_reverse(limits=c(400,0),breaks=seq(0,400,25)) + 
  scale_fill_gradientn(name= "Salinidad(PSU)",colors = viridis(14), breaks=seq(34.2,35.6, 0.1))+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = seq(-20,-15,1))+
  guides(fill = guide_colourbar(barwidth = 0.7, barheight = 15))+
  theme(text = element_text(size=10),
        axis.title.y = element_text(margin = margin(r = 7)),
        axis.title.x = element_text(margin = margin(t = 7)),
        legend.title = element_text(vjust = 4),
        plot.title = element_text(hjust = 0.5))+
  labs(x="Longitud", y="Profundidad(m)") +
  #facet_wrap(~month, nrow = 3)+
  ggtitle("Seccion de Salinidad Enero 2015")+
  scale_x_reverse()


-----------------------------------------------------

#PROMEDIOS MENSUALES  
temp.month = df%>% 
  group_by(Longitud, Latitud ,Depth, month) %>% 
  summarise(temp = mean(temp, na.rm = TRUE))%>%
  
    
mba <- mba.surf(temp.month[,c('Longitud', 'Depth', 'temp')], 500, 500)
dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y) 
df3 <- melt(mba$xyz.est$z, varnames = c('Longitud', 'Depth'), value.name = 'temp')

library(ggplot2)

ggplot(data=df3, aes(Longitud, Depth))+
  geom_raster(aes(fill = temp), interpolate = F, hjust = 0.5, vjust = 0.5) +
  geom_contour(aes(z = temp), breaks=seq(4,29, 1)) + 
  geom_text_contour(aes(z = temp),, breaks=seq(4,29, 1), stroke = 0.1)+
  scale_y_reverse(limits=c(300,0),breaks=seq(0,300,25)) + 
  scale_fill_gradientn(name= "Temperatura(°C)",colours = matlab.like2(7), breaks=seq(4,28,4))+
  coord_cartesian(expand = F)+
  scale_x_longitude()





