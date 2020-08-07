### Analysis and figures the Nesting biology of Green-and-gold tanager
###(Tangara schrankii): unique traits for lowland reproductive success? by 
### Mario Loaiza-Muñoz and Gustavo Londoño Contact: loaizamunoz01@gmail.com  

library(ggplot2)
library(plyr)
library(gridExtra)
library(grid)
library(rlang)
library(lme4)

### Figure 3 Incubation behavior of the Green-and-gold Tanager (Tangara schrankii) 
### throughout the incubation period, based on 32 nests monitored at the Tono and 
### Pantiacolla stations between 2008 and 2014. 

### Text features and background elements

fondo  <- theme(panel.border = element_rect(linetype = "solid", fill = NA),
                panel.background = element_rect(fill = "white"),
                legend.position="none") 
ejestext <- theme(axis.title.y = element_text(family="serif",vjust=1.2,size = 13),
                  axis.title.x = element_text(family="serif",vjust=-0.5,size = 13),
                  axis.text.x = element_text(family="serif",size = 12,lineheight = 0.9, vjust = 1),
                  axis.text.y = element_text(family="serif",size = 12,lineheight = 0.9, vjust = .5))

### Figure 3A

### Load  Incubation_ts.csv

tsincu<-read.table(file.choose(),header=T, sep=";", dec=",")

a <- ggplot(tsincu, aes(estado, on, fill=estado)) + 
    fondo + ejestext +
    geom_boxplot(fill="gray")+
    scale_x_discrete(limit=c("Early", "Middle", "Late"))+
    geom_jitter(shape=16, position=position_jitter(0.2), colour= "grey60")+
    stat_boxplot(geom ='errorbar', size = .6, width=0.6) + 
    labs(x="", y = "Attentiveness (%)")+ 
  theme(axis.text.x = element_blank())+
    annotate(family= "serif",geom = "text", x = 0.4, y = 82, 
           label = "A", size = 5, hjust = -1, vjust = 0)

### Figure 3B 

### incubacion_min_Ts.csv

tsmin<-read.table(file.choose(),header=T, sep=";", dec=",")

### Violin function divided author in https://stackoverflow.com/questions/51228076/ggplot-split-violin-plot-with-horizontal-mean-lines

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1,'group']
  newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 
                                              1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})

geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


b <- ggplot(tsmin, aes(Status, Minutos, fill=Event)) + 
  ejestext + fondo +
  geom_split_violin(trim = TRUE)+
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean,
               geom = "crossbar", 
               width = 0.25,
               position = position_dodge(width = .25),)+
  scale_fill_manual(values=c("gey20", "grey60"))+
  labs(x="Incubation period", y = "Incubation bouts duration (Min)")+
  scale_x_discrete(limits=c("Early","Middle","Late"))+
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.92, 0.84), legend.title = element_blank(),
        legend.key =element_rect(colour="white"))+
  annotate(family= "serif",geom = "text", x = 0.4, y = 290, 
           label = "B", size = 5, hjust = -1, vjust = 0)

tiff("Figure_3.tiff",width=12,height=15,
     units="cm",res=400,compression="lzw")
grid.arrange(a, b,  nrow=2, ncol=1)
dev.off()

### Figura 4 Nestling development of Tangara schrankii


### Load  morfonestling.csv

data <-read.table(file.choose(),header=T, sep=";", dec=",")
str(data)
data$dia <- as.numeric(data$dia)

tarsus <- data [1:266,]
wing   <- data [267:532,]
weight <- data [533:798,]


a <- ggplot(tarsus, aes(dia, med)) + 
  geom_jitter(shape=1, position=position_jitter(0.5), colour= "black",
              size = 2)+
  geom_smooth(method = "loess", color = "black")+
  fondo + ejestext +
  scale_x_continuous(breaks=seq(2, 16, by = 2))+
  labs(x="", y = "Tarsus length (mm)")+
  annotate(family= "serif",geom = "text", x = 0.6, y = 21.5, 
           label = "A", size = 5)

b <- ggplot(wing, aes(dia, med)) + 
  geom_jitter(shape=1, position=position_jitter(0.5), colour= "black",
              size = 2)+
  geom_smooth(method = "loess", color = "black")+
  fondo + ejestext +
  scale_x_continuous(breaks=seq(2, 16, by = 2))+
  labs(x="", y = "Wing length (mm)")+
  annotate(family= "serif",geom = "text", x = 0.6, y = 46, 
           label = "B", size = 5)

c <- ggplot(weight, aes(dia, med)) + 
  geom_jitter(shape=1, position=position_jitter(0.5), colour= "black",
              size = 2)+
  geom_smooth(method = "loess", color = "black")+
  fondo + ejestext +
  scale_x_continuous(breaks=seq(2, 16, by = 2))+
  labs(x="Day after hatching", y = "Weight (g)")+ 
  annotate(family= "serif",geom = "text", x = 0.6, y = 16.2, 
           label = "C", size = 5)

tiff("Morphonestling.tiff",width=15,height= 20 ,
     units="cm",res=400,compression="lzw")

grid.arrange(a, b, c, ncol=1)
dev.off()

### Figura 5

ejestext <- theme(axis.title.y = element_text(family="serif",vjust=1.2,size = 13),
                  axis.title.x = element_text(family="serif",vjust=-0.5,size = 12),
                  axis.text.x = element_text(family="serif",size = 9,lineheight = 0.9, vjust = 1),
                  axis.text.y = element_text(family="serif",size = 11,lineheight = 0.9, vjust = .5))

### feedingXhoraT_schrankii.csv

axhora<-read.table(file.choose(),header=T, sep=";", dec=",")
str(axhora)
axhora$Thora<-as.factor(axhora$Thora)

### Figura 5A 

a <- ggplot(axhora, aes(Thora, alimentacion)) + 
  ejestext + fondo +
  stat_boxplot(geom ='errorbar', size = .6, width=0.6) + 
  geom_boxplot()+
  scale_x_discrete("Time of day (hrs)",limits=c("5-6","6-7","7-8","8-9","9-10","10-11"
                               ,"11-12","12-13","13-14","14-15",
                               "15-16","16-17","17-18"))+
  scale_y_continuous("",breaks=seq(1, 12, by = 2))+
  theme(plot.subtitle = element_text(family="serif",size = 11,
                                     lineheight = 0.9, vjust = 1, hjust = 0.5))+
  annotate(family= "serif",geom = "text", x = 1, y = 10.5, 
           label = "A", size = 5)

#### 5B

### Load feedingXdia_T_schrankii.csv

alimentacionXdia<-read.csv(file.choose(), header=T, sep=";", dec=".")
str(alimentacionXdia)
alimentacionXdia$dia<-as.factor(alimentacionXdia$dia)

b <- ggplot(alimentacionXdia, aes(dia, alimentacion)) + 
  ejestext + fondo +
  stat_boxplot(geom ='errorbar', size = .6, width=0.6) + 
  geom_boxplot()+
  labs(x = "Days after hatching", y ="")+
  annotate(family= "serif",geom = "text", x = 1, y = 38, 
           label = "B", size = 5)

tiff("Feeding.tiff",width=16,height=13,
     units="cm",res=400,compression="lzw")

leftitle = textGrob("Number of feeding trips", gp=gpar(fontfamily="serif"), 
                    vjust = 2, rot = 90)
grid.arrange(a, b, left=leftitle,  nrow=2, ncol=1)
dev.off()

####   statistical analysis ####

### Incubation analysis the next script was elaborated by Juan Pablo Gomez Juan Pablo Gomez gomez444@gmail.com

library(lme4)
library(betareg)

### Load Incubation_ts.csv

tsincu<-read.table(file.choose(),header=T, sep=";", dec=",")

### Load incubation_Ts2020.csv

tsincu1<-read.table(file.choose(),header=T, sep=";", dec=",")

### Data evaluation

viajes.bar	<-	tapply(tsincu$viajes,tsincu$estado,mean)
barplot(table(tsincu$viajes,tsincu$estado),space=0)
viajes.expect	<-	nrow(tsincu)*dpois(3:12,viajes.bar)
points((1:10)-0.5,viajes.expect,type="b",col="red",pch=19)

### Model 1

viajes.glm	<-	glm(viajes~estado-1,data=tsincu,family="poisson")
summary(viajes.glm)
### Null model
viajes.null	<-	glm(viajes~1,data=tsincu,family="poisson")
summary(viajes.null)
### Comparison of models 
BIC(viajes.glm)-BIC(viajes.null)
### Random effects by nest
viajes.glmer		<-	glmer(trips~state + (1|Nest),data=tsincu1,family="poisson")
viajes.null.mer	<-	glmer(trips~1+(1|Nest),data=tsincu1,family="poisson")
BIC(viajes.glmer)-BIC(viajes.null.mer)
### Prediction of the trips per stage 
predict(viajes.null.mer,re.form=NA,type="response",newdata=data.frame(state=c("Early","Meddle","Final")))
### confidence interval of the models
exp(confint(viajes.null.mer,parm="(Intercept)"))

### Normal regression with random effects for attentiveness and stages 
logit.on		<-	qlogis(on)
hist(logit.on)
onbout.lmer	<-	lmer(logit.on~state+(1|tsincu1$Nest))
onbout.null.mer	<-	lmer(logit.on~1+(1|tsincu1$Nest))
BIC(onbout.lmer) - BIC(onbout.null.mer)
### Prediction of the attentiveness by day 
plogis(predict(onbout.null.mer,re.form=NA,newdata=data.frame(state=c("Early","Meddle","Final"))))
### confidence interval of the models
plogis(confint(onbout.null.mer,parm="(Intercept)"))
# 2.5 %    97.5 %

### The next code was used by estimate the nestling growth, 
### for calculate the growth rate change the data in "w" for 
### each chick from Nestling_GR.

w<-c(2.85,4.00,5.20,7.05,8.50,10.30,11.60,13.80,14.40,14.40,14.35,15.00,13.45,14.50)## T24MAl09
no<-w[1]
len<-length(w)-1
# Funcion logistica
exact.logistic<- function(k,b,no,dt,len,plot.it="False"){
  
  times<- seq(from=0,to=len,by=dt);	
  n.t <- b/(1+ ((b-no)/no)*exp(-k*times));
  
  out <- cbind(times,n.t)
  if(plot.it=="TRUE"){
    plot(times,n.t, type="l", col="red", xlab="time", ylab="n(t)", main="Exact solution of the logistic equation")	}
  return(out)
}

#Funcion para calcular la probabilidad del modelo
ls.log<-function(theta,data){
  k=theta[1]
  b=theta[2]
  dt=1
  len=len
  no=no
  estimated<-exact.logistic(k,b,no,dt,len)
  dif<-(data-estimated)^2
  min.dif<-sum(dif)
  return(min.dif)
}
#Optimizacion para calcular los parametros que maximizan la probabilidad
Ml.reslts.log=optim(par=c(0.1,30),ls.log,method="Nelder-Mead",data=w)
k.hat.log<-Ml.reslts.log$par[1]
b.hat.log<-Ml.reslts.log$par[2]
loglike.ml.log<-log(Ml.reslts.log$value)
k.hat.log
b.hat.log
loglike.ml.log
#Figura de los datos observados vs. los esperados segun el modelo logisitico
plot(w,pch=19,xlab="Days",ylab="Weigth (gr)")
w.hat.log<-exact.logistic(k.hat.log,31.4,3.4,1,17)[,2]
points(w.hat.log,type="l",col="red")

#Funcion de gompertz
exact.gompertz<- function(k,b,no,dt,len,plot.it="False"){
  
  times<- seq(from=0,to=len,by=dt);	
  n.t <- b*exp(-no*exp(-k*times));
  
  out <- cbind(times,n.t)
  if(plot.it=="TRUE"){
    plot(times,n.t, type="l", col="red", xlab="time", ylab="n(t)", main="Exact solution of the logistic equation")	}
  return(out)
}
#Funcion para calcular la probabilidad del modelo
ls.gom<-function(theta,data){
  k=theta[1]
  b=theta[2]
  dt=1
  len=len
  no=no
  estimated<-exact.gompertz(k,b,no,dt,len)
  dif<-(data-estimated)^2
  min.dif<-sum(dif)
  return(min.dif)
}
Ml.reslts.gom=optim(par=c(0.1,30),ls.gom,method="Nelder-Mead",data=w)
k.hat.gom<-Ml.reslts.gom$par[1]
b.hat.gom<-Ml.reslts.gom$par[2]
loglike.ml.gom<-log(Ml.reslts.gom$value)
k.hat.gom
b.hat.gom
loglike.ml.gom
w.hat.gom<-exact.gompertz(k.hat.gom,b.hat.gom,3.4,1,17)[,2]
points(w.hat.gom,type="l",col="red",lty=2)

#Funcion de von bertalanffy
exact.bert<- function(k,b,dt,len,plot.it="False"){
  
  times<- seq(from=0,to=len,by=dt);	
  n.t <- (b*((1-exp(-k*times)^0.75)));
  
  out <- cbind(times,n.t)
  if(plot.it=="TRUE"){
    plot(times,n.t, type="l", col="red", xlab="time", ylab="n(t)", main="Exact solution of the logistic equation")	}
  return(out)
}
#Funcion para calcular la probabilidad del modelo
ls.bert<-function(theta,data){
  k=theta[1]
  b=theta[2]
  dt=1
  len=len
  no=no
  estimated<-exact.bert(k,b,dt,len)
  dif<-(data-estimated)^2
  min.dif<-sum(dif)
  return(min.dif)
}
Ml.reslts.bert=optim(par=c(0.1,30),ls.bert,method="Nelder-Mead",data=w)
k.hat.bert<-Ml.reslts.bert$par[1]
b.hat.bert<-Ml.reslts.bert$par[2]
loglike.ml.bert<-log(Ml.reslts.bert$value)
k.hat.bert
b.hat.bert
loglike.ml.bert
w.hat.bert<-exact.bert(k.hat.bert,b.hat.bert,1,17)[,2]
points(w.hat.bert,type="l",col="red",lty=3)

aic.log<-2*2-2*loglike.ml.log
aic.gom<-2*2-2*loglike.ml.gom
aic.bert<-2*2-2*loglike.ml.bert
k.hat<-c(k.hat.log,k.hat.gom,k.hat.bert)
b.hat<-c(b.hat.log,b.hat.gom,b.hat.bert)
loglike<-c(loglike.ml.log,loglike.ml.gom,loglike.ml.bert)
aic<-c(aic.log,aic.gom,aic.bert)
delta.aic<-min(c(aic.log,aic.gom,aic.bert))-c(aic.log,aic.gom,aic.bert)

results<-cbind(k.hat,b.hat,loglike,aic,delta.aic)
rownames(results)<-c("logistic","gompertz","vonBertalanffy")
results

### Feeding 

### Load Feeding_T_schrankii.csv

fd<-read.table(file.choose(),header=T, sep=";", dec=",")

### Feeding morning and afternoon
incuanova<-aov(lm(fd$feed~factor(fd$estado)))
summary(incuanova)

### Feeding throughout the nestling period 
incuanova<-aov(lm(fd$feed~factor(fd$hora)))
summary(incuanova)

### Nest survival was calculated with code created of Joseph A. LaManna joseph.lamanna@marquette.edu

### Load sup_T_schrankii.csv

data<-read.table(file.choose(),header=T, sep=";", dec=",")

nestfate=data[!is.na(data$Nest_Fate),]
nestfate$p.fate=nestfate$Nest_Fate
nestfate=subset(nestfate,nestfate$Nest_Fate!=2.2) #abandoned after laying, no predation
nestfate=subset(nestfate,nestfate$Nest_Fate!=2.4) #never occupied
nestfate=subset(nestfate,nestfate$Nest_Fate!=2.5) #not enough info
nestfate=subset(nestfate,nestfate$Nest_Fate!=2.6) # muerto en el nido 
dim(data)
dim(nestfate)

nestfate$days=nestfate$Total_Days
nestfate=subset(nestfate,nestfate$days>0)

nestfate$length=ceiling(nestfate$Total_Days)

nestfate.lay=subset(nestfate,nestfate$Lay_Days>0)
nestfate.inc=subset(nestfate,nestfate$Incubation_Days>0)
nestfate.nstl=subset(nestfate,nestfate$Nestling_Days>0)
nestfate.lay$p.fate=nestfate.lay$Lay_Fate
nestfate.lay$days=nestfate.lay$Lay_Days
nestfate.inc$p.fate=nestfate.inc$Incubation_Fate
nestfate.inc$days=nestfate.inc$Incubation_Days
nestfate.nstl$p.fate=nestfate.nstl$Nestling_Fate
nestfate.nstl$days=nestfate.nstl$Nestling_Days
nestfate.lay$stage=c("LAY")
nestfate.inc$stage=c("INC")
nestfate.nstl$stage=c("NSTL")
nestfate=rbind(nestfate.lay,nestfate.inc,nestfate.nstl)
nestfate=nestfate[order(nestfate$Nest_ID),]
nestfate$stage=as.factor(nestfate$stage)

head(nestfate)


##Assigning binomial coefficients to fate codes to estimate SURVIVAL!!!

nestfate$p.fate=as.numeric(as.character(nestfate$p.fate))#Convert factor values to numeric 
is.numeric(nestfate$p.fate)
nestfate$fate=nestfate$p.fate
nestfate$fate[nestfate$p.fate==1.0]=1
nestfate$fate[nestfate$p.fate==2.3]=0
is.factor(nestfate$Estacion)

##Run model to stimate survival of nest

M1C=glm(fate~1, data=nestfate,family=binomial(logexp(days=nestfate$days)))
summary(M1C)

# obtener el inverso del logistico, es decir la tasa diaria de supervivencia

inv.logit(summary(M1C)$coeff[1,1])

ci_intercept=inv.logit(summary(M1C)$coeff[1,1]+c(-1,1)*qnorm(0.975)*
                         summary(M1C)$coeff[1,2])
ci_intercept
ci_ma=inv.logit(summary(M1C)$coeff[2,1]+c(-1,1)*qnorm(0.975)*
                  summary(M1C)$coeff[2,2])
ci_ma

### Effect of height to the ground and survival  

nestfate=nestfate[!is.na(nestfate$Altura),]

M2C=glm(fate~Altura, data=nestfate,family=binomial(logexp(days=nestfate$days)))
summary(M2C)




