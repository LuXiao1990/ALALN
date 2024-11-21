# library
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggh4x)
library(ggtext)
library(openxlsx)
library(car)
library(nlme)
library(kableExtra)
library(Rmisc)
library(MuMIn)
library(tibble)
library(patchwork)
library(ggsci)
library(corrplot)
library(psych)
library(Cairo)
library(showtext)
library(ggpubr) # Combine ggplots
library(tidyr) #regexpr() and gather()
library(viridis) # colour solution
library(scales)


# Data preparation
data <- read.xlsx("ALAN_data.xlsx", sheet="Sheet1")

## Total biomass
dat1 <- data %>% as.data.frame() %>% filter(!is.na(Total_biomass)) %>% 
  droplevels()

## Root mass fraction
dat2 <- data %>% as.data.frame() %>% filter(!is.na(RMF)) %>% 
  droplevels()

## photosynthesis
dat3 <- data %>%filter(!is.na(mydata$Photo)) %>%
  droplevels()

## Tolerance
dat4 <- data %>% filter(!is.na(mydata$Tolerance2)) %>% 
  filter(Herbivory=="Herb+")%>%droplevels()

## Relative growth rate of larvae

data1 <- read.xlsx("relative_growth_rate_data.xlsx", sheet="Sheet1")

night <- data1 %>% as.data.frame() %>% filter(time=="night") %>% 
  filter(!is.na(rgc)) %>% droplevels()


# Total biomass

u<-gl(1,1,length(dat1$Total_biomass))
vf1 <- varIdent(form=~1|species)

# Four-way interaction
lm1 <- lme(log(Total_biomass) ~ scale(Initial_height)+Status*ALAN*
             Herbivory* Nutrient,
           random=list(u=pdBlocked(list(pdIdent(form=~family-1),
                                        pdIdent(form=~species-1),
                                        pdIdent(form=~Compartment-1)
           ))),
           weights=varIdent(form=~1|species),
           control=list(maxIter=1000,msMaxIter=1000,niterEM=1000),
           data=dat1,method="ML")

Four <- drop1(lm1,test="Chisq")[-c(1,2),]

## Three-way interaction
ThreeRef <- update(lm1,.~.- Herbivory:Nutrient:ALAN:Status) 

Three<- drop1(ThreeRef,test="Chisq")[-c(1,2),]

## Two-way interaction
TwoRef <- update(ThreeRef,.~.- (Herbivory:Nutrient:ALAN+
                                  Herbivory:Nutrient:Status+
                                  Herbivory:ALAN:Status+
                                  Nutrient:ALAN:Status))

Two <- drop1(TwoRef,test="Chisq")[-c(1,2),]

## Main effect
MainRef <- update(TwoRef, .~. -(Herbivory:ALAN + Herbivory:Nutrient +
                                  Herbivory:Status + ALAN:Nutrient+ ALAN:Status +
                                  Nutrient:Status))

Main <- drop1(MainRef,test="Chisq")[-c(1,2),]

#Covariance
CovarRef<- update(MainRef, .~. -(Herbivory+ALAN+Nutrient+Status))

Covar<-drop1(CovarRef,test="Chisq")[-1,]

table<-rbind(Covar,Main,Two,Three,Four)
table$" "=ifelse(table$`Pr(>Chi)`<0.05,"*",
                 ifelse(table$`Pr(>Chi)`<0.1,"..",""))#add significant levels
kable(table,digits=4)%>% kable_styling(full_width = T)

r1 <- r.squaredGLMM(lm1)

r1

# Root mass fraction

u<-gl(1,1,length(dat2$RMF))
vf1 <- varIdent(form=~1|species)

# Four-way interaction
lm2<- lme(log(RMF) ~ scale(Initial_height)+Status*ALAN*
            Herbivory* Nutrient,
          random=list(u=pdBlocked(list(pdIdent(form=~family-1),
                                       pdIdent(form=~species-1),
                                       pdIdent(form=~Region-1)
          ))),
          weights=varIdent(form=~1|species),
          control=list(maxIter=1000,msMaxIter=1000,niterEM=1000),
          data=dat2,method="ML")

Four <- drop1(lm2,test="Chisq")[-c(1,2),]

## Three-way interaction
ThreeRef <- update(lm2,.~.- Herbivory:Nutrient:ALAN:Status) 

Three<- drop1(ThreeRef,test="Chisq")[-c(1,2),]

## Two-way interaction
TwoRef <- update(ThreeRef,.~.- (Herbivory:Nutrient:ALAN+
                                  Herbivory:Nutrient:Status+
                                  Herbivory:ALAN:Status+
                                  Nutrient:ALAN:Status))

Two <- drop1(TwoRef,test="Chisq")[-c(1,2),]

## Main effect
MainRef <- update(TwoRef, .~. -(Herbivory:ALAN + Herbivory:Nutrient +
                                  Herbivory:Status + ALAN:Nutrient+ ALAN:Status +
                                  Nutrient:Status))

Main <- drop1(MainRef,test="Chisq")[-c(1,2),]

#Covariance
CovarRef<- update(MainRef, .~. -(Herbivory+ALAN+Nutrient+Status))

Covar<-drop1(CovarRef,test="Chisq")[-1,]


table<-rbind(Covar,Main,Two,Three,Four)
table$" "=ifelse(table$`Pr(>Chi)`<0.05,"*",
                 ifelse(table$`Pr(>Chi)`<0.1,"..",""))#add significant levels
kable(table,digits=4)%>% kable_styling(full_width = T)

r2 <- r.squaredGLMM(lm2)

r2

# Net Photosynthesis

u<-gl(1,1,length(dat3$Specific_Leaf_Area))
vf1 <- varIdent(form=~1|species)

# Four-way interaction
lm3 <- lme((Photo)^1/3 ~ scale(Initial_height)+Status*ALAN*
             Herbivory* Nutrient,
           random=list(u=pdBlocked(list(pdIdent(form=~family-1),
                                        pdIdent(form=~species-1),
                                        pdIdent(form=~Region-1)
           ))),
           control=list(maxIter=1000,msMaxIter=1000,niterEM=1000),
           data=dat3, method="ML")

Four <- drop1(lm3,test="Chisq")[-c(1,2),]

## Three-way interaction
ThreeRef <- update(lm3,.~.- Herbivory:Nutrient:ALAN:Status) 

Three<- drop1(ThreeRef,test="Chisq")[-c(1,2),]

## Two-way interaction
TwoRef <- update(ThreeRef,.~.- (Herbivory:Nutrient:ALAN+
                                  Herbivory:Nutrient:Status+
                                  Herbivory:ALAN:Status+
                                  Nutrient:ALAN:Status))

Two <- drop1(TwoRef,test="Chisq")[-c(1,2),]

## Main effect
MainRef <- update(TwoRef, .~. -(Herbivory:ALAN + Herbivory:Nutrient +
                                  Herbivory:Status + ALAN:Nutrient+ ALAN:Status +
                                  Nutrient:Status))

Main <- drop1(MainRef,test="Chisq")[-c(1,2),]

#Covariance
CovarRef<- update(MainRef, .~. -(Herbivory+ALAN+Nutrient+Status))

Covar<-drop1(CovarRef,test="Chisq")[-1,]


table<-rbind(Covar,Main,Two,Three,Four)
table$" "=ifelse(table$`Pr(>Chi)`<0.05,"*",
                 ifelse(table$`Pr(>Chi)`<0.1,"..",""))#add significant levels
kable(table,digits=5)%>% kable_styling(full_width = T)

r3<-r.squaredGLMM(lm3)
r3

# Tolerance score
u<-gl(1,1,length(dat4$Tolerance2))
vf1 <- varIdent(form=~1|family)

# Three-way interaction
lm4 <- lme(sqrt(Tolerance) ~ scale(Initial_height_ratio)+Status*ALAN*Nutrient,
           random=list(u=pdBlocked(list(pdIdent(form=~family-1),
                                        pdIdent(form=~species-1),
                                        pdIdent(form=~Region-1)
           ))),
           weights = vf1,
           data=dat4, method="ML",
           control=list(maxIter=1000,msMaxIter=1000,niterEM=1000))

Three <- drop1(lm4,test="Chisq")[-c(1,2),]

## Two-way interaction
TwoRef <- update(lm4,.~.- Nutrient:ALAN:Status) 

Two<- drop1(TwoRef,test="Chisq")[-c(1,2),]

## Main effect
MainRef <- update(TwoRef, .~. -(ALAN:Nutrient+ ALAN:Status +
                                  Nutrient:Status))

Main <- drop1(MainRef,test="Chisq")[-c(1,2),]

#Covariance
CovarRef<- update(MainRef, .~. -(ALAN+Nutrient+Status))

Covar<-drop1(CovarRef,test="Chisq")[-1,]


table<-rbind(Covar,Main,Two,Three)
table$" "=ifelse(table$`Pr(>Chi)`<0.05,"*",
                 ifelse(table$`Pr(>Chi)`<0.1,"..",""))#add significant levels
kable(table,digits=4)%>% kable_styling(full_width = T)

r4<-r.squaredGLMM(lm4)
r4

# Relative growth rate of larvae

u<-gl(1,1,length(night$rgc))
vf2 <- varIdent(form=~1|Region)

## Three-way interaction
lm5 <- lme(log(rgc)~scale(Initial_insect_weight)+Status*ALAN*Nutrient,
           random=list(u=pdBlocked(list(pdIdent(form=~family-1),
                                        pdIdent(form=~species-1),
                                        pdIdent(form=~Region-1)
           ))),
           weights = vf2,
           control=list(maxIter=400,msMaxIter=300,niterEM=300),
           data=night, method="ML")

Three <- drop1(lm5,test="Chisq")[-c(1,2),]

## Two-way interaction
TwoRef <- update(lm5,.~.-Status:ALAN:Nutrient) 

Two<- drop1(TwoRef,test="Chisq")[-c(1,2),]

## Main effect
MainRef <- update(TwoRef,.~.- (Nutrient:ALAN+
                                 Nutrient:Status+
                                 ALAN:Status))


Main <- drop1(MainRef,test="Chisq")[-c(1,2),]

#Covariance
CovarRef<- update(MainRef, .~. -(ALAN+Nutrient+Status))

Covar<-drop1(CovarRef,test="Chisq")[-1,]

table<-rbind(Covar,Main,Two,Three)
table$" "=ifelse(table$`Pr(>Chi)`<0.05,"*",
                 ifelse(table$`Pr(>Chi)`<0.1,"..",""))#add significant levels
kable(table,digits=4)%>% kable_styling(full_width = T)

r5<-r.squaredGLMM(lm5)
r5

# Figure( Fig. 1, Fig. 2, Fig. 3,  Fig. S2, and Fig. S3)

## Theme
theme1 <-theme_classic()+
  theme(panel.spacing.x = unit(0, "lines"),strip.placement = "outside",
        strip.background = element_blank(),
        strip.text =element_text(size=11,color='black'))+
  theme(axis.text=element_text(size=11,colour="black"),
        axis.title = element_text(size=11,color='black'))+
  theme(axis.line = element_line(linewidth=0.5))+
  theme(axis.ticks = element_line(linewidth=0.5))+
  theme(axis.ticks.length=unit(0.15, "cm")) +
  theme(legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent",colour = NA),
        legend.text = element_text(size=10,color = "black"))

theme2 <-theme_classic()+
  theme(axis.text=element_text(size=11,colour="black"),
        axis.title = element_text(size=11,color='black'))+
  theme(axis.line = element_line(linewidth=0.5))+
  theme(axis.ticks = element_line(linewidth=0.5))+
  theme(axis.ticks.length=unit(0.15, "cm")) +
  theme(legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent",colour = NA),
        legend.text = element_text(size=10,color = "black"))

## Fig 1
###p3
Fig3 <- summarySE(dat1, measurevar="Total_biomass", 
                  groupvars=c("Herbivory","ALAN","Status"))

Fig3_1 <- summarySE(dat1, measurevar="Total_biomass", 
                    groupvars=c("Herbivory"))


Fig3$ALAN <-ordered(Fig3$ALAN,levels=c("No-ALAN","ALAN"))

Fig3$Herbivory <-ordered(Fig3$Herbivory,levels=c("Herb-","Herb+"))


p3 <- ggplot(Fig3, aes(x=ALAN,y=Total_biomass,color=Status)) +
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group=Status), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=Total_biomass-se, ymax=Total_biomass+se), width=0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5)) +
  labs(x=NULL, y = "Total biomass (g)")+
  ylim(0.1,5)+
  ## facet
  facet_wrap(~Herbivory,strip.position = "bottom")+
  theme1+
  theme(legend.position = c(0,0.15),legend.justification.inside = c(0,1),
        legend.key.height = unit(4, "mm"))

### p7
Fig7 <- summarySE(dat2, measurevar="RMF",   
                  groupvars=c("Herbivory","ALAN","Nutrient"))

Fig7$ALAN <-ordered(Fig7$ALAN,levels=c("No-ALAN","ALAN"))
Fig7$Herbivory <-ordered(Fig7$Herbivory,levels=c("Herb-","Herb+"))

p7 <- ggplot(Fig7, aes(x=ALAN,y=RMF,color=Nutrient)) +
  scale_color_manual(values = c("#018A67","#F3a332"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group=Nutrient), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=RMF-se, ymax=RMF+se), width=0,position =   
                  position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5)) +
  labs(x=NULL, y = "Root mass fraction")+
  ylim(0.05,0.3)+
  ## facet
  facet_wrap(~Herbivory,strip.position = "bottom")+
  theme1+
  theme(legend.position = c(1,0.15),legend.justification.inside = c(1,1),
        legend.key.height = unit(4, "mm"))


Fig_1 <- p3+p7

cairo_pdf(file="Fig.1.pdf",width =18/2.54, height = 10/2.54)

print(Fig_1)

dev.off()


## Fig. 2
### p8
Fig8 <- summarySE(dat3, measurevar="Photo", groupvars=c("Status","ALAN"))

Fig8_1 <- summarySE(dat3, measurevar="Photo", groupvars=c("ALAN"))


Fig8$ALAN <-ordered(Fig8$ALAN,levels=c("No-ALAN","ALAN"))

p8 <- ggplot(Fig8, aes(x=ALAN,y=Photo,color=Status))+
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group= Status), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=Photo-se, ymax=Photo+se), width=0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5))+
  labs(x=NULL, 
       y = expression(Net~photosynthetic~rate~(μmol~CO[2]~m^-2~s^-1)))+
  ylim(-3,4)

p8 <- p8+theme2+
  theme(legend.position = c(1, 0.15),legend.justification.inside = c(1,1),
        legend.key.height = unit(4, "mm"))

cairo_pdf(file="Fig.2.pdf",width =10/2.54, height = 10/2.54)

print(p8)

dev.off()

## Fig.3

### p10
Fig10 <- summarySE(dat4, measurevar="Tolerance2", groupvars=c("Status","ALAN"))

Fig10 $ALAN <-ordered(Fig10$ALAN,levels=c("No-ALAN","ALAN"))

p10 <- ggplot(Fig10, aes(x=ALAN,y=Tolerance2,color=Status))+
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group= Status), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=Tolerance2-se, ymax=Tolerance2+se), 
                width=0,position = position_dodge(width = 0.5),linewidth=0.5) +
  geom_point(size = 2,position = position_dodge(width = 0.5))+
  labs(x=NULL, y = "Tolerance score")+
  theme2+
  theme(legend.position = c(1,0.22),legend.justification.inside = c(1,1),
        legend.key.height = unit(4, "mm"))

###p11
Fig11 <- summarySE(night, measurevar="rgc", 
                   groupvars=c("ALAN","Status"))

Fig11$ALAN <-ordered(Fig11$ALAN,levels=c("No-ALAN","ALAN"))

p11 <- ggplot(Fig11, aes(x=ALAN,y=rgc,color=Status))+
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group= Status), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=rgc-se, ymax=rgc+se), 
                width=0,position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5))+
  labs(x=NULL, y = "Larval growth rate(g/hourly)")+
  theme2+
  ylim(0.05,0.25)+
  theme(legend.position = "none")

### p12
Fig12 <- summarySE(night, measurevar="rgc", 
                   groupvars=c("Nutrient"))

Fig12$Nutrient <-ordered(Fig12$Nutrient,levels=c("Low-nutrient","High-nutrient"))


p12 <- ggplot(Fig12, aes(x=Nutrient,y=rgc,color=Nutrient))+
  scale_color_manual(values = c("#018A67","#F3a332"))+
  geom_errorbar(aes(ymin=rgc-se, ymax=rgc+se), 
                width=0,position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5))+
  labs(x=NULL, y = "Larval growth rate(g/hourly)")+
  theme2+
  ylim(0.1,0.2)+
  theme(legend.position = "none")

Fig_3 <- p10+p11+p12

cairo_pdf(file="Fig.3.pdf",width =21/2.54, height = 7/2.54)

print(Fig_3)

dev.off()


## Fig. S2
###p1_1
Fig1_1 <- summarySE(dat1, measurevar="Total_biomass", 
                    groupvars=c("Herbivory"))

p1_1 <- ggplot(Fig1_1, aes(x=Herbivory,y=Total_biomass,color=Herbivory))+
  scale_color_manual(values = c("#8A2BE2", "#00008B"))+
  geom_errorbar(aes(ymin=Total_biomass-se, ymax=Total_biomass+se), 
                width=0,position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5))+
  labs(x=NULL, y = "Total biomass (g)")+
  theme_bw()+ 
  #remove grid lines and border
  theme(panel.background = element_rect(fill = "transparent",colour = 'black'),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = 'white'))+
  #remove title of legend
  theme(legend.position = "none")+  
  ## font
  theme(axis.text.x = element_text(size=11,color="black"),
        axis.text.y = element_text(size=11,color="black"),
        axis.title.y.left =element_text(size=11,,color="black"))

###p1
Fig1 <- summarySE(dat1, measurevar="Total_biomass", 
                  groupvars=c("Herbivory","Status"))

Fig1$Herbivory <-ordered(Fig1$Herbivory,levels=c("Herb-","Herb+"))

p1 <- ggplot(Fig1, aes(x=Herbivory,y=Total_biomass,color=Status)) +
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group=Status), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=Total_biomass-se, ymax=Total_biomass+se), width=0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5)) +
  labs(x=NULL, y = "Total biomass (g)")+
  # theme
  theme_bw()+ 
  ##remove grid lines and border
  theme(panel.background = element_rect(fill = "transparent",colour = 'black'),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = 'white'))+
  ##remove title of legend
  theme(legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent",colour = NA),
        legend.text = element_text(size=10,color = "black"))+
  ## font
  theme(axis.text.x = element_text(size=11,color="black"),
        axis.text.y = element_text(size=11,color="black"),
        axis.title.y.left =element_text(size=11,,color="black"))+
  theme(legend.position = c(0,0.3),legend.justification.inside = c(0,1),
        legend.key.height = unit(4, "mm"))

###p2_1
Fig2_1 <- summarySE(dat1, measurevar="Total_biomass", 
                    groupvars=c("Nutrient"))

Fig2_1$Nutrient <-ordered(Fig2_1$Nutrient,levels=c("Low-nutrient","High-nutrient"))

p2_1 <- ggplot(Fig2_1, aes(x=Nutrient,y=Total_biomass,color=Nutrient))+
  scale_color_manual(values = c("#018A67","#F3a332"))+
  geom_errorbar(aes(ymin=Total_biomass-se, ymax=Total_biomass+se), 
                width=0,position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5))+
  labs(x=NULL, y = "Total biomass (g)")+
  theme_bw()+ 
  #remove grid lines and border
  theme(panel.background = element_rect(fill = "transparent",colour = 'black'),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = 'white'))+
  #remove title of legend
  theme(legend.position = "none")+  
  ## font
  theme(axis.text.x = element_text(size=11,color="black"),
        axis.text.y = element_text(size=11,color="black"),
        axis.title.y.left =element_text(size=11,,color="black"))

### p2
Fig2 <- summarySE(dat1, measurevar="Total_biomass", 
                  groupvars=c("Nutrient","Status"))

Fig2$Nutrient <-ordered(Fig2$Nutrient,levels=c("Low-nutrient","High-nutrient"))

p2 <- ggplot(Fig2, aes(x=Nutrient,y=Total_biomass,color=Status)) +
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group=Status), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=Total_biomass-se, ymax=Total_biomass+se), width=0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5)) +
  labs(x=NULL, y = "Total biomass (g)")+
  theme_bw()+ 
  #remove grid lines and border
  theme(panel.background = element_rect(fill = "transparent",colour = 'black'),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = 'white'))+
  #remove title of legend
  theme(legend.position = "none")+
  ## font
  theme(axis.text.x = element_text(size=11,color="black"),
        axis.text.y = element_text(size=11,color="black"),
        axis.title.y.left =element_text(size=11,,color="black"))

### p4_1
Fig4_1 <- summarySE(dat2, measurevar="RMF",   
                    groupvars=c("Herbivory"))

p4_1 <- ggplot(Fig4_1, aes(x=Herbivory,y=RMF,color=Herbivory))+
  scale_color_manual(values = c("#8A2BE2", "#00008B"))+
  geom_errorbar(aes(ymin=RMF-se, ymax=RMF+se), 
                width=0,position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5))+
  labs(x=NULL, y = "Root mass fraction")+
  theme_bw()+ 
  #remove grid lines and border
  theme(panel.background = element_rect(fill = "transparent",colour = 'black'),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = 'white'))+
  #remove title of legend
  theme(legend.position = "none")+  
  ## font
  theme(axis.text.x = element_text(size=11,color="black"),
        axis.text.y = element_text(size=11,color="black"),
        axis.title.y.left =element_text(size=11,,color="black"))

###p4
Fig4 <- summarySE(dat2, measurevar="RMF",   
                  groupvars=c("Herbivory","Status"))

Fig4$Herbivory <-ordered(Fig4$Herbivory,levels=c("Herb-","Herb+"))

p4 <- ggplot(Fig4, aes(x=Herbivory,y=RMF,color=Status)) +
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group=Status), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=RMF-se, ymax=RMF+se), width=0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5)) +
  labs(x=NULL, y = "Root mass fraction")+
  theme_bw()+ 
  #remove grid lines and border
  theme(panel.background = element_rect(fill = "transparent",colour = 'black'),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = 'white'))+
  #remove title of legend
  theme(legend.position = "none")+  
  ## font
  theme(axis.text.x = element_text(size=11,color="black"),
        axis.text.y = element_text(size=11,color="black"),
        axis.title.y.left =element_text(size=11,,color="black"))

### p5_1
Fig5_1 <- summarySE(dat2, measurevar="RMF",   
                    groupvars=c("Nutrient"))

Fig5_1$Nutrient <-ordered(Fig5_1$Nutrient,levels=c("Low-nutrient","High-nutrient"))

p5_1 <- ggplot(Fig5_1, aes(x=Nutrient,y=RMF,color=Nutrient))+
  scale_color_manual(values = c("#018A67","#F3a332"))+
  geom_errorbar(aes(ymin=RMF-se, ymax=RMF+se), 
                width=0,position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5))+
  labs(x=NULL, y = "Root mass fraction")+
  theme_bw()+ 
  #remove grid lines and border
  theme(panel.background = element_rect(fill = "transparent",colour = 'black'),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = 'white'))+
  #remove title of legend
  theme(legend.position = "none")+  
  ## font
  theme(axis.text.x = element_text(size=11,color="black"),
        axis.text.y = element_text(size=11,color="black"),
        axis.title.y.left =element_text(size=11,,color="black"))

###p5
Fig5 <- summarySE(dat2, measurevar="RMF",   
                  groupvars=c("Nutrient","Status"))


Fig5$Nutrient <-ordered(Fig5$Nutrient,levels=c("Low-nutrient","High-nutrient"))


p5 <- ggplot(Fig5, aes(x=Nutrient,y=RMF,color=Status)) +
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group=Status), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=RMF-se, ymax=RMF+se), width=0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5)) +
  labs(x=NULL, y = "Root mass fraction")+
  theme_bw()+ 
  #remove grid lines and border
  theme(panel.background = element_rect(fill = "transparent",colour = 'black'),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = 'white'))+
  #remove title of legend
  theme(legend.position = "none")+  
  ## font
  theme(axis.text.x = element_text(size=11,color="black"),
        axis.text.y = element_text(size=11,color="black"),
        axis.title.y.left =element_text(size=11,,color="black"))

### p6
Fig6 <- summarySE(dat2, measurevar="RMF",   
                  groupvars=c("ALAN","Status"))

Fig6$ALAN <-ordered(Fig6$ALAN,levels=c("No-ALAN","ALAN"))

p6 <- ggplot(Fig6, aes(x=ALAN,y=RMF,color=Status)) +
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_line(colour="darkgrey",linetype="dashed", linewidth=0.5,
            aes(group=Status), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=RMF-se, ymax=RMF+se), width=0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 2,position = position_dodge(width = 0.5)) +
  labs(x=NULL, y = "Root mass fraction")+
  theme_bw()+ 
  #remove grid lines and border
  theme(panel.background = element_rect(fill = "transparent",colour = 'black'),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = 'white'))+
  #remove title of legend
  theme(legend.position = "none")+  
  ## font
  theme(axis.text.x = element_text(size=11,color="black"),
        axis.text.y = element_text(size=11,color="black"),
        axis.title.y.left =element_text(size=11,color="black"))

Fig_S2 <- p1_1+p1+p2_1+p2+p4_1+p5_1+p6+p4+p5

cairo_pdf(file="Fig.S2.pdf",width =21/2.54, height = 15/2.54)

print(Fig_S2)

dev.off()

## Fig.S3
Fig9 <- summarySE(dat3, measurevar="Photo", 
                  groupvars=c("Herbivory","Nutrient","ALAN","Status"))

Fig9$ALAN <-ordered(Fig9$ALAN,levels=c("No-ALAN","ALAN"))
Fig9$Nutrient <-ordered(Fig9$Nutrient,levels=c("Low-nutrient","High-nutrient"))
Fig9$Herbivory <-ordered(Fig9$Herbivory,levels=c("Herb-","Herb+"))

### p9_1
Fig9_1 <- filter(Fig9,Herbivory=="Herb-")

p9_1 <- ggplot(Fig9_1, aes(x=ALAN, y=Photo,color=Status)) +
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_point(size = 2,position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin=Photo-se, ymax=Photo+se), width=0,
                position = position_dodge(width = 0.5)) +
  facet_grid(~Nutrient) +
  labs(x=NULL, y = expression(Net~photosynthetic~rate~(μmol~CO[2]~m^-2~s^-1)))+
  facet_wrap(~Nutrient,strip.position = "bottom")+
  ylim(-4,5)+
  theme1+
  theme(legend.position = c(0,0.15),legend.justification.inside = c(0,1),
        legend.key.height = unit(4, "mm"))+
  theme(plot.margin = margin(0,0.2,0,0))

###p9_2
Fig9_2 <- filter(Fig9,Herbivory=="Herb+")

p9_2 <- ggplot(Fig9_2, aes(x=ALAN, y=Photo,color=Status)) +
  scale_color_manual(values = c("#DE582B","#1868B2"))+
  geom_point(size = 2,position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin=Photo-se, ymax=Photo+se), width=0,
                position = position_dodge(width = 0.5)) +
  facet_grid(~Nutrient) +
  labs(x=NULL, y =NULL)+
  facet_wrap(~Nutrient,strip.position = "bottom")+
  ylim(-4,5)+
  theme1+
  theme(axis.text.y=element_blank())+
  theme(legend.position = "none")+
  theme(plot.margin = margin(0,0,0,0))

Fig_S3 <- p9_1+p9_2

cairo_pdf(file="Fig.S3.pdf",width =18/2.54, height = 10/2.54)

print(Fig_S3)

dev.off()