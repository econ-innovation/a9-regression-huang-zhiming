#Rosokha, Yaroslav, and Younge, Kenneth, (2020) "Motivating Innovation: 
The Effect of Loss Aversion on the Willingness to Persist." Review of 
Economics and Statistics 102:3, 569–582.

#复现其中图表部分内容

library(Cairo)
library(ggplot2)
library(plyr)

dataIn=read.csv("data\\04-calculated_net_benefits.csv")
dataIn$Incentives=factor(dataIn$Incentives)
dataIn$Incentives=revalue(dataIn$Incentives, c("0"="Gaines"))
dataIn$Incentives=revalue(dataIn$Incentives, c("2"="Losses"))

dataIn$Structures=factor(dataIn$Structures)
dataIn$Structures=revalue(dataIn$Structures, c("0"="Baseline"))
dataIn$Structures=revalue(dataIn$Structures, c("3"="Breakthrough"))


cairo_pdf(filename="results\\05-figure5.pdf",width=7,height=5,family="CMU 
Serif")
ggplot(dataIn,aes(x=moves_taken,y=netBenefit))+
  geom_line(aes(group = eid),position = position_jitter(width = 3, height 
= 3),color=gray.colors(1, start =.6, end = .6),lwd=0.25)+
  theme(legend.text = element_text(size = 11), axis.title= 
element_text(size = 11), legend.title= element_text(size = 11),axis.text= 
element_text(size = 11))+
  theme_minimal()+
  stat_smooth(color="black",alpha=1,lwd=1.75)+
  theme(legend.position = 
"right",legend.direction="vertical",legend.box="vertical")+
  guides(size=FALSE,alpha=FALSE,color=FALSE)+
  theme(text=element_text(size=11))+
  theme(legend.key.height=unit(2,"cm"),legend.text = 
element_text(colour="black", size = 11),legend.title = 
element_text(colour="black", size = 11,face="bold"))+
  theme(strip.text.x = element_text(size=11,face="bold"), strip.text.y = 
element_text(size=11,face="bold"))+
  theme(axis.text= element_text(size = 11), axis.title= element_text(size 
= 11),axis.title.y=element_text(vjust=.5),title= element_text(size = 20))+ 
  labs(y = "Net Benefit", x="Move Number")+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))+
  facet_grid(Incentives~Structures)
dev.off()

