##2020

amt2020


#preliminary visualization and analysis
ppt2020 <- ggplot(data=amt2020, aes(x=DATE , y=PPT_AMT))+
  geom_line(lwd=0.3,colour ="darkblue")+
  labs(x="Month",y="Daily Precipitation Amount (mm)", colour= "percentile")+
  coord_cartesian(ylim = c(0,270),expand=0) +
  scale_x_datetime(date_breaks = "1 month",labels = date_format("%b"))
ppt2020

#percentile calculation 
P5  <- quantile(amt2020$PPT_AMT, p=c(0.05))
P10 <- quantile(amt2020$PPT_AMT, p=c(0.10))
P15 <- quantile(amt2020$PPT_AMT, p=c(0.15))
P20 <- quantile(amt2020$PPT_AMT, p=c(0.20))
P25 <- quantile(amt2020$PPT_AMT, p=c(0.25))
P30 <- quantile(amt2020$PPT_AMT, p=c(0.30))
P35 <- quantile(amt2020$PPT_AMT, p=c(0.35))
P40 <- quantile(amt2020$PPT_AMT, p=c(0.40))
P45 <- quantile(amt2020$PPT_AMT, p=c(0.45))
P50 <- quantile(amt2020$PPT_AMT, p=c(0.50))
P55 <- quantile(amt2020$PPT_AMT, p=c(0.55))
P60 <- quantile(amt2020$PPT_AMT, p=c(0.60))
P65 <- quantile(amt2020$PPT_AMT, p=c(0.65))
P70 <- quantile(amt2020$PPT_AMT, p=c(0.70))
P75 <- quantile(amt2020$PPT_AMT, p=c(0.75))
P80 <- quantile(amt2020$PPT_AMT, p=c(0.80))
P85 <- quantile(amt2020$PPT_AMT, p=c(0.85))
P90 <- quantile(amt2020$PPT_AMT, p=c(0.90))
P95 <- quantile(amt2020$PPT_AMT, p=c(0.95))
P100 <- quantile(amt2020$PPT_AMT, p=c(1))


P5
P10
P15
P20
P25
P30
P35
P40
P45
P50
P55
P60
P65
P70
P75
P80
P85
P90
P95
P100

##statistics

minimum <- min(amt2020$PPT_AMT)
maximum <- max(amt2020$PPT_AMT)
mean <- mean(amt2020$PPT_AMT)
med<- median(amt2020$PPT_AMT)
std<- sd(amt2020$PPT_AMT)


#percentile inseration into the series graph

h20 <- ppt2020 + geom_hline(aes(yintercept = P5, colour = "P5")) +
  geom_hline(aes(yintercept = P10, colour = "P10")) +
  #geom_hline(aes(yintercept = P15, colour = "P15")) +
  geom_hline(aes(yintercept = P20, colour = "P20")) +
  #geom_hline(aes(yintercept = P25, colour = "P25")) +
  geom_hline(aes(yintercept = P30, colour = "P30")) +
  #geom_hline(aes(yintercept = P35, colour = "P35")) +
  geom_hline(aes(yintercept = P40, colour = "P40")) +
  #geom_hline(aes(yintercept = P45, colour = "P45")) +
  geom_hline(aes(yintercept = P50, colour = "P50")) +
  #geom_hline(aes(yintercept = P55, colour = "P55")) +
  geom_hline(aes(yintercept = P60, colour = "P60")) +
  #geom_hline(aes(yintercept = P65, colour = "P65")) +
  geom_hline(aes(yintercept = P70, colour = "P70")) +
  #geom_hline(aes(yintercept = P75, colour = "P75")) +
  geom_hline(aes(yintercept = P80, colour = "P80")) +
  #geom_hline(aes(yintercept = P85, colour = "P85")) +
  geom_hline(aes(yintercept = P90, colour = "P90")) +
  geom_hline(aes(yintercept = P95, colour = "P95")) +
  geom_hline(aes(yintercept = P100, colour = "P100")) + 
  scale_colour_manual(values =c('darkgrey','black', 'red','orange', 'deeppink','deepskyblue',
                                'green', 'blue4','purple','bisque','cyan','darkseagreen4'))
h20

#labels= c("P5", "P10", "P20", "P30", "P40", "P50", "P60", "P70", "P80", "P90", "P95", "P100"))


#aesthetic for the series graph
h20a <- h20 + 
  xlab("Time (days)") + 
  ylab("Daily Precipitation Amount (mm)")+theme_bw()+
  ylim(c(0,300))+
  #ggtitle("Movie Budget Distribution")+
  theme(axis.title.x = element_text(colour="black", size=10),
        axis.title.y = element_text(colour="black", size=10),
        axis.text.x=element_text(size=10, colour="black"),
        axis.text.y=element_text(size=10, colour="black"),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        legend.position=c(1,1),
        legend.justification=c(1.19,1.1),
        legend.key.size=unit(.4,"cm"),
        legend.key.height = unit(.2, 'cm'))
h20a


#cumulative percentage
data2020 <- count(amt2020, 'PPT_AMT')
data2020

cum2020 <- cumsum(data2020$freq)
cum2020

cumperc_2020 <- cum2020/nrow(amt2020)*100
cumperc_2020

amt2020 <- cbind(data2020, cumperc_2020)
amt2020


#computation of histogram
p <- ggplot(data = amt2020,aes(x=PPT_AMT)) 
p <- p + geom_histogram(aes(x=PPT_AMT),binwidth = 18,fill= 'grey', colour= 'black') 
p <- p + geom_line(aes(y=cumperc_2020*0.18), colour  = "red")+ 
  coord_cartesian(ylim = c(0,19), expand =0)  
p <- p + scale_y_continuous("Frequency", sec.axis = sec_axis(~./0.18, name = "Cumulative Percentage"))
p



#aesthetics for the cumulative percentage graph
k20 <- p +theme_bw()+ theme(
  axis.title.y.left=element_text(color="black", size=10),
  axis.text.y.left=element_text(color="black", size=10),
  axis.title.y.right=element_text(color="red", size=10),
  axis.text.y.right=element_text(color="red", size=10),
  axis.title.x = element_text(colour="black", size=10),
  axis.text.x = element_text(size=10, colour="black")) +
  xlab("Precipitation Amount (mm)")

k20



#####combine both graphs + insertation
fig20<- h20a +
  inset_element(k20, left = 0.001, bottom = 0.50, right = 0.5, top = 0.99)

fig20


ggsave("C:\\Users\\chuka\\OneDrive\\Desktop\\demo2020.jpeg",
       width = 32, height = 22, dpi = 500, units = "cm", device='jpeg')




#figure <- ggarrange(h20a, k20,
ncol = 2, nrow = 2)
#figure