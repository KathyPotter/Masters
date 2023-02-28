library(lme4)
library(ggplot2)

data2 <- read.csv("data/chickExp2/e2results.csv")

data2$Treatment <- factor(data2$Treatment, levels = c("Control", "Colour"))

plot2 <- ggplot(data2, aes(x=Treatment, y=Latency, fill=Treatment)) +
  geom_boxplot(alpha=0.5, width=0.6) +
  theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_jitter(color="black", size=2.5, alpha=0.8,position=position_jitter(width=0.15)) +
  labs(x = "Treatment", y = "Latency (sec)") +
  scale_y_continuous(limits=c(0,30), breaks = c(0,5,10,15,20,25,30))+
  theme(legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        axis.title.x=element_text(margin=margin(15,0,0,0)),
        axis.title.y=element_text(margin=margin(0,15,0,0))) +
  scale_fill_manual(values=c("grey","lightblue"))

plot2

# output to pdf
currentdate <- Sys.Date()
pdfilename <- paste("out/plot_Exp2",currentdate,".pdf",sep="")
pdf(file=pdfilename, width=6, height=5)
plot2
dev.off()


#Is the Latency normally distributed
shapiro.test(data2$Latency)

#Distribution not Normal

##Analysis to use in thesis

glmer_model <- glmer(Latency ~ Treatment +(1|Chick) , family = "Gamma", data=data2)
summary(glmer_model)

##results to report: 
##Contrary to our prediction, chicks took significantly longer to find targets that moved without revealing a flash colour than those that did reveal a flash colour (t(24) = 2.16, p < 0.05; Fig. 2)

summary(data2)
data2 %>%
  group_by(Treatment) %>%
  summarize(mean = mean(Latency),
            q1 = quantile(Latency, 0.25),
            q3 = quantile(Latency, 0.75)
  )
