library(tidyverse)
library(lme4)

data <- read.csv("data/chickExp1/e1results.csv") 

data$Treatment <- factor(data$Treatment, levels = c("Control", "Colour"))

plot1 <- ggplot(data, aes(x=Treatment, y=Latency, fill=Treatment)) +
  geom_boxplot(alpha=0.5, width=0.6) +
  theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_jitter(color="black", size=2.5, alpha=0.8,position=position_jitter(width=0.15)) +
  labs(x = "Treatment", y = "Latency (sec)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        axis.title.x=element_text(margin=margin(15,0,0,0)),
        axis.title.y=element_text(margin=margin(0,15,0,0))) +
  scale_fill_manual(values=c("grey","lightblue"))

plot1

#Is the Latency normally distributed p <0.05 so data is NOT normal
shapiro.test(data$Latency)

#Distribution not Normal

glmer_model <- glmer(Latency ~ Treatment +(1|Chick) , family = "Gamma", data=data)
summary(glmer_model)

##results to report: 
##no effect of colour on latency to attack target (t(31) = -0.48, p = 0.63; Fig. 1)

# output to pdf
currentdate <- Sys.Date()
pdfilename <- paste("out/plot_Exp1",currentdate,".pdf",sep="")
pdf(file=pdfilename, width=6, height=5)
plot1
dev.off()


library(dplyr)
data %>%
  group_by(Treatment) %>%
  summarize(mean = mean(Latency),
            q1 = quantile(Latency, 0.25),
            q3 = quantile(Latency, 0.75),
            count = count(Latency),
           )
summary(data)
