rm(list=ls())
setwd(choose.dir())
getwd()

library(ggplot2)

## Figure 1B; Figure S1

data <- read.table("bac1_type_alpha.txt",header = T)

data$Type <- factor(data$Type, levels=c('PC','TR','D3','D4'))
data$Area <- factor(data$Area, levels=c('LN','SX','AH','ZJ','JS')) 

data$group <- factor(data$group, levels=c('PC','TR','D4'))
color_type <- c("#26a1b6","#f38827","#0072B2","#B2182B")
p<- ggplot(data, aes(x = Type, y = chao1, fill = Type))+
  geom_boxplot(alpha=0.7, size=0.7)+
  #geom_jitter(aes(shape=Area, width=0.25, alpha=2, size=0.5, stroke=1))+
  geom_jitter(color = 'black', show.legend = FALSE,width = 0.2) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = 'black'),
    plot.background = element_rect(fill = "transparent",colour = 'black')
  )

p1 <- p + scale_fill_manual(values = color_type)
p1
ggsave(p1,filename="bac1_type_chao1.pdf", width= 5, height = 5)


## Figure S3C

data <- read.table("bac_PCTR_alpha.txt",header = T)

data$Type <- factor(data$Type, levels=c('PC','TR','D3','D4'))
data$Area <- factor(data$Area, levels=c('LN','SX','AH','ZJ','JS'))  
data$Year <- as.factor(data$Year)

p<- ggplot(data, aes(x = Area, y =shannon , color = Type))+
  #geom_bar(stat = "identity", position = "dodge")+
  geom_boxplot(alpha=0.5, size=1.5, position=position_dodge(0.8))+
  geom_jitter(aes(shape=Year, fill=Type), position=position_dodge(0.8),size=2)+
  #geom_jitter( position=position_jitter(0.17), size=1, alpha=0.7)+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = 'black'),
    plot.background = element_rect(fill = "transparent",colour = 'black')
  )

p


library(dplyr)
data <- data %>% mutate(dist_cat = as.numeric(as.factor(Area)),
                        scat_adj = ifelse(Type == "PC", -0.2, 0.2))

p <- ggplot(data, aes(x = Area, y =observed_otus)) + 
     geom_boxplot(aes(color = Type), notch = FALSE, size = 1) + 
     geom_jitter(aes(scat_adj+dist_cat, observed_otus, shape=factor(Year),color = Type),
              position=position_jitter(width=0.1,height=0), alpha=1,size = 1.5) +
    #scale_color_brewer(palette = "Set2") +
    theme_bw()+theme(panel.grid = element_blank())

p1 <- p + scale_color_manual(values = c("#26a1b6","#f38827"))
p1
ggsave(p1,filename="bac_PCTR_sobs_year.pdf", width= 5, height = 5)


