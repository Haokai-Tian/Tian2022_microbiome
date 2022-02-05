
### R process ###
rm(list=ls())
setwd(choose.dir())
getwd()
library(ggplot2)

#### Import data ####
dat <- read.delim('bac_PCTR_FDR0.05_filtered.txt', sep = '\t')




dat2 <-merge(dat, dat1, by.x="otu",by.y="otu") ##merge#



dat <- dat[order(dat$phylum_class), ]
dat$otu_sort <- 1:nrow(dat)


phylum_num <- phylum_num[order(phylum_num, decreasing = TRUE)]
otu_stat$phylum <- factor(otu_stat$phylum, levels = names(phylum_num))
otu_stat <- otu_stat[order(otu_stat$phylum), ]
otu_stat$otu_sort <- 1:nrow(otu_stat)


p <- ggplot(dat, aes(otu_sort, -log(FDR, 10))) +      
  geom_point(aes(size = logCPM, color = phylum, shape = enrich)) +       
  scale_size(range = c(1, 5))+   
  scale_shape_manual(limits = c('enriched', 'depleted',"no-sign"), values = c(17, 25,20)) +        
  labs(x = NULL, y = '-log10(FDR)', size = 'log2(CPM)', shape = 'enriched') + 
  theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'), panel.background = element_rect(fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +   
  geom_hline(yintercept = -log10(0.05), color = 'gray', linetype = 2, size = 1)       

p


phylum_num <- summary(dat$phylum_class)

phylum_range <- c(0, phylum_num[1])
phylum_name <- phylum_num[1]/2
for (i in 2:length(phylum_num)) {
  phylum_range[i+1] <- phylum_range[i] + phylum_num[i] 
  phylum_name[i] <- phylum_range[i] + phylum_num[i]/2
}


p <- p +
  scale_x_continuous(breaks = phylum_name, labels = names(phylum_num), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p


for (i in 1:(length(phylum_range) - 1)) p <- p + annotate('rect', xmin = phylum_range[i], xmax = phylum_range[i+1], ymin = -Inf, ymax = Inf, alpha = 0.1, fill = ifelse(i %% 2 == 0, 'gray60', 'gray40'))

p



p <- ggplot(dat, aes(otu_sort, -log(FDR, 10))) +
  labs(x = NULL, y = '-log10(FDR)', size = 'log2(CPM)') +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'), panel.background = element_rect(fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  scale_x_continuous(breaks = phylum_name, labels = names(phylum_num), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


for (i in 1:(length(phylum_range) - 1)) p <- p + annotate('rect', xmin = phylum_range[i], xmax = phylum_range[i+1], ymin = -Inf, ymax = Inf, fill = ifelse(i %% 2 == 0, 'gray95', 'gray85'))


p <- p + 
  geom_point(aes(size = logCPM, color = phylum, shape = enrich)) +
  scale_size(breaks= c(5,10,15))+
  scale_shape_manual(limits = c('enriched', 'depleted',"no-sign"), values = c(17, 25,20)) +   
  guides(color = 'none') +
  geom_hline(yintercept = -log10(0.05), color = 'gray', linetype = 2, size = 1)

p



ggsave('bac_PCTR_manhattan1.pdf', p, width = 15, height = 5)
ggsave('bac_PCTR_manhattan1.png', p, width = 15, height = 5)


p <- ggplot(dat, aes(otu_sort, -log(FDR, 10))) +
  labs(x = NULL, y = '-log10(FDR)') +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'), panel.background = element_rect(fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  scale_x_continuous(breaks = phylum_name, labels = names(phylum_num), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


for (i in 1:(length(phylum_range) - 1)) p <- p + annotate('rect', xmin = phylum_range[i], xmax = phylum_range[i+1], ymin = -Inf, ymax = Inf, fill = ifelse(i %% 2 == 0, 'gray95', 'gray85'))
p <- p + 
  geom_point(aes(size = logCPM, color = enrich)) +
  #scale_size(breaks= c(5,10,15))+
  # scale_shape_manual(limits = c('enriched', 'depleted',"no-sign"), values = c(17, 25,20)) +   
  guides(color = 'none') +
  geom_hline(yintercept = -log10(0.05), color = 'gray', linetype = 2, size = 1)

p

library(ggExtra)
ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)
ggMarginal(p, type = "boxplot", groupColour = TRUE, groupFill = TRUE)
library(cowplot)
library(ggpubr)
