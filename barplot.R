### R process ###
rm(list=ls()) 
#########

library(reshape2)    
library(ggplot2) 
library(ggalluvial)
library(scales)


#### Import data ####
setwd("E:/data/barplot/")

phylum <- read.csv("bac_type_phylum_RA.txt" ,header = T, sep = "\t",row.names = NULL)

phylum <- t(phylum)
phylum <- melt(phylum, id = "phylum")
colnames(phylum) <- c("variable","Taxon","value")

##color##
Palette1 <-c("dark green","green","springgreen3","cadetblue3","purple",
             "orange","violetred","brown","red",'#D55E00', "blue4","grey50","tan","salmon",
             "dark blue","yellow","black")
Palette3 <- c("red","purple1","olivedrab2","mediumpurple1","mistyrose3","orange","orchid1","sienna","skyblue","pink1",
              "peachpuff1","seashell3","tan1","violet","tomato","yellow3","yellowgreen")

phylum$variable <- factor(phylum$variable, levels=c('LNPC','LNTR','SXPC','SXTR','AHPC','AHTR','ZJPC','ZJTR','JSPC','JSTR'))
phylum$variable <- factor(phylum$variable, levels=c('PC','TR','D3','D4'))
phylum$variable <- factor(phylum$variable, levels=c('above','below','neutral'))
phylum$Taxon <- factor(phylum$Taxon, levels=c('Gammaproteobacteria','Alphaproteobacteria','Actinobacteria','Bacteroidia',
                                              'Bacilli','Clostridia','Deltaproteobacteria','Acidobacteriia',
                                              'Others'))
phylum$Taxon <- factor(phylum$Taxon, levels=c('Sordariomycetes','Eurotiomycetes','Dothideomycetes','Saccharomycetes',
                                              'Agaricomycetes','Unclassified_p_Ascomycota','Leotiomycetes','Tremellomycetes',
                                              'Mortierellomycetes','Wallemiomycetes','Microbotryomycetes','Malasseziomycetes',
                                              'Agaricostilbomycetes','Lecanoromycetes','Others'))

p1 <- ggplot(data = phylum,aes(x = variable, y = value, fill = Taxon))+
  geom_col(position = 'stack', width = 0.3)
p1 <- p + geom_alluvium(aes(fill = Taxon),alpha = .5,width = 0.6) +   geom_stratum(aes(fill = Taxon),width = 0.6)
p1

p2 <- p1 + ylab(label = "Relative Abundance") + xlab(label = "")


p3 <- p2 + scale_fill_manual(values = Palette1)
p3 <- p2 + scale_fill_manual(values = Palette3)
p3


p4 <- p3 + theme_bw()+ theme(panel.grid=element_blank()) +
  theme(panel.border = element_blank()) +  
  theme(panel.background=element_rect(fill='transparent',color='black'),plot.margin = unit(c(3,5,1,1),"mm"))
p4

p5 <- p4 + theme(axis.text.x=element_text(colour="black",size=12,face = "bold",angle = 45,vjust = 1,hjust = 1)) +
  theme(axis.text.y=element_text(colour = "black",size = 12)) +   
  theme(axis.line = element_line(colour = "black"))+   
  theme(axis.title.y = element_text(size = 12,face = "bold",  margin = unit(c(0,1,0,1),"lines")))
p5

p6 <- p5 + scale_y_continuous(expand = c(0,0)) 
p6

p7 <- p6 + theme(legend.text = element_text(colour = "black",size = 12)) +
  theme(legend.title = element_text(size = 12,colour = "black"))
p7
