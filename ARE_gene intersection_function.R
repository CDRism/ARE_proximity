####Androgen response element analyses####
#The goal of this function is to examine whether genes regulated by androgens
#have more androgen response elements proximal to the gene than the number
#of androgen response elements proximal to all genes in the genome

library(ggplot2)
library(cowplot)

randRE = function(geneList, REs, sigNum, nperm){
  #geneList: vector of geneNames from a dataframe
  #REs: vector of RE values
  #sigNum: number of significant DEGs, used for standardization across iterations
  #nperm: number of random permutations to perform
  
  data = data.frame(cbind(geneList,REs))
  data$geneList = as.factor(geneList)
  data$REs = as.numeric(as.character(data$REs))
  
  nullDist = list()
  
  for(i in 1:nperm){
    data.sub = data[sample(length(data$geneList),sigNum,replace = F),]
    mean.REs = mean(data.sub$REs)
    
    nullDist[[length(nullDist)+1]] = data.frame(RE = mean.REs)
  }
  nullDist = plyr::ldply(nullDist)
  nullDist <<- nullDist
  hist(nullDist$RE)
}

#to get p-value (replace AREs with vector of ARE counts in your
#gene list of interest; denominator is the number of simulations)
length(which(nullDist$RE > mean(AREs)))/10000
#multiply by 2 if using a two-sided test (you should do this)

#figure
plot_theme = theme_bw()+ 
  theme(axis.text=element_text(size=12, color = "black"), axis.title=element_text(size=12, face="bold"))+
  theme(axis.text.y=element_blank(), axis.ticks.y = element_blank())+
  theme(legend.text = element_blank(), legend.title = element_blank())+
  theme(legend.position="none")+
  theme(panel.grid.major=element_line(colour="white"), panel.grid.minor = element_line(colour = "white"))+
  theme(panel.border=element_blank(), axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"))

#for the geom_segment line in each of these,
#replace AREs with your vector of ARE counts in your gene list of interest
#here I just have it set to 2 as an example
AREs = 2

a = ggplot_build(ggplot(nullDist, aes(x = RE))+
                   geom_density(alpha = 0.75, fill = "gray")+
                   xlab("Null Distribution\nof AREs")+ylab("Density")+
                   scale_y_continuous(expand = c(0,0))+
                   scale_x_continuous(expand = c(0,0))+
                   #coord_cartesian(xlim = c(-3,3))+
                   geom_segment(x = mean(AREs), xend = mean(AREs), y = 0, yend = 4, lwd = 1.5, color = "black", linetype = "solid")+
                   plot_theme)$data[[1]]

comp = ggplot(nullDist, aes(x = RE))+
  geom_area(data = subset(a, x < quantile(nullDist$RE, 0.025)), aes(x=x,y=y), fill = "#EBCC2A", alpha = 0.75)+
  geom_area(data = subset(a, x > quantile(nullDist$RE, 0.975)), aes(x=x,y=y), fill = "#EBCC2A", alpha = 0.75)+
  geom_density(alpha = 0.5, fill = "gray")+
  xlab("Null Distribution of AREs")+ylab("Density")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  #coord_cartesian(xlim = c(-3,3))+
  geom_segment(x = mean(AREs), xend = mean(AREs), y = 0, yend = 4, lwd = 1.5, color = "black", linetype = "dashed")+
  plot_theme
