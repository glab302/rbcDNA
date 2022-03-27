##########################################################
# Fig1b
##########################################################
df <- data.frame(
  name = c("aHCC","aHCC", "aHCC", "aHCC","bCRC", "bCRC", "bCRC", "bCRC","cLC", "cLC", "cLC", "cLC", "HD", "HD"),
  type = c("all", "type1", "type2", "type3", "all", "type1", "type2", "type3", "all", "type1", "type2", "type3","all","all"),
  values = c(44, 4, 14, 26, 99, 37, 35, 27, 89, 22, 13, 54, 106, 106),
  level = c('1','2','2','2','1','2','2','2','1','2','2','2','1','2')
)

Fig1b <- ggplot(df, aes(x = level, y = values, fill = name, alpha = level)) +
    geom_col(width = 1, color = 'gray90', size = 1, position = position_stack()) +
    coord_polar(theta = 'y') +
    scale_alpha_manual(values = c('0' = 0.2, '1' = 1, '2' = 0.7), guide = F) +
    scale_x_discrete(breaks = NULL) +
    scale_y_continuous(breaks = NULL) + #scale_fill_jama()
    scale_fill_manual(values=c("#E64B35FF","#00A087FF","#DF8F44FF","#374E55FF"), na.translate = F) + #绿色"#1B7837"
    labs(x = NULL, y = NULL) + 
    theme_minimal()+theme(legend.position="none")

pdf("Fig1b.pdf")
Fig1b
dev.off()



##########################################################
# Fig1c
##########################################################
library(stringr)
library(karyoploteR)
library(regioneR)
library(zoo)

Graphics.rvg <- function(rbcDNA_Sample, gDNA_Sample, savefile){
  pdf(str_c('Fig1c.',savefile,'.pdf'), height=4, width=5)
  # Plotting chr
  pp <- getDefaultPlotParams(plot.type=1)
  pp$leftmargin <- 0.2
  pp$data1height <- 400
  pp$data1inmargin <- 10
  pp$data2inmargin <- 0
  pp$ideogramheight <- 10
  # genome <- toGRanges('/data2/xingyun/GaoData/For_manuscript/Version_202110/Summary/Bam/Human/rgDNA_5G20G/GRCh38.sort.genome')
  # kp <- plotKaryotype(genome, plot.type=1, cex=0.5, plot.params=pp)
  # kp <- plotKaryotype('hg38', plot.type=1, cex=0.5, chromosomes = paste0("chr", c(1:22)), plot.params=pp)
  kp <- plotKaryotype('hg38', plot.type=1, cex=0.5, chromosomes = paste0("chr", c(1:22)), plot.params=pp)#, ideogram.plotter=NULL)
  cy=getCytobandColors(color.schema="biovizbase")
  kpAddCytobandsAsLine(kp, lwd=1, color.schema='biovizbase', color.table=cy)
  # Whole-genome Barplot
  kpPlotBigWig(kp, data=rbcDNA_Sample,ymin=0, ymax='global', data.panel=1, r0=0.51, r1=1, col="#E64B35FF", border=NA)
  kpAxis(kp, ymin=0, ymax=kp$latest.plot$computed.values$ymax, numticks=2, r0=0.51, r1=1, col="black", cex=0.01, side = 1, lwd=0.2, text.col='white')
  kpPlotBigWig(kp, data=gDNA_Sample,ymin=0, ymax='global', data.panel=1, r0=0.49, r1=0, col="#3E60AA", border=NA)
  kpAxis(kp, ymin=0, ymax=kp$latest.plot$computed.values$ymax, numticks=2, r0=0.49, r1=0, col="black", cex=0.01, side = 1, lwd=0.2, text.col='white')
  dev.off()
}
filepath="/data2/xingyun/GaoData/For_manuscript/Version_202202/Bam/Human/RunData/20200118.file2.tempAnalysis"
Graphics.rvg(str_c(filepath, '/GLPExp_20r.uniq.nodup.1000000.bw'), str_c(filepath, '/GLPExp_20g.uniq.nodup.1000000.bw'), '1mbin')
Graphics.rvg(str_c(filepath, '/GLPExp_20r.uniq.nodup.1000000rmbl.bw'), str_c(filepath, '/GLPExp_20g.uniq.nodup.1000000rmbl.bw'), '1mbinrmbl')




##########################################################
# Fig1e
##########################################################
library(reshape2)
library(ggplot2)
library(openxlsx)
library(stringr)
library(ggpubr)
library(cowplot)
data <- read.xlsx("../../Data/SupplementaryTables_REVISED.xlsx", sheet = '2', startRow = 2)
data_use_melt <- data
data_use_melt$PercIntergenic = data[,'#Reads-in-Intergenic']/data[, '#High.Quality.Bases.Analyzed']
data_use_melt$PercIntron = data[,'#Reads-in-Intron']/data[, '#High.Quality.Bases.Analyzed']
data_use_melt$PercExon = data[,'#Reads-in-Exon']/data[, '#High.Quality.Bases.Analyzed']
data_use <- data_use_melt[, c('PatientName', 'PercIntergenic', 'PercIntron', 'PercExon')]
data_use_melt <- melt(data_use, id = 'PatientName')
data_use_melt$Label <- as.factor(gsub("\\d","",data_use_melt$PatientName))
#绘制带分面的柱状图
data_use_melt$PatientName <- factor(data_use_melt$PatientName, levels=as.character(data_use_melt[data_use_melt$variable=="PercIntron", "PatientName"]))
data_use_melt[data_use_melt$Label!='GLPHD', 'Label2'] <- 'PanC'
data_use_melt[data_use_melt$Label=='GLPHD', 'Label2'] <- 'HD'

data_f <- data_use_melt
data_f$Label <- factor(data_f$Label, levels = c("GLPHD", "GLPHCC", "GLPCRC", "GLPLC"))
data_f$Label2 <- factor(data_f$Label2, levels = c("HD", "PanC"))
data_f_intergenic <- data_f[(data_f$variable=="PercIntergenic"),]
data_f_exon <- data_f[(data_f$variable=="PercExon"),]
data_f_intron <- data_f[(data_f$variable=="PercIntron"),]
my_comparisons <- list( c("HD", "PanC") )
p_intergenic <- ggplot(data_f_intergenic, aes(x=Label2, y=value, fill=Label2)) +labs(x = '', y = '') +#Proportion Of Covered Genome
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.3, outlier.size=0.5) + 
  scale_fill_manual(values=c("#374E55FF", "#ADDAE9"))+ 
  theme_classic() + 
  scale_y_continuous(breaks=seq(0,1,0.25),limits = c(0,1.1), labels = seq(0,1,0.25)*100)+
  stat_compare_means(comparisons = my_comparisons, aes(label=..p.signif..), method = "t.test", label.y = seq(0.75,1.1,0.06)) + 
  theme(strip.background =element_blank(), legend.position="none",  axis.ticks.x = element_blank(), axis.text=element_text(color='black'))#, plot.margin=unit(c(0.3,-0.15,0,-0.5), 'cm')) # margin: shang,you,xia,zuo
p_intron <- ggplot(data_f_intron, aes(x=Label2, y=value, fill=Label2)) + labs(x = '', y = '') +
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.3, outlier.size=0.5)+ 
  scale_fill_manual(values=c("#374E55FF", "#ADDAE9"))+ 
  theme_classic()+
  scale_y_continuous(breaks=seq(0,1,0.25),limits = c(0,1.1), labels = seq(0,1,0.25)*100)+
  stat_compare_means(comparisons = my_comparisons, aes(label=..p.signif..), method = "t.test", label.y = seq(0.75,1.1,0.06)) + 
  theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'))#, plot.margin=unit(c(0.3,-0.15,0,-0.5), 'cm')) 
p_exon <- ggplot(data_f_exon, aes(x=Label2, y=value, fill=Label2))+ labs(x = '', y = '') +
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.3, outlier.size=0.5)+ 
  scale_fill_manual(values=c("#374E55FF", "#ADDAE9"))+
  theme_classic()+ 
  scale_y_continuous(breaks=seq(0,0.1,0.025),limits = c(0,0.11), labels = seq(0,0.1,0.025)*100)+
  stat_compare_means(comparisons = my_comparisons, aes(label=..p.signif..), method = "t.test", label.y = seq(0.075,0.11,0.006)) + 
  theme(strip.background =element_blank(),legend.position="none",  axis.ticks.x = element_blank(), axis.text=element_text(color='black'))#, plot.margin=unit(c(0.3,-0.15,0,-0.5), 'cm'))
pg <- plot_grid(p_intergenic, p_intron, p_exon, ncol=4, align="hv",rel_widths=c(1,1,1,1))#p_centro, ncol=4,
ggsave(str_c("Fig1e.pdf"), pg, wi=6, height=3)



##########################################################
# Extended fig 5
##########################################################
my_comparisons <- list( c("GLPHCC", "GLPCRC"), c("GLPLC", "GLPCRC"), c("GLPHCC", "GLPLC"), c("GLPHD", "GLPHCC"), c("GLPHD", "GLPCRC"), c("GLPHD", "GLPLC") )
p_intergenic <- ggplot(data_f_intergenic, aes(x=Label, y=value, fill=Label)) +labs(x = '', y = '') +#Proportion Of Covered Genome
  geom_boxplot(draw_quantiles = 0.5, trim = FALSE, position=position_dodge(width = 0.7),width=1) + 
  scale_fill_manual(values=c("#374E55FF", "#E64B35FF","#00A087FF","#DF8F44FF"))+ 
  theme_classic() + 
  scale_y_continuous(breaks=seq(0,1,0.25),limits = c(0,1.1), labels = seq(0,1,0.25)*100)+
  stat_compare_means(comparisons = my_comparisons, aes(label=..p.signif..), method = "t.test", label.y = seq(0.75,1.1,0.06)) + 
  theme(strip.background =element_blank(), legend.position="none",  axis.ticks.x = element_blank(), axis.text=element_text(color='black'))#, plot.margin=unit(c(0.3,-0.15,0,-0.5), 'cm')) # margin: shang,you,xia,zuo
p_exon <- ggplot(data_f_exon, aes(x=Label, y=value, fill=Label))+ labs(x = '', y = '') +
  geom_boxplot(draw_quantiles = 0.5, trim = FALSE, position=position_dodge(width = 0.7),width=1)+ 
  scale_fill_manual(values=c("#374E55FF", "#E64B35FF","#00A087FF","#DF8F44FF"))+
  theme_classic()+ 
  scale_y_continuous(breaks=seq(0,0.1,0.025),limits = c(0,0.11), labels = seq(0,0.1,0.025)*100)+
  stat_compare_means(comparisons = my_comparisons, aes(label=..p.signif..), method = "t.test", label.y = seq(0.075,0.11,0.006)) + 
  theme(strip.background =element_blank(),legend.position="none",  axis.ticks.x = element_blank(), axis.text=element_text(color='black'))#, plot.margin=unit(c(0.3,-0.15,0,-0.5), 'cm'))
p_intron <- ggplot(data_f_intron, aes(x=Label, y=value, fill=Label)) + labs(x = '', y = '') +
  geom_boxplot(draw_quantiles = 0.5, trim = FALSE, position=position_dodge(width = 0.7),width=1)+ 
  scale_fill_manual(values=c("#374E55FF", "#E64B35FF","#00A087FF","#DF8F44FF"))+ 
  theme_classic()+
  scale_y_continuous(breaks=seq(0,1,0.25),limits = c(0,1.1), labels = seq(0,1,0.25)*100)+
  stat_compare_means(comparisons = my_comparisons, aes(label=..p.signif..), method = "t.test", label.y = seq(0.75,1.1,0.06)) + 
  theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'))#, plot.margin=unit(c(0.3,-0.15,0,-0.5), 'cm')) 
p_centro <- ggplot(data_f_centro, aes(x=Label, y=value, fill=Label)) + labs(x = '', y = '') +
  geom_boxplot(draw_quantiles = 0.5, trim = FALSE, position=position_dodge(width = 0.7),width=1)+ 
  scale_fill_manual(values=c("#374E55FF", "#E64B35FF","#00A087FF","#DF8F44FF"))+ 
  theme_classic()+
  scale_y_continuous(breaks=seq(0,1,0.25),limits = c(0,1.1), labels = seq(0,1,0.25)*100)+
  stat_compare_means(comparisons = my_comparisons, aes(label=..p.signif..), method = "t.test", label.y = seq(0.75,1,0.05)) + 
  theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'), plot.margin=unit(c(0.3,-0.15,0,-0.3), 'cm')) 

pg <- plot_grid(p_intergenic, p_intron, p_exon, ncol=3, align="hv",rel_widths=c(0.3, 0.3, 0.3))
ggsave(str_c("ExtendedFig5.pdf"), pg, wi=6, height=3)



