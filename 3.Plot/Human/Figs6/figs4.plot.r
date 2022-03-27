library(openxlsx)
library(ggplot2)
library(ggpubr)
library(stringr)
library(cowplot)

# F:/Data Report/DataReport/For manuscript/v2/Data update/Fig1/Coverage Distribution/Overall_Coverage_210204.log
data <- read.xlsx("../../Data/SupplementaryTables_REVISED.xlsx", sheet = '1', startRow = 2)

# data <- read.xlsx("/data2/xingyun/GaoData/For_manuscript/Version_202106/Data/Fig1/SupplementTable_Human_Stat_210607.xlsx", sheet = 'Supplement Table1', startRow = 2)
data$Label <- gsub("\\d", "", data$PatientName)
data$Label <- factor(data$Label, levels = c("GLPHD", "GLPHCC", "GLPCRC", "GLPLC"))
colnames(data)[c(12,13,14,15)] <- c("RBC", "HGB", "WBC", "PLT")
data <- data[, c("PatientName", "Patient.Type", "RBC", "HGB", "WBC", "PLT", "Label")]
data =  data[!is.na(data$RBC),]
data =  data[data$RBC!='-',]
data$RBC <- as.numeric(data$RBC)
data$HGB <- as.numeric(data$HGB)
data$WBC <- as.numeric(data$WBC)
data$PLT <- as.numeric(data$PLT)

my_comparisons <- list( c("GLPHCC", "GLPCRC"), c("GLPLC", "GLPCRC"), c("GLPHCC", "GLPLC"), 
                        c("GLPHD", "GLPHCC"), c("GLPHD", "GLPCRC"),  c("GLPHD", "GLPLC"))

p_rbc <- ggplot(data, aes(x=Label, y=RBC, fill=Label)) + ylim(0,10) +ylab('')+xlab('')+
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.2, outlier.size=0.5)+ scale_fill_manual(values=c("#374E55FF", "#E64B35FF","#00A087FF","#DF8F44FF"), na.translate = F) + theme_classic() + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label.y = seq(10*0.6,10, length.out = 7) , label = "p.signif") +
    theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'), plot.margin=unit(c(0.3,0.3,0,-0.3), 'cm')) 

p_hgb <- ggplot(data, aes(x=Label, y=HGB, fill=Label)) + ylim(0,500) +ylab('')+xlab('')+
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.2, outlier.size=0.5)+ scale_fill_manual(values=c("#374E55FF", "#E64B35FF","#00A087FF","#DF8F44FF"), na.translate = F) + theme_classic() + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label.y = seq(500*0.6,500, length.out = 7) , label = "p.signif") +
    theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'), plot.margin=unit(c(0.3,0.3,0,-0.3), 'cm')) 

p_wbc <- ggplot(data, aes(x=Label, y=WBC, fill=Label)) + ylim(0,50) +ylab('')+xlab('')+
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.2, outlier.size=0.5)+ scale_fill_manual(values=c("#374E55FF", "#E64B35FF","#00A087FF","#DF8F44FF"), na.translate = F) + theme_classic() + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label.y = seq(50*0.6,50, length.out = 7) , label = "p.signif") +
    theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'), plot.margin=unit(c(0.3,0.3,0,-0.3), 'cm')) 

p_plt <- ggplot(data, aes(x=Label, y=PLT, fill=Label)) + ylim(0,1000) +ylab('')+xlab('')+
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.2, outlier.size=0.5)+ scale_fill_manual(values=c("#374E55FF", "#E64B35FF","#00A087FF","#DF8F44FF"), na.translate = F) + theme_classic() + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label.y = seq(1000*0.6, 1000, length.out = 7) , label = "p.signif") +
    theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'), plot.margin=unit(c(0.3,0.3,0,-0.3), 'cm')) 

pg <- plot_grid(p_rbc, p_hgb, p_wbc, p_plt, ncol=4, align="hv")
ggsave(str_c("FigS4B_BloodExtractionResults.pdf"), pg, wi=5.5, height=1.52)

