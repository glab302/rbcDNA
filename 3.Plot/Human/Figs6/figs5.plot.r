
library(reportROC)
library(stringr)
library(openxlsx)
library(ggplot2)
library(stringr)
library(cowplot)
library(ggpubr)
# Healthy-PanCancer
test <- read.table("./Fig2/rbcDNA_DNN4L_Classification/1.DataMatrix/Classification_name/lccrchcc_hd_binaryc_test.csv", sep=",", head=T)
test_proba <- read.table("./Fig2/rbcDNA_DNN4L_Classification/3.Results/lccrchcc_hd/Ensemble_Model_test.csv", sep="\t", head=T)
test_proba <- cbind(test, test_proba)[, c("PatientName", "PatientType", "Stage", "Target", "X", "class_true", "final_prob")]
rownames(test_proba) <- test_proba$PatientName
library("pROC")
pdf("BestThreshold.pdf");roc(test$Target, test_proba$final_prob, plot=TRUE, print.thres=TRUE, print.auc=TRUE);dev.off()
# F:/Data Report/DataReport/For manuscript/v2/Data update/Fig1/Coverage Distribution/Overall_Coverage_210204.log
data <- read.xlsx("../../Data/SupplementaryTables_REVISED.xlsx", sheet = '1', startRow = 2)
rownames(data) <- data$PatientName
data$Label <- gsub("\\d", "", data$PatientName)
data$Label <- factor(data$Label, levels = c("GLPHD", "GLPHCC", "GLPCRC", "GLPLC"))
colnames(data)[c(12,13,14,15)] <- c("RBC", "HGB", "WBC", "PLT")
data <- data[, c("PatientName", "Patient.Type", "RBC", "HGB", "WBC", "PLT", "Label")]


data <- cbind(data[rownames(test_proba),], test_proba)
data$Patient.Name==data$PatientName
data$RBC <- as.numeric(data$RBC)
data$HGB <- as.numeric(data$HGB)
data$WBC <- as.numeric(data$WBC)
data$PLT <- as.numeric(data$PLT)

hd_prob <- data[data$Target==0,"final_prob"]; hd_label <- rep(0,length(hd_prob))
lc_prob <- data[data$Target==1,"final_prob"]; lc_label <- rep(1,length(lc_prob))

predictor <- c(hd_prob, lc_prob)
binary=rep(0,length(predictor))
threshold=max(hd_prob[-which(hd_prob==max(hd_prob))])

binary[predictor>=threshold]=1
stage1 <- as.numeric(reportROC(gold=c(hd_label, lc_label),predictor.binary=binary,plot=F, important="se")[c("SEN","SEN.low","SEN.up")])

data[data$final_prob<=threshold, "final_pred"] <- 0
data[data$final_prob>threshold, "final_pred"] <- 1
data[data$class_true==data$final_pred, "Pred_CW"] <- "Correct"
data[data$class_true!=data$final_pred, "Pred_CW"] <- "WRONG"

data$Pred_CW <- as.factor(data$Pred_CW)
my_comparisons <- list( c("Correct", "WRONG") )
data =  data[!is.na(data$RBC),]
p_rbc <- ggplot(data, aes(x=Pred_CW, y=RBC, fill=Pred_CW)) + ylim(0,10) +ylab('')+xlab('')+
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.2, outlier.size=0.5)+ scale_fill_manual(values=c("#7876B1FF","#374E55FF"), na.translate = F) + theme_classic() + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label.y = 10*0.7 , label = "p.signif") +
      theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'), plot.margin=unit(c(0.3,0.3,0,-0.3), 'cm')) 
 
p_hgb <- ggplot(data, aes(x=Pred_CW, y=HGB, fill=Pred_CW)) + ylim(0,500)  +ylab('')+xlab('')+
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.2, outlier.size=0.5)+ scale_fill_manual(values=c("#7876B1FF","#374E55FF"), na.translate = F) + theme_classic() + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label.y = 500*0.7 , label = "p.signif") +
      theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'), plot.margin=unit(c(0.3,0.3,0,-0.3), 'cm')) 

p_wbc <- ggplot(data, aes(x=Pred_CW, y=WBC, fill=Pred_CW)) + ylim(0,50) +ylab('')+xlab('')+
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.2, outlier.size=0.5)+ scale_fill_manual(values=c("#7876B1FF","#374E55FF"), na.translate = F) + theme_classic() + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label.y = 50*0.7 , label = "p.signif") +
      theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'), plot.margin=unit(c(0.3,0.3,0,-0.3), 'cm')) 

p_plt <- ggplot(data, aes(x=Pred_CW, y=PLT, fill=Pred_CW)) + ylim(0,1000) +ylab('')+xlab('')+
  geom_boxplot(position=position_dodge(width = 0.7), lwd=0.2, outlier.size=0.5)+ scale_fill_manual(values=c("#7876B1FF","#374E55FF"), na.translate = F) + theme_classic() + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label.y = 1000*0.7 , label = "p.signif" )+
      theme(strip.background =element_blank(),legend.position="none",axis.ticks.x = element_blank(), axis.text=element_text(color='black'), plot.margin=unit(c(0.3,0.3,0,-0.3), 'cm')) 

pg <- plot_grid(p_rbc, p_hgb, p_wbc, p_plt, ncol=4, align="hv")

ggsave(str_c("FigS4C_BloodExtractionResults.pdf"), pg, wi=5.5, height=1.52)

table(data$Pred_CW)
