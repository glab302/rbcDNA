#!/usr/bin/env Rscript
fun_anova <- function(grp1_data,grp2_data,pvalue.threshold){
      p = NULL
      p_adj = NULL
      group_data <- cbind(grp1_data,grp2_data)
      group_data <- na.omit(group_data)#删除有NA的行
      # group_data <- group_data[apply(group_data,1,sum)!=0,]
      group_label <- as.factor(c(rep("grp1_data",length(colnames(grp1_data))),rep("grp2_data",length(colnames(grp2_data)))))
      for(i in 1:dim(group_data)[1]) { 
        group_data_i <- as.numeric(c(group_data[i,colnames(grp1_data)],group_data[i,colnames(grp2_data)]))
        group_i <- data.frame(group_data_i,group_label)
        levels(group_i$group_label)
        res.aov <- aov(group_data_i ~ group_label,data=group_i)
        res.aov.summary <- summary(res.aov)
        res.tukey <- TukeyHSD(res.aov)# performing multiple pairwise-comparison between the means of groups
        p <- c(p,as.numeric(unlist(res.aov.summary)[9])) 
        p_adj <- c(p_adj,as.numeric(unlist(res.tukey)[4]))
      }
      names(p) <- rownames(group_data)
      names(p_adj) <- rownames(group_data)
      group_data.p.res <- as.data.frame(cbind(group_data,p,p_adj))
      colnames(group_data.p.res)[(dim(group_data.p.res)[2]-1):dim(group_data.p.res)[2]] <- c("one_wayAnova.pvalue","one_wayAnova.p_adj")
      return(group_data.p.res)
}

FindFeatures <- function(grp1_data,grp2_data,test.use,lg2fc.threshold,pvalue.threshold,Pseudocount,title){
  test_p_result <- c()
  control_versus_treat_selected_regions_intersect <- c()
  grp1_data <- as.data.frame(grp1_data)
  grp2_data <- as.data.frame(grp2_data)
  if(test.use=="anova"){
    test_p_result <- fun_anova(grp1_data,grp2_data,pvalue.threshold)
  }
  avg_lg2FC <- (log2((apply(grp2_data,1,mean)+Pseudocount)/(apply(grp1_data,1,mean)+Pseudocount)))
  med_lg2FC <- (log2((apply(grp2_data,1,median)+Pseudocount)/(apply(grp1_data,1,median)+Pseudocount)))
  grp2vgrp1 <- as.data.frame(cbind(test_p_result,avg_lg2FC,med_lg2FC))
  ####################################
  # sign the significant regions
  ####################################
  pvalue = grp2vgrp1[,(dim(grp2vgrp1)[2]-3)]
  padj = grp2vgrp1[,(dim(grp2vgrp1)[2]-2)]
  avg.lg2fc = grp2vgrp1[,(dim(grp2vgrp1)[2]-1)]
  median.lg2fc = grp2vgrp1[,(dim(grp2vgrp1)[2])]

  grp2vgrp1$Significant.pvalue.avg_lg2FC <- ifelse(pvalue<=pvalue.threshold&avg.lg2fc>lg2fc.threshold, "upsig", "nosig")
  grp2vgrp1[pvalue<=pvalue.threshold&avg.lg2fc<(-lg2fc.threshold),"Significant.pvalue.avg_lg2FC"]="downsig"
  print(table(grp2vgrp1$"Significant.pvalue.avg_lg2FC"))
  grp2vgrp1$Significant.padj.avg_lg2FC <- ifelse(padj<=pvalue.threshold&avg.lg2fc>lg2fc.threshold, "upsig", "nosig")
  grp2vgrp1[padj<=pvalue.threshold&avg.lg2fc<(-lg2fc.threshold),"Significant.padj.avg_lg2FC"]="downsig"
  print(table(grp2vgrp1$"Significant.pvalue.avg_lg2FC"))
  grp2vgrp1$Significant.pvalue.med_lg2FC <- ifelse(pvalue<=pvalue.threshold&median.lg2fc>lg2fc.threshold, "upsig", "nosig")
  grp2vgrp1[pvalue<=pvalue.threshold&median.lg2fc<(-lg2fc.threshold),"Significant.pvalue.med_lg2FC"]="downsig"
  print(table(grp2vgrp1$"Significant.pvalue.med_lg2FC"))
  grp2vgrp1$Significant.padj.med_lg2FC <- ifelse(padj<=pvalue.threshold&median.lg2fc>lg2fc.threshold, "upsig", "nosig")
  grp2vgrp1[padj<=pvalue.threshold&median.lg2fc<(-lg2fc.threshold),"Significant.padj.med_lg2FC"]="downsig"
  print(table(grp2vgrp1$"Significant.pvalue.med_lg2FC"))
  ####################################
  # Output1:
  ####################################
  write.table(grp2vgrp1,str_c(title,"DataMatrix_SignedFindMarkers.log"),sep="\t")
  ####################################
  # Output2:
  ####################################
  grp2vgrp1.temp <- cbind(grp2vgrp1,pvalue,avg.lg2fc)
  plot1=ggplot(grp2vgrp1.temp, aes(x = avg.lg2fc, y = -log10(pvalue))) +
        geom_point(aes(color = Significant.pvalue.avg_lg2FC),show.legend = FALSE) +
        scale_color_manual(values = c("blue","grey","red")) +xlab(str_c("log2 Fold Change\n","Sign Regions:",dim(grp2vgrp1.temp[grp2vgrp1.temp$Significant.pvalue.avg_lg2FC!="nosig",])))+ylab("-log10 (pvalue)")+ ggtitle(str_c(title,"Significant.pvalue.avg_lg2FC"))+
        theme_bw() + theme(legend.position = "bottom",axis.title= element_text(size=10,color="black", vjust=0.5, hjust=0.5),
        	axis.text= element_text(size=15, color="black", vjust=0.5, hjust=0.5),legend.text= element_text(size=15,  color="black", face= "bold", vjust=0.5, hjust=0.5)) +guides(fill=FALSE)
  select_features=rownames(grp2vgrp1.temp[grp2vgrp1.temp$Significant.pvalue.avg_lg2FC!="nosig",])
  
  grp2vgrp1.temp <- cbind(grp2vgrp1,padj,avg.lg2fc)
  plot2=ggplot(grp2vgrp1.temp, aes(x = avg.lg2fc, y = -log10(padj))) +
        geom_point(aes(color = Significant.padj.avg_lg2FC),show.legend = FALSE) +
        scale_color_manual(values = c("blue","grey","red")) +xlab(str_c("log2 Fold Change\n","Sign Regions:",dim(grp2vgrp1.temp[grp2vgrp1.temp$Significant.padj.avg_lg2FC!="nosig",])))+ylab("-log10 (padj)")+ ggtitle(str_c(title,"Significant.padj.avg_lg2FC"))+
        theme_bw() + theme(legend.position = "bottom",axis.title= element_text(size=10,color="black", vjust=0.5, hjust=0.5),
        	axis.text= element_text(size=15, color="black", vjust=0.5, hjust=0.5),
        	legend.text= element_text(size=15,  color="black", face= "bold", vjust=0.5, hjust=0.5)) +guides(fill=FALSE)
  select_features=rownames(grp2vgrp1.temp[grp2vgrp1.temp$Significant.padj.avg_lg2FC!="nosig",])
      
  grp2vgrp1.temp <- cbind(grp2vgrp1,pvalue,median.lg2fc)
  plot3=ggplot(grp2vgrp1.temp, aes(x = median.lg2fc, y = -log10(pvalue))) +
        geom_point(aes(color = Significant.pvalue.med_lg2FC),show.legend = FALSE) +
        scale_color_manual(values = c("blue","grey","red")) +xlab(str_c("log2 Fold Change\n","Sign Regions:",dim(grp2vgrp1.temp[grp2vgrp1.temp$Significant.pvalue.med_lg2FC!="nosig",])))+ylab("-log10 (pvalue)")+ ggtitle(str_c(title,"Significant.pvalue.med_lg2FC"))+
        theme_bw() + theme(legend.position = "bottom",axis.title= element_text(size=10,color="black", vjust=0.5, hjust=0.5),
        	axis.text= element_text(size=15, color="black", vjust=0.5, hjust=0.5),
        	legend.text= element_text(size=15,  color="black", face= "bold", vjust=0.5, hjust=0.5)) +guides(fill=FALSE)
  select_features=rownames(grp2vgrp1.temp[grp2vgrp1.temp$Significant.pvalue.med_lg2FC!="nosig",])

  grp2vgrp1.temp <- cbind(grp2vgrp1,padj,median.lg2fc)
  plot4=ggplot(grp2vgrp1.temp, aes(x = median.lg2fc, y = -log10(padj))) +
        geom_point(aes(color = Significant.padj.med_lg2FC),show.legend = FALSE) +
        scale_color_manual(values = c("blue","grey","red")) +xlab(str_c("log2 Fold Change\n","Sign Regions:",dim(grp2vgrp1.temp[grp2vgrp1.temp$Significant.padj.med_lg2FC!="nosig",])))+ylab("-log10 (padj)")+ ggtitle(str_c(title,"Significant.padj.med_lg2FC"))+
        theme_bw() + theme(legend.position = "bottom",axis.title= element_text(size=10,color="black", vjust=0.5, hjust=0.5),
        	axis.text= element_text(size=15, color="black", vjust=0.5, hjust=0.5),
        	legend.text= element_text(size=15,  color="black", face= "bold", vjust=0.5, hjust=0.5)) +guides(fill=FALSE)
  select_features=rownames(grp2vgrp1.temp[grp2vgrp1.temp$Significant.padj.med_lg2FC!="nosig",])

  pg <- plot_grid(plot1, plot2,plot3, plot4, ncol=2, rel_heights=c(2, 2), align="v", label_size=17, hjust=10)
  ggsave(str_c(title,"DataMatrix_SignedFindMarkers.pdf"),pg,width=12,height=12)

  sig_up=grp2vgrp1[grp2vgrp1$Significant.padj.avg_lg2FC=='upsig',]
  sig_up=tail(rownames(sig_up[order(sig_up$avg_lg2FC),]),20)
  sig_down=grp2vgrp1[grp2vgrp1$Significant.padj.avg_lg2FC=='downsig',]
  sig_down=head(rownames(sig_down[order(sig_down$avg_lg2FC),]),20)
  data_rownames_conapc=c(sig_up,sig_down)
  table(grp2vgrp1[data_rownames_conapc,]$Significant.padj.avg_lg2FC)
  select_features=data_rownames_conapc
  write.table(grp2vgrp1[data_rownames_conapc,],str_c(title,"DataMatrix_SignedFindMarkers_40regions.log"),sep="\t")
  ####################################
  # Output2:
  ####################################
  grp2vgrp1.temp <- cbind(grp2vgrp1,pvalue,avg.lg2fc)
  grp2vgrp1.temp$Significant.pvalue.avg_lg2FC_40 <- "nosig"
  grp2vgrp1.temp[sig_up,"Significant.pvalue.avg_lg2FC_40"] <- "upsig"
  grp2vgrp1.temp[sig_down,"Significant.pvalue.avg_lg2FC_40"] <- "downsig"
  print(table(grp2vgrp1.temp$"Significant.pvalue.avg_lg2FC_40"))

  plot1=ggplot(grp2vgrp1.temp, aes(x = avg.lg2fc, y = -log10(pvalue))) +
        geom_point(aes(color = Significant.pvalue.avg_lg2FC_40),show.legend = FALSE) +
        scale_color_manual(values = c("blue","grey","red")) +xlab(str_c("log2 Fold Change\n","Sign Regions:",dim(grp2vgrp1.temp[grp2vgrp1.temp$Significant.pvalue.avg_lg2FC_40!="nosig",])))+ylab("-log10 (pvalue)")+ ggtitle(str_c(title,"Significant.pvalue.avg_lg2FC_40"))+
        theme_bw() + theme(legend.position = "bottom",axis.title= element_text(size=10,color="black", vjust=0.5, hjust=0.5),
        	axis.text= element_text(size=15, color="black", vjust=0.5, hjust=0.5),legend.text= element_text(size=15,  color="black", face= "bold", vjust=0.5, hjust=0.5)) +guides(fill=FALSE)
  ggsave(str_c(title,"DataMatrix_SignedFindMarkerspvalue.avg_lg2FCPCAIndependent_40regions.pdf"),plot1,width=8,height=6)


  return(grp2vgrp1)

}

## set the right library paths
library(optparse)
library(dplyr)
library(patchwork)
library(cowplot)
library(reshape2)
library(stringr)
library(ggpubr)
library(ggbiplot)
library(pheatmap)
library(ggplot2)
library(factoextra)
library(RColorBrewer)
library(scatterplot3d)
library(RCurl)#同时引入later包，需要打开X11
library(bitops)
library(later)
option_list = list(
    make_option(c("-p", "--path"), type = "character", default = "",  help = "a file of weeks data(row1:a group of mus(diff weeks); row2:another group of mus(diff weeks))"),
    make_option(c("-i", "--files"), type = "character", default = "",  help = "a file of weeks data(row1:a group of mus(diff weeks); row2:another group of mus(diff weeks))"),
    make_option(c("-t", "--samples_type1"), type = "character", default = "",  help = "a file of weeks data(row1:a group of mus(diff weeks); row2:another group of mus(diff weeks))"),
    make_option(c("-q", "--samples_type2"), type = "character", default = "",  help = "a file of weeks data(row1:a group of mus(diff weeks); row2:another group of mus(diff weeks))"),
    make_option(c("-f", "--foldchange_or_not"), type = "character", default = "yes",  help = "Option is yes/ no"),
    make_option(c("-s", "--test.use"), type = "character", default = "anova",  help = "Option is anova/ wilcoxon/ null"),
    make_option(c("-a", "--pvalue.threshold"), type = "double", default = 0.01,  help = "Pvalue cutoff for significant regions"),
    make_option(c("-d", "--lg2fc.threshold"), type = "integer", default = 1,  help = "Option is 1"),
    make_option(c("-o", "--output"), type = "character", default = "",  help = "Prefix of all files name")
)

parseobj = OptionParser(option_list=option_list)
opt = parse_args(parseobj)
path <- as.character(opt$path)
###########################################################################
###########################################################################
files <- opt$files
files.temp <- read.table(str_c(path,"/",as.character(files)),head=T,sep="\t", row.names=1)
files.temp1 <- as.data.frame(t(files.temp))
files.temp <- files.temp1
###########################################################################
########################################################################### 
# samples_type <- opt$samples_type
# files.temp <- files.temp[files.temp['task_label']==samples_type,]
# table(files.temp['sample_label'])
# samples_type1 <- files.temp[files.temp['sample_label']==as.character(opt$samples_type1), 1:(dim(files.temp)[2]-2)]
if(length(unlist(strsplit(opt$samples_type1, "\\|")))>1){
      samples_type1_tmp <- subset(files.temp, (sample_label==unlist(strsplit(opt$samples_type1, "\\|"))[1]) | (sample_label==unlist(strsplit(opt$samples_type1, "\\|"))[2]), select=c(-task_label, -sample_label))
}else{
      samples_type1_tmp <- subset(files.temp, sample_label==as.character(opt$samples_type1), select=c(-task_label, -sample_label))
}
samples_type1 <- samples_type1_tmp
# samples_type2 <- files.temp[files.temp['sample_label']==as.character(opt$samples_type2), 1:(dim(files.temp)[2]-2)]#files[,grep(samples_type_type2,colnames(files))]
if(length(unlist(strsplit(opt$samples_type2, "\\|")))>1){
      samples_type2_tmp <- subset(files.temp, (sample_label==unlist(strsplit(opt$samples_type2, "\\|"))[1]) | (sample_label==unlist(strsplit(opt$samples_type2, "\\|"))[2]), select=c(-task_label, -sample_label))
}else{
      samples_type2_tmp <- subset(files.temp, sample_label==as.character(opt$samples_type2), select=c(-task_label, -sample_label))
}
samples_type2 <- samples_type2_tmp
###########################################################################
#Option:#
#############################################intersect()##############################
foldchange_or_not <- as.character(opt$foldchange_or_not)
test.use <- as.character(opt$test.use)
pvalue.threshold <- as.numeric(opt$pvalue.threshold)
lg2fc.threshold <- as.numeric(opt$lg2fc.threshold)
Pseudocount=1/10000
###########################################################################
###########################################################################
setwd(str_c(path,"/UnsupervisedClustering/"))
title <- str_c(as.character(opt$output))
dir.create(title)
setwd(title)
###########################################################################
# Select regions and Default output:table and pvalueFC plot#
###########################################################################
D_samples_type1 <- as.data.frame(t(samples_type1))
D_samples_type2 <- as.data.frame(t(samples_type2))
D_samples_type2 <- D_samples_type2[, setdiff(colnames(D_samples_type2), c("A_yApc1", "A_yApc2", "A_yApc4","A_yApc5","A_yApc6"))]

D_samples_type1_1 <- c()
for(i in colnames(D_samples_type1)){
      D_samples_type1_1 <- cbind(D_samples_type1_1, as.numeric(as.character(as.matrix(D_samples_type1[, i]))))
}
D_samples_type1_1 <- as.data.frame(D_samples_type1_1)
colnames(D_samples_type1_1) <- colnames(D_samples_type1)
rownames(D_samples_type1_1) <- rownames(D_samples_type1)

D_samples_type2_1 <- c()
for(i in colnames(D_samples_type2)){
      D_samples_type2_1 <- cbind(D_samples_type2_1, as.numeric(as.character(as.matrix(D_samples_type2[, i]))))
}
D_samples_type2_1 <- as.data.frame(D_samples_type2_1)
colnames(D_samples_type2_1) <- colnames(D_samples_type2)
rownames(D_samples_type2_1) <- rownames(D_samples_type2)

grp2vgrp1 <- FindFeatures(D_samples_type1_1,D_samples_type2_1,test.use=test.use,lg2fc.threshold,pvalue.threshold,Pseudocount,title)

