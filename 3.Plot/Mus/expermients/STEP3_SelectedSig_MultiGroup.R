#!/usr/bin/env Rscript
fun_anova <- function(grp1_data,grp2_data,grp3_data,pvalue.threshold){
      p = NULL
      p_adj = NULL
      group_data <- cbind(grp1_data,grp2_data,grp3_data)
      group_data <- na.omit(group_data)#删除有NA的行
      # group_data <- group_data[apply(group_data,1,sum)!=0,]
      group_label <- as.factor(c(rep("Con",length(colnames(grp1_data))),rep("Sti",length(colnames(grp2_data))),rep("Sti2",length(colnames(grp3_data)))))
      for(i in 1:dim(group_data)[1]) { 
        group_data_i <- as.numeric(c(group_data[i,colnames(grp1_data)],group_data[i,colnames(grp2_data)],group_data[i,colnames(grp3_data)]))
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

fun_multi_anova <- function(grp1_data,grp2_data,grp3_data,pvalue.threshold,control_factors1,cancer_factors1,title){
      p = NULL
      scale_x.raw <- c()
      control.trainingset <- control
      cancer.trainingset <- cancer
      scale_x.raw <- cbind(control.trainingset,cancer.trainingset)
      scale_x.raw <- na.omit(scale_x.raw)#删除有NA的行
      # scale_x.raw <- scale_x.raw[apply(scale_x.raw,1,sum)!=0,]#删除全为0的行
      p.label = NULL
      p.factors1 = NULL
      p.connet = NULL
      factors1 <- c(control_factors1,cancer_factors1)
      # scale_x.raw <- scale_x.raw[apply(scale_x.raw,1,sum)!=0,]#删除全为0的行
      for(i in 1:dim(scale_x.raw)[1]) { 
        aov.control <- scale_x.raw[i,colnames(control.trainingset)]
        aov.cancer <- scale_x.raw[i,colnames(cancer.trainingset)]
        aov.2type <- c(aov.control,aov.cancer)
        aov.label <- as.factor(c(rep("control",length(colnames(control.trainingset))),rep("cancer",length(colnames(cancer.trainingset)))))
        aov.data.frame <- data.frame(as.numeric(aov.2type),aov.label,factors1)
        colnames(aov.data.frame) <- c("aov.2type","aov.label","aov.factors1")
        aov.test <- aov(aov.2type ~ aov.label*aov.factors1,data=aov.data.frame)
        aov.summary <- summary(aov.test)
        # label 对应的pvalue
        p.label<-c(p.label,as.numeric(unlist(aov.summary)["Pr(>F)1"])) 
        # factors1 对应的pvalue
        p.factors1 <-c(p.factors1,as.numeric(unlist(aov.summary)["Pr(>F)2"])) 
        # connet 对应的pvalue
        p.connet <-c(p.connet,as.numeric(unlist(aov.summary)["Pr(>F)3"])) 
      }
      names(p.label) <- rownames(scale_x.raw)
      names(p.factors1) <- rownames(scale_x.raw)
      names(p.connet) <- rownames(scale_x.raw)
      scale_x.pvalue2 <- as.data.frame(cbind(scale_x.raw,p.label,p.factors1,p.connet))#带pvalue的总表
      write.table(scale_x.pvalue2,str_c(title,"/original_Matrix_PvalueFromTwowayAnova",title,".log"),sep="\t",quote=F,row.names=T)

      p_value = pvalue.threshold
      diff_PValue2 <- scale_x.pvalue2[scale_x.pvalue2$p.label < p_value,]
      diff_PValue2.no <- scale_x.pvalue2[scale_x.pvalue2$p.label > p_value,]
      print(length(rownames(diff_PValue2)));print(length(rownames(diff_PValue2.no)))
      plabel_diff_PValue2 <- rownames(diff_PValue2)
      # factors1
      p_value = pvalue.threshold
      diff_PValue2 <- scale_x.pvalue2[scale_x.pvalue2$p.factors1 < p_value,]
      diff_PValue2.no <- scale_x.pvalue2[scale_x.pvalue2$p.factors1 > p_value,]
      print(length(rownames(diff_PValue2)));print(length(rownames(diff_PValue2.no)))
      pweeks_diff_PValue2 <- rownames(diff_PValue2)
      diff_PValue2 <- setdiff(plabel_diff_PValue2,pweeks_diff_PValue2)
      hmMat21 <- scale_x.raw[diff_PValue2,]#pvalue符合要求的矩阵，前control后cancer
      print(str_c("只有原始的Control和DEN样品的：","Samples_num:",dim(hmMat21)[2],";Features_num:",dim(hmMat21)[1]))
      return(hmMat21)
}
fun_wilcoxon <- function(grp1_data,grp2_data,grp3_data,pvalue.threshold,title){
      # PValue
      control.trainingset <- control
      cancer.trainingset <- cancer
      p = NULL
      scale_x.raw <- cbind(control.trainingset,cancer.trainingset)
      scale_x.raw <- na.omit(scale_x.raw)#删除有NA的行
      scale_x.raw <- scale_x.raw[apply(scale_x.raw,1,sum)!=0,]#删除全为0的行
      wilcox.x = c()
      for(i in 1:dim(scale_x.raw)[1]) { 
            wilcox.x <-wilcox.test(as.numeric(scale_x.raw[i,colnames(control.trainingset)]),as.numeric(scale_x.raw[i,colnames(cancer.trainingset)]))
            p<-c(p,wilcox.x$p.value) 
      }
      names(p) <- rownames(scale_x.raw)
      scale_x.pvalue2 <- as.data.frame(cbind(scale_x.raw,p))
      write.table(scale_x.pvalue2,str_c(title,"/original_Matrix_PvalueFromWilcox",title,".log"),sep="\t",quote=F,row.names=T)
      p_value = pvalue.threshold
      diff_PValue2 <- scale_x.pvalue2[scale_x.pvalue2$p < p_value,]
      diff_PValue2.no <- scale_x.pvalue2[scale_x.pvalue2$p > p_value,]
      length(rownames(diff_PValue2));length(rownames(diff_PValue2.no))
      hmMat21 <- cbind(control.trainingset,cancer.trainingset)[rownames(diff_PValue2),]#pvalue符合要求的矩阵，前control后cancer
      return(hmMat21)
}

plot_PCA <- function(data,grp1_data,grp2_data,grp3_data,select_features,pic_name){
  label_subtitle <- str_c("Total:",length(select_features),"---",pic_name)
  df <- data[select_features,colnames(cbind(grp1_data,grp2_data,grp3_data))]
  colnames(df) <- gsub("_DENinduction|_Control|R.100000|_Mus_APC_HFD_","",colnames(df))
  #对数据进行转置
  newtest<-as.data.frame(t(df))
  gene_exp.pca <- prcomp(newtest,center = T,scale= T)
  p1 <- fviz_pca_ind(gene_exp.pca,geom.ind = "point",pointshape = 21,pointsize = 2.5, title = pic_name,subtitle = str_c(label_subtitle,"\n","center = T,scale= T"), # show points only (nbut not "text")
          fill.ind =c(rep("Con",dim(grp1_data)[2]),rep("DEN",dim(grp2_data)[2]),rep("APC",dim(grp3_data)[2])), # color by groups
          palette = c("green4","blue4", "red4"),
          addEllipses = FALSE,mean.point=TRUE,#gradient.cols = "RdYlBu", Concentration ellipses
          legend.title = "Groups"
          )
  gene_exp.pca <- prcomp(newtest,center = F,scale= F)
  p2 <- fviz_pca_ind(gene_exp.pca,geom.ind = "point",pointshape = 21,pointsize = 2.5, title = pic_name,subtitle = str_c(label_subtitle,"\n","center = F,scale= F"), # show points only (nbut not "text")
          fill.ind =c(rep("Con",dim(grp1_data)[2]),rep("DEN",dim(grp2_data)[2]),rep("APC",dim(grp3_data)[2])), # color by groups
          palette = c("green4","blue4", "red4"),
          addEllipses = FALSE,mean.point=TRUE,#gradient.cols = "RdYlBu", Concentration ellipses
          legend.title = "Groups"
          )
  gene_exp.pca <- prcomp(newtest,center = T,scale= F)
  p3 <- fviz_pca_ind(gene_exp.pca,geom.ind = "text",pointshape = 21,pointsize = 2.5, title = pic_name,subtitle = str_c(label_subtitle,"\n","center = T,scale= F"), # show points only (nbut not "text")
          fill.ind =c(rep("Con",dim(grp1_data)[2]),rep("DEN",dim(grp2_data)[2]),rep("APC",dim(grp3_data)[2])), # color by groups
          palette = c("green4","blue4", "red4"),
          addEllipses = FALSE,mean.point=TRUE,#gradient.cols = "RdYlBu", Concentration ellipses
          legend.title = "Groups"
          ) 
  gene_exp.pca <- prcomp(newtest,center = F,scale= T)
  p4 <- fviz_pca_ind(gene_exp.pca,geom.ind = "point",pointshape = 21,pointsize = 2.5, title = pic_name,subtitle = str_c(label_subtitle,"\n","center = F,scale= T","\n","grp1:",colnames(grp1_data)[1],"\ngrp2:",colnames(grp2_data)[1],"\ngrp3:",colnames(grp3_data)[1]), # show points only (nbut not "text")
          fill.ind =c(rep("Con",dim(grp1_data)[2]),rep("DEN",dim(grp2_data)[2]),rep("APC",dim(grp3_data)[2])), # color by groups
          palette = c("green4","blue4", "red4"),
          addEllipses = FALSE,mean.point=TRUE,#gradient.cols = "RdYlBu", Concentration ellipses
          legend.title = "Groups"
          )

  p <- p1+p2+p3+p4
  return(p)
}

plot_PCA_Independent <- function(data,grp1_data,grp2_data,grp3_data,select_features,pic_name){
  label_subtitle <- str_c("Total:",length(select_features),"---",pic_name)
  df <- data[select_features,colnames(cbind(grp1_data,grp2_data,grp3_data))]
  #对数据进行转置
  newtest<-as.data.frame(t(df))

  df_grp1_data_t <- as.data.frame(t(data[select_features,colnames(grp1_data)]))
  df_grp1_data_t$Species <- rep("Con",dim(grp1_data)[2])
  diff_weeks <- c()
  for(i in 1:length(strsplit(rownames(df_grp1_data_t),"\\."))){
    diff_weeks <- c(diff_weeks,gsub("X01_C57_|X02_C57_|X0_C57_|X03_C57_|X1_C57_|X2_C57_|X3_C57_|Mus_C57_|M426_|M427_|M428_|M429_|M430_|M431_|M432_|M433_|M434_|M435_","",unlist(strsplit(rownames(df_grp1_data_t),"\\.")[i])[1]))
  }
  df_grp1_data_t$weeks <- gsub("Control_","Con_",diff_weeks)
  df_grp1_data_t$weeks_num <- as.numeric(gsub("Con_|weeks","",df_grp1_data_t$weeks[grep("Con_",df_grp1_data_t$weeks)]))
  df_grp1_data_t <- df_grp1_data_t[order(df_grp1_data_t$weeks_num),]
  df_grp2_data_t <- as.data.frame(t(data[select_features,colnames(grp2_data)]))
  df_grp2_data_t$Species <- rep("Sti",dim(grp2_data)[2])
  diff_weeks <- c()
  for(i in 1:length(strsplit(rownames(df_grp2_data_t),"\\."))){
    diff_weeks <- c(diff_weeks,gsub("X01_C57_|X02_C57_|X0_C57_|X03_C57_|X1_C57_|X2_C57_|X3_C57_|Mus_C57_|M426_|M427_|M428_|M429_|M430_|M431_|M432_|M433_|M434_|M435_","",unlist(strsplit(rownames(df_grp2_data_t),"\\.")[i])[1]))
  }
  df_grp2_data_t$weeks <- gsub("DENinduction_","Sti_",diff_weeks)
  df_grp2_data_t$weeks_num <- as.numeric(gsub("Sti_|weeks","",df_grp2_data_t$weeks[grep("Sti_",df_grp2_data_t$weeks)]))
  df_grp2_data_t <- df_grp2_data_t[order(df_grp2_data_t$weeks_num),]

  df_data_t <- rbind(df_grp1_data_t,df_grp2_data_t)
  real_order <- unique(c(str_c("Con_",df_grp1_data_t$weeks_num,"weeks"),str_c("Sti_",df_grp2_data_t$weeks_num,"weeks")))
  df_data_t$order <- 1:dim(df_data_t)[1]
  gene_exp.pca <- prcomp(df_data_t[,setdiff(colnames(df_data_t),c("Species","weeks","weeks_num","order"))],center = T,scale= T)

  a <- cbind(gene_exp.pca$x[,1:2],df_data_t[,c("weeks","Species","order")])
  gd<- a %>% group_by(Species) %>% summarise(PC1 = mean(PC1),PC2  = mean(PC2))
  p5 <- ggplot(a, aes(x=PC1, y=PC2)) + 
    geom_point(aes(size=factor(a$weeks,levels=real_order),shape=factor(a$weeks,levels=real_order),fill=factor(a$weeks,levels=real_order)))+
    scale_shape_manual(values=c(rep(21,length(real_order[grep("Con_",real_order)])),rep(24,length(real_order[grep("Sti_",real_order)]))))+
    scale_fill_manual(values=c(colorRampPalette(brewer.pal(9, "Oranges"))(16)[6:(5+length(real_order[grep("Con_",real_order)]))],colorRampPalette(brewer.pal(9, "Greens"))(12)[6:(5+round(length(real_order[grep("Sti_",real_order)])/2,0))],colorRampPalette(brewer.pal(9, "Blues"))(12)[8:(7+(length(real_order[grep("Sti_",real_order)])-round(length(real_order[grep("Sti_",real_order)])/2,0)))]))+
    scale_size_manual(values=rep(4,22))+
    # geom_point(data = (gd),size = 10,color=("red4"),shape=21)+geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.3)+
    geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.3) +
    labs(x = str_c("PC1 : ",round(get_eigenvalue(gene_exp.pca)[1,"variance.percent"],2),"%"),y=str_c("PC2 : ",round(get_eigenvalue(gene_exp.pca)[2,"variance.percent"],2),"%"))+
    ggtitle("mouse model (C57BL/6J)")+theme_bw()+
    theme(axis.title= element_text(size=15,color="black", face= "bold", vjust=0.5, hjust=0.5),plot.title = element_text(size=15,color="black", face= "bold", vjust=0.5, hjust=0.5),
    axis.text= element_text(size=15, color="black", vjust=0.5, hjust=0.5),legend.title = element_blank()) 
  return(p5)
}

FindFeatures <- function(grp1_data,grp2_data,grp3_data,test.use,lg2fc.threshold,pvalue.threshold,Pseudocount,title){
  # input:grp1_data[con] and grp2_data[sti]
  # test_method:anova, milti-anova, wilcox
  # Pseudocount:1/bin size [尾基数假设每个区间有一个bp的覆盖]
  # output:dataset1, dataset2, avg_logFC(log fold-chage of the average expression between the two groups.[dataset2/dataset1]), p_val_adj(Adjusted p-value, based on bonferroni correction)
  test_p_result <- c()
  control_versus_treat_selected_regions_intersect <- c()
  grp1_data <- as.data.frame(grp1_data)
  grp2_data <- as.data.frame(grp2_data)
  grp3_data <- as.data.frame(grp3_data)
  if(test.use=="anova"){
    test_p_result <- fun_anova(grp1_data,grp2_data,grp3_data,pvalue.threshold)
  }else if(test.use=="wilcoxon"){
    test_p_result <- fun_wilcoxon(grp1_data,grp2_data,grp3_data,pvalue.threshold,title)
  }else if(test.use=="null"){
    test_p_result <- as.data.frame(cbind(grp1_data,grp2_data,grp3_data),title)
  }else if(test.use=="multi_anova"){
    control_factors1 <- c()
    for(i in 1:length(strsplit(colnames(control),"_"))){
      control_factors1 <- c(control_factors1,unlist(strsplit(colnames(control),"_")[i])[length(unlist(strsplit(colnames(control),"_")[i]))])
    }
    cancer_factors1 <- c()
    for(i in 1:length(strsplit(colnames(cancer),"_"))){
      cancer_factors1 <- c(cancer_factors1,unlist(strsplit(colnames(cancer),"_")[i])[length(unlist(strsplit(colnames(cancer),"_")[i]))])
    }
    control_factors1 <- gsub(".rep1|.rep2|.rep3","",control_factors1)
    cancer_factors1 <- gsub(".rep1|.rep2|.rep3","",cancer_factors1)
    control_cancer.anova <- fun_multi_anova(control,cancer,pvalue.threshold,control_factors1,cancer_factors1,title)
  }
  # avg_lg2FC <- (log2((apply(grp2_data,1,mean)+Pseudocount)/(apply(grp1_data,1,mean)+Pseudocount)))
  # med_lg2FC <- (log2((apply(grp2_data,1,median)+Pseudocount)/(apply(grp1_data,1,median)+Pseudocount)))
  grp2vgrp1 <- as.data.frame(test_p_result)
  ####################################
  # sign the significant regions
  ####################################
  pvalue = grp2vgrp1[,(dim(grp2vgrp1)[2]-1)]
  padj = grp2vgrp1[,(dim(grp2vgrp1)[2])]

  ####################################
  # Output3: 
  ####################################
  grp2vgrp1.temp <- cbind(grp2vgrp1,pvalue,padj)
  data_res_method="mean"
  select_features=rownames(na.omit(grp2vgrp1.temp[(grp2vgrp1.temp$pvalue)<pvalue.threshold,]))
  # mean_pvalue1_plot <- plot_box(grp2vgrp1.temp,grp1_data,grp2_data,grp3_data,select_features,data_res_method)
  select_features=rownames(na.omit(grp2vgrp1.temp[(grp2vgrp1.temp$padj)<pvalue.threshold,]))
  # mean_pvalue2_plot <- plot_box(grp2vgrp1.temp,grp1_data,grp2_data,grp3_data,select_features,data_res_method)
  data_res_method="median"
  select_features=rownames(na.omit(grp2vgrp1.temp[(grp2vgrp1.temp$pvalue)<pvalue.threshold,]))
  # median_pvalue1_plot <- plot_box(grp2vgrp1.temp,grp1_data,grp2_data,grp3_data,select_features,data_res_method)
  select_features=rownames(na.omit(grp2vgrp1.temp[(grp2vgrp1.temp$padj)<pvalue.threshold,]))
  # median_pvalue2_plot <- plot_box(grp2vgrp1.temp,grp1_data,grp2_data,grp3_data,select_features,data_res_method)
  # pg <- plot_grid(mean_pvalue1_plot, mean_pvalue2_plot,median_pvalue1_plot,median_pvalue2_plot, ncol=2, rel_heights=c(2, 2), align="v", label_size=17, hjust=10)

  # ggsave(str_c(title,"DataMatrix_SignedFindMarkers_Boxplot.pdf"),pg,width=7,height=7)
  ####################################
  # Output4: 
  ####################################
  grp2vgrp1.temp <- cbind(grp2vgrp1,pvalue,padj)
  select_features=rownames(na.omit(grp2vgrp1.temp[(grp2vgrp1.temp$pvalue)<pvalue.threshold,]))
  p1=plot_PCA(grp2vgrp1.temp,grp1_data,grp2_data,grp3_data,select_features,"Only Pvalue")
  ggsave(str_c(title,"DataMatrix_SignedFindMarkers_OnlyPvaluePCA.pdf"),p1,width=10,height=10)
  # p=plot_PCA_Independent(grp2vgrp1.temp,grp1_data,grp2_data,grp3_data,select_features,"Only Pvalue")
  # ggsave(str_c(title,"DataMatrix_SignedFindMarkers_OnlyPvaluePCAIndependent.pdf"),p,width=11,height=8)

  select_features=rownames(na.omit(grp2vgrp1.temp[(grp2vgrp1.temp$padj)<pvalue.threshold,]))
  p2=plot_PCA(grp2vgrp1.temp,grp1_data,grp2_data,grp3_data,select_features,"Only Padj")
  ggsave(str_c(title,"DataMatrix_SignedFindMarkers_OnlyPadjPCA.pdf"),p2,width=10,height=10)
  # p=plot_PCA_Independent(grp2vgrp1.temp,grp1_data,grp2_data,grp3_data,select_features,"Only Padj")
  # ggsave(str_c(title,"DataMatrix_SignedFindMarkers_OnlyPadjPCAIndependent.pdf"),p,width=11,height=8)

  return(grp2vgrp1)

}

## set the right library paths

library(optparse)
library(dplyr)
library(patchwork)
library(cowplot)
# library(reshape2)
library(ggpubr)
library(stringr)
#library(devtools)
library(ggbiplot)
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
    # make_option(c("-r", "--other_files"), type = "character", default = ".Mus_AvailableData/agematched_APC_WT.NorDataSetSamplesInfo.log",  help = "a file of weeks data(row1:a group of mus(diff weeks); row2:another group of mus(diff weeks))"),
    # make_option(c("-f", "--foldchange_or_not"), type = "character", default = "yes",  help = "Option is yes/ no"),
    # make_option(c("-d", "--week_control"), type = "character", default = '_20weeks|_24weeks|_28weeks',  help = "Option is Control/ DENinduction"),
    # make_option(c("-e", "--week_cancer"), type = "character", default = '_20weeks|_24weeks|_28weeks',  help = "Option is Control/ DENinduction"),
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
# files.temp <- files.temp[files.temp['task_label']==samples_type,]
# table(files.temp['sample_label'])
samples_type11 <- files.temp[files.temp['sample_label']=='A_Ctrl', 1:(dim(files.temp)[2]-2)]#files[,grep(samples_type_type1,colnames(files))]
samples_type12 <- files.temp[files.temp['sample_label']=='D_Ctrl', 1:(dim(files.temp)[2]-2)]#files[,grep(samples_type_type1,colnames(files))]
samples_type1 <- rbind(samples_type11, samples_type12)
samples_type2 <- files.temp[files.temp['sample_label']=='A_Apc', 1:(dim(files.temp)[2]-2)]#files[,grep(samples_type_type2,colnames(files))]
samples_type3 <- files.temp[files.temp['sample_label']=='D_tDEN', 1:(dim(files.temp)[2]-2)]#files[,grep(samples_type_type2,colnames(files))]
###########################################################################
#Option:#
#############################################intersect()##############################
# foldchange_or_not <- as.character(opt$foldchange_or_not)
test.use <- as.character(opt$test.use)
pvalue.threshold <- as.numeric(opt$pvalue.threshold)
lg2fc.threshold <- as.numeric(opt$lg2fc.threshold)
Pseudocount=1/bin.size
###########################################################################
###########################################################################
setwd(path)
title <- str_c(as.character(opt$output))
dir.create(title)
setwd(title)
###########################################################################
# normalization or not#
###########################################################################
# if regions divided by 10000 bins, scale factor = 100000
# if regions divided by 100000 bins, scale factor = 10000
D_samples_type1 <- as.data.frame(t(samples_type1))
D_samples_type2 <- as.data.frame(t(samples_type2))
D_samples_type3 <- as.data.frame(t(samples_type3))
print(str_c("num:",dim(D_samples_type1)[2]))
print(str_c("num:",dim(D_samples_type2)[2]))
print(str_c("num:",dim(D_samples_type3)[2]))


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


D_samples_type1_1 <- c()
for(i in colnames(D_samples_type1)){
      D_samples_type1_1 <- cbind(D_samples_type1_1, as.numeric(as.character(as.matrix(D_samples_type1[, i]))))
}
D_samples_type1_1 <- as.data.frame(D_samples_type1_1)
colnames(D_samples_type1_1) <- colnames(D_samples_type1)
rownames(D_samples_type1_1) <- rownames(D_samples_type1)

D_samples_type3_1 <- c()
for(i in colnames(D_samples_type3)){
      D_samples_type3_1 <- cbind(D_samples_type3_1, as.numeric(as.character(as.matrix(D_samples_type3[, i]))))
}
D_samples_type3_1 <- as.data.frame(D_samples_type3_1)
colnames(D_samples_type3_1) <- colnames(D_samples_type3)
rownames(D_samples_type3_1) <- rownames(D_samples_type3)
###########################################################################
# Select regions and Default output:table and pvalueFC plot#
###########################################################################
grp2vgrp1 <- FindFeatures(D_samples_type1_1,D_samples_type2_1,D_samples_type3_1,test.use=test.use,lg2fc.threshold,pvalue.threshold,Pseudocount,title)


