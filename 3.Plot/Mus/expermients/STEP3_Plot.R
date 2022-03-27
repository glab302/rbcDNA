## set the right library paths
pheatmap_f_g2 <- function(groups_label, grp1_data, grp2_data, pic_name){
    pdf(pic_name,width=8,height=8)
    
    sample_group_a=unlist(strsplit(groups_label,"_"))[1]
    sample_group_b=unlist(strsplit(groups_label,"_"))[2]
    # diff types numbers
    print(str_c("GroupA-sample:",dim(grp1_data)[2],";  GroupB-sample:",dim(grp2_data)[2]))
    # diff types numbers
    hmMat21 <- cbind(grp1_data, grp2_data)
    hmMat21 <- apply(hmMat21,2,as.numeric); dim(hmMat21)
    aka2 = data.frame(SampleType = factor(c(rep(str_c(sample_group_a," :",dim(grp1_data)[2]),dim(grp1_data)[2]),
                                            rep(str_c(sample_group_b," :",dim(grp2_data)[2]),dim(grp2_data)[2]))))
    rownames(aka2)<- colnames(hmMat21)
    aka3 = list(SampleType = c(GroupA ="#deebf7",GroupB="#fc9272"))
    names(aka3$SampleType) <- c(str_c(sample_group_a," :",dim(grp1_data)[2]),str_c(sample_group_b," :",dim(grp2_data)[2]))
    dista <- c('correlation', 'euclidean', 'maximum', 'manhattan', 'canberra','binary', 'minkowski')#'euclidean'
    for(i in 1:length(dista)){
      bk <- c(seq(-1.5,-0.1,by=0.01),seq(0,1.5,by=0.01))
      print(str_c("height:",225/dim(hmMat21)[2]))
      print(str_c("width:",300/dim(hmMat21)[1]))
      aa=pheatmap(hmMat21,scale="row",cluster_rows =T,cluster_cols = T,annotation_col = aka2,color = c(colorRampPalette(colors = c("#084594","#08519c","white"))(length(bk)/2),
                  colorRampPalette(colors = c("white","#cb181d","firebrick3"))(length(bk)/2)),legend_breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5),main=dista[i],
                  legend_labels=c('-1.5','-1','-0.5','0','0.5','1','1.5'),breaks=bk, annotation_colors = aka3[1],annotation_legend = TRUE,
                  annotation_names_row=F,treeheight_row=20, treeheight_col=20,cellheight =225/dim(hmMat21)[1],cellwidth =300/dim(hmMat21)[2],
                  clustering_distance_cols =dista[i],clustering_distance_row =dista[i], fontsize=10,show_rownames=F,show_colnames=T,fontsize_col=5,cex.lab=1)
      aa
      aa=pheatmap(hmMat21,scale="row",cluster_rows =T,cluster_cols = T,annotation_col = aka2,color = c(colorRampPalette(colors = c("#084594","#08519c","white"))(length(bk)/2),
                  colorRampPalette(colors = c("white","#cb181d","firebrick3"))(length(bk)/2)),#legend_breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5), 
                  legend_labels=F,breaks=bk, annotation_colors = aka3[1],annotation_legend = F,annotation_names_col=F,familyfont="Arial",
                  annotation_names_row=F,treeheight_row=20, treeheight_col=40,cellheight =225/dim(hmMat21)[1],cellwidth =300/dim(hmMat21)[2],clustering_distance_cols =dista[i],clustering_distance_row =dista[i], fontsize=20,show_rownames=F,show_colnames=F,fontsize_col=5,cex.lab=1)
      aa
    }
    dev.off()
}

pheatmap_f_g3 <- function(groups_label, grp1_data, grp2_data, grp3_data, pic_name){
    pdf(pic_name,width=8,height=8)
    sample_group_a=unlist(strsplit(groups_label,"_"))[1]
    sample_group_b=unlist(strsplit(groups_label,"_"))[2]
    sample_group_c=unlist(strsplit(groups_label,"_"))[3]
    # diff types numbers
    print(str_c("GroupA-sample:",dim(grp1_data)[2],";  GroupB-sample:",dim(grp2_data)[2],";  GroupC-sample:",dim(grp3_data)[2]))
    # diff types numbers
    hmMat21 <- cbind(grp1_data, grp2_data, grp3_data)
    hmMat21 <- apply(hmMat21,2,as.numeric); dim(hmMat21)
    aka2 = data.frame(SampleType = factor(c(rep(str_c(sample_group_a," :",dim(grp1_data)[2]),dim(grp1_data)[2]),
                                            rep(str_c(sample_group_b," :",dim(grp2_data)[2]),dim(grp2_data)[2]),
                                            rep(str_c(sample_group_c," :",dim(grp3_data)[2]),dim(grp3_data)[2]))))
    rownames(aka2)<- colnames(hmMat21)
    aka3 = list(SampleType = c(GroupA ="#deebf7",GroupB="#fc9272",GroupC="#EF3B2C"))
    names(aka3$SampleType) <- c(str_c(sample_group_a," :",dim(grp1_data)[2]),str_c(sample_group_b," :",dim(grp2_data)[2]),str_c(sample_group_c," :",dim(grp3_data)[2]))
    dista <- c('correlation', 'euclidean', 'maximum', 'manhattan', 'canberra','binary', 'minkowski')#'euclidean'
    for(i in 1:length(dista)){
      bk <- c(seq(-1.5,-0.1,by=0.01),seq(0,1.5,by=0.01))
      print(str_c("height:",225/dim(hmMat21)[2]))
      print(str_c("width:",300/dim(hmMat21)[1]))
      aa=pheatmap(hmMat21,scale="row",cluster_rows =T,cluster_cols = T,annotation_col = aka2,color = c(colorRampPalette(colors = c("#084594","#08519c","white"))(length(bk)/2),
                  colorRampPalette(colors = c("white","#cb181d","firebrick3"))(length(bk)/2)),legend_breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5), 
                  legend_labels=c('-1.5','-1','-0.5','0','0.5','1','1.5'),breaks=bk, annotation_colors = aka3[1],annotation_legend = TRUE,main=dista[i],
                  annotation_names_row=F,treeheight_row=20, treeheight_col=20,cellheight =225/dim(hmMat21)[1],cellwidth =300/dim(hmMat21)[2],
                  clustering_distance_cols =dista[i],clustering_distance_row =dista[i], fontsize=10,show_rownames=F,show_colnames=T,fontsize_col=5,cex.lab=1)
      aa
      aa=pheatmap(hmMat21,scale="row",cluster_rows =F,cluster_cols = T,annotation_col = aka2,color = c(colorRampPalette(colors = c("#084594","#08519c","white"))(length(bk)/2),
                  colorRampPalette(colors = c("white","#cb181d","firebrick3"))(length(bk)/2)),#legend_breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5), 
                  legend_labels=F,breaks=bk, annotation_colors = aka3[1],annotation_legend = F,annotation_names_col=F,familyfont="Arial",
                  annotation_names_row=F,treeheight_row=20, treeheight_col=40,cellheight =225/dim(hmMat21)[1],cellwidth =300/dim(hmMat21)[2],clustering_distance_cols =dista[i],clustering_distance_row =dista[i], fontsize=20,show_rownames=F,show_colnames=F,fontsize_col=5,cex.lab=1)
      aa
    }
    dev.off()
}

pheatmap_f_g4 <- function(groups_label, grp1_data, grp2_data, grp3_data, grp4_data, pic_name){
    pdf(pic_name,width=8,height=8)
    
    sample_group_a=unlist(strsplit(groups_label,"_"))[1]
    sample_group_b=unlist(strsplit(groups_label,"_"))[2]
    sample_group_c=unlist(strsplit(groups_label,"_"))[3]
    sample_group_d=unlist(strsplit(groups_label,"_"))[4]
    # diff types numbers
    print(str_c("GroupA-sample:",dim(grp1_data)[2],";  GroupB-sample:",dim(grp2_data)[2],";  GroupC-sample:",dim(grp3_data)[2],";  GroupD-sample:",dim(grp4_data)[2]))
    # diff types numbers
    hmMat21 <- cbind(grp1_data, grp2_data, grp3_data, grp4_data)
    hmMat21 <- apply(hmMat21,2,as.numeric); dim(hmMat21)
    aka2 = data.frame(SampleType = factor(c(rep(str_c(sample_group_a," :",dim(grp1_data)[2]),dim(grp1_data)[2]),
                                            rep(str_c(sample_group_b," :",dim(grp2_data)[2]),dim(grp2_data)[2]),
                                            rep(str_c(sample_group_c," :",dim(grp3_data)[2]),dim(grp3_data)[2]),
                                            rep(str_c(sample_group_d," :",dim(grp4_data)[2]),dim(grp4_data)[2]))))
    rownames(aka2)<- colnames(hmMat21)
    aka3 = list(SampleType = c(GroupA ="#deebf7",GroupB="#fc9272",GroupC="#7FB5D5",GroupD="#BD4502"))
    names(aka3$SampleType) <- c(str_c(sample_group_a," :",dim(grp1_data)[2]),str_c(sample_group_b," :",dim(grp2_data)[2]),str_c(sample_group_c," :",dim(grp3_data)[2]),str_c(sample_group_d," :",dim(grp4_data)[2]))
    dista <- c('correlation', 'euclidean', 'maximum', 'manhattan', 'canberra','binary', 'minkowski')#'euclidean'
    for(i in 1:length(dista)){
      bk <- c(seq(-1.5,-0.1,by=0.01),seq(0,1.5,by=0.01))
      print(str_c("height:",225/dim(hmMat21)[2]))
      print(str_c("width:",300/dim(hmMat21)[1]))
      aa=pheatmap(hmMat21,scale="row",cluster_rows =T,cluster_cols = T,annotation_col = aka2,color = c(colorRampPalette(colors = c("#084594","#08519c","white"))(length(bk)/2),
                  colorRampPalette(colors = c("white","#cb181d","firebrick3"))(length(bk)/2)),legend_breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5), 
                  legend_labels=c('-1.5','-1','-0.5','0','0.5','1','1.5'),breaks=bk, annotation_colors = aka3[1],annotation_legend = TRUE,
                  annotation_names_row=F,treeheight_row=20, treeheight_col=20,cellheight =225/dim(hmMat21)[1],cellwidth =300/dim(hmMat21)[2],main=dista[i],
                  clustering_distance_cols =dista[i],clustering_distance_row =dista[i], fontsize=10,show_rownames=F,show_colnames=T,fontsize_col=5,cex.lab=1)
      aa
      aa=pheatmap(hmMat21,scale="row",cluster_rows =F,cluster_cols = T,annotation_col = aka2,color = c(colorRampPalette(colors = c("#084594","#08519c","white"))(length(bk)/2),
                  colorRampPalette(colors = c("white","#cb181d","firebrick3"))(length(bk)/2)),#legend_breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5), 
                  legend_labels=F,breaks=bk, annotation_colors = aka3[1],annotation_legend = F,annotation_names_col=F,familyfont="Arial",
                  annotation_names_row=F,treeheight_row=20, treeheight_col=40,cellheight =225/dim(hmMat21)[1],cellwidth =300/dim(hmMat21)[2],clustering_distance_cols =dista[i],clustering_distance_row =dista[i], 
                  fontsize=20,show_rownames=F,show_colnames=F,fontsize_col=5,cex.lab=1)
      aa
    }
    dev.off()

}

pheatmap_f_g6 <- function(groups_label, grp1_data, grp2_data, grp3_data, grp4_data, grp5_data, grp6_data, pic_name){
    pdf(pic_name,width=8,height=8)
    
    sample_group_a=unlist(strsplit(groups_label,"_"))[1]
    sample_group_b=unlist(strsplit(groups_label,"_"))[2]
    sample_group_c=unlist(strsplit(groups_label,"_"))[3]
    sample_group_d=unlist(strsplit(groups_label,"_"))[4]
    sample_group_e=unlist(strsplit(groups_label,"_"))[5]
    sample_group_f=unlist(strsplit(groups_label,"_"))[6]
    # diff types numbers
    print(str_c("GroupA-sample:",dim(grp1_data)[2],";  GroupB-sample:",dim(grp2_data)[2],";  GroupC-sample:",dim(grp3_data)[2],";  GroupD-sample:",dim(grp4_data)[2]))
    # diff types numbers
    hmMat21 <- cbind(grp1_data, grp2_data, grp3_data, grp4_data, grp5_data, grp6_data)
    hmMat21 <- apply(hmMat21,2,as.numeric); dim(hmMat21)
    aka2 = data.frame(SampleType = factor(c(rep(str_c(sample_group_a," :",dim(grp1_data)[2]),dim(grp1_data)[2]),
                                            rep(str_c(sample_group_b," :",dim(grp2_data)[2]),dim(grp2_data)[2]),
                                            rep(str_c(sample_group_c," :",dim(grp3_data)[2]),dim(grp3_data)[2]),
                                            rep(str_c(sample_group_d," :",dim(grp4_data)[2]),dim(grp4_data)[2]),
                                            rep(str_c(sample_group_e," :",dim(grp5_data)[2]),dim(grp5_data)[2]),
                                            rep(str_c(sample_group_f," :",dim(grp6_data)[2]),dim(grp6_data)[2]))))
    rownames(aka2)<- colnames(hmMat21)
    aka3 = list(SampleType = c(GroupA ="#deebf7",GroupB="#fc9272",GroupC="#7FB5D5",GroupD="#BD4502",GroupE="#08519C",GroupF="orange"))
    names(aka3$SampleType) <- c(str_c(sample_group_a," :",dim(grp1_data)[2]),
                                str_c(sample_group_b," :",dim(grp2_data)[2]),
                                str_c(sample_group_c," :",dim(grp3_data)[2]),
                                str_c(sample_group_d," :",dim(grp4_data)[2]),
                                str_c(sample_group_e," :",dim(grp5_data)[2]),
                                str_c(sample_group_f," :",dim(grp6_data)[2]))
    dista <- c('correlation', 'euclidean', 'maximum', 'manhattan', 'canberra','binary', 'minkowski')#'euclidean'
    for(i in 1:length(dista)){
      bk <- c(seq(-1.5,-0.1,by=0.01),seq(0,1.5,by=0.01))
      print(str_c("height:",225/dim(hmMat21)[2]))
      print(str_c("width:",300/dim(hmMat21)[1]))
      aa=pheatmap(hmMat21,scale="row",cluster_rows =T,cluster_cols = T,annotation_col = aka2,color = c(colorRampPalette(colors = c("#084594","#08519c","white"))(length(bk)/2),
                  colorRampPalette(colors = c("white","#cb181d","firebrick3"))(length(bk)/2)),legend_breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5), 
                  legend_labels=c('-1.5','-1','-0.5','0','0.5','1','1.5'),breaks=bk, annotation_colors = aka3[1],annotation_legend = TRUE,
                  annotation_names_row=F,treeheight_row=20, treeheight_col=20,cellheight =225/dim(hmMat21)[1],cellwidth =300/dim(hmMat21)[2],main=dista[i],
                  clustering_distance_cols =dista[i],clustering_distance_row =dista[i], fontsize=10,show_rownames=F,show_colnames=T,fontsize_col=5,cex.lab=1)
      aa
      aa=pheatmap(hmMat21,scale="row",cluster_rows =T,cluster_cols = T,annotation_col = aka2,color = c(colorRampPalette(colors = c("#084594","#08519c","white"))(length(bk)/2),
                  colorRampPalette(colors = c("white","#cb181d","firebrick3"))(length(bk)/2)),#legend_breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5), 
                  legend_labels=F,breaks=bk, annotation_colors = aka3[1],annotation_legend = F,annotation_names_col=F,familyfont="Arial",
                  annotation_names_row=F,treeheight_row=20, treeheight_col=40,cellheight =225/dim(hmMat21)[1],cellwidth =300/dim(hmMat21)[2],clustering_distance_cols =dista[i],clustering_distance_row =dista[i], 
                  fontsize=20,show_rownames=F,show_colnames=F,fontsize_col=5,cex.lab=1)
      aa
    }
    dev.off()

}

## set the right library paths

library(optparse)
library(dplyr)
library(patchwork)
library(cowplot)
# library(reshape2)
library(ggpubr)
library(stringr)
library(pheatmap)
library(ggbiplot)
library(ggplot2)
library(factoextra)
library(RColorBrewer)
library(scatterplot3d)
library(RCurl)#同时引入later包，需要打开X11
library(bitops)
library(later) 
option_list = list(
    make_option(c("-p", "--path"), type = "character", default = "./",  help = "a file of weeks data(row1:a group of mus(diff weeks); row2:another group of mus(diff weeks))"),
    make_option(c("-i", "--files"), type = "character", default = "NormalizedData/CovInBins_1000079.t.NorDataSetSamplesInfo.log",  help = "a file of weeks data(row1:a group of mus(diff weeks); row2:another group of mus(diff weeks))"),
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
transformat <- function(df){
  D_samples <- as.data.frame(t(df))
  D_samples_1 <- c()
  for(i in colnames(D_samples)){
        D_samples_1 <- cbind(D_samples_1, as.numeric(as.character(as.matrix(D_samples[, i]))))
  }
  D_samples_1 <- as.data.frame(D_samples_1)
  colnames(D_samples_1) <- colnames(D_samples); rownames(D_samples_1) <- rownames(D_samples)
  return(D_samples_1)
}

# 1.DEN_revised
opt$output = "1.DEN_revised"
features = read.table(str_c(path, '/UnsupervisedClustering/1.DEN_revised/1.DEN_revisedDataMatrix_SignedFindMarkers_40regions.log'), sep='\t')
samples_type1 <- files.temp[files.temp['sample_label']=='D_Ctrl', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
samples_type2 <- files.temp[files.temp['sample_label']=='D_tDEN', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
print(str_c("GroupA-sample:",dim(samples_type1)[1],";  GroupB-sample:",dim(samples_type2)[1]))
groups_label = 'Con_DEN'
pic_name = str_c(str_c(path, '/UnsupervisedClustering/1.DEN_revised/pheatmap_Con_DEN_name.pdf'))
pheatmap_f_g2(groups_label, transformat(samples_type1), transformat(samples_type2), pic_name)

samples_type3 <- files.temp[files.temp['sample_label']=='D_ntDEN', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
groups_label = 'Con_DEN_ntDEN'
pic_name = str_c(str_c(path, '/UnsupervisedClustering/', as.character(opt$output), '/pheatmap_Con_DEN_ntDEN_name.pdf'))
pheatmap_f_g3(groups_label, transformat(samples_type1), transformat(samples_type2), transformat(samples_type3), pic_name)




###########################################################################
###########################################################################
# 2.Apc_revised
# features = read.table(str_c(path, '/UnsupervisedClustering/2.Apc/2.ApcDataMatrix_SignedFindMarkers_40regions.log'), sep='\t')
features = read.table(str_c(path, '/UnsupervisedClustering/2.Apc_revised/2.Apc_revisedDataMatrix_SignedFindMarkers_40regions.log'), sep='\t')
samples_type1 <- files.temp[files.temp['sample_label']=='A_Ctrl', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
samples_type2 <- files.temp[files.temp['sample_label']=='A_Apc', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
print(str_c("GroupA-sample:",dim(samples_type1)[1],";  GroupB-sample:",dim(samples_type2)[1]))
groups_label = 'Con_APC'
pic_name = str_c(str_c(path, '/UnsupervisedClustering/2.Apc_revised/pheatmap_Con_APC_name.pdf'))
pheatmap_f_g2(groups_label, transformat(samples_type1), transformat(samples_type2), pic_name)

groups_label = 'Con_APC_5wAPC_15APC'
# samples_type3 <- files.temp[grep('10373|10374', rownames(files.temp)), rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
samples_type3 <- files.temp[c('A_yApc3','A_yApc4'), rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
samples_type4 <- files.temp[c('A_yApc1','A_yApc2','A_yApc5'), rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
pic_name = str_c(str_c(path, '/UnsupervisedClustering/2.Apc_revised/pheatmap_Con_APC_yAPC_name.pdf'))
pheatmap_f_g4(groups_label, transformat(samples_type1), transformat(samples_type2), transformat(samples_type3),transformat(samples_type4), pic_name)


samples_type3 <- files.temp[files.temp['sample_label']=='A_flCtrl', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
samples_type4 <- files.temp[files.temp['sample_label']=='A_flApc', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
groups_label = 'Con_APC_flCtl_flAPC'
pic_name = str_c(str_c(path, '/UnsupervisedClustering/2.Apc_revised/pheatmap_Con_APC_flCtl_flAPC_name.pdf'))
pheatmap_f_g4(groups_label, transformat(samples_type1), transformat(samples_type2), transformat(samples_type3), transformat(samples_type4), pic_name)


samples_type3 <- files.temp[files.temp['sample_label']=='A_tCtrl', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
samples_type4 <- files.temp[files.temp['sample_label']=='A_tApc', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
samples_type4 <- samples_type4[c("A_tApc1","A_tApc3","A_tApc4"), ]
samples_type5 <- files.temp[files.temp['sample_label']=='A_otCtrl', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
samples_type6 <- files.temp[files.temp['sample_label']=='A_otApc', rownames(features)]#files[,grep(samples_type_type1,colnames(files))]
groups_label = 'Con_APC_tCtl_tAPC_otCtl_otAPC'
pic_name = str_c(str_c(path, '/UnsupervisedClustering/2.Apc_revised/pheatmap_Con_APC_tCtl_tAPC_otCtl_otAPC_name.pdf'))
pheatmap_f_g6(groups_label, transformat(samples_type1), transformat(samples_type2), transformat(samples_type3), 
              transformat(samples_type4), transformat(samples_type5), transformat(samples_type6), pic_name)




