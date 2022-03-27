from sklearn.metrics import roc_curve, auc,precision_recall_curve,average_precision_score
import matplotlib.pyplot as plt
import pickle
import argparse
import pandas as pd
import numpy as np
from sklearn.metrics import accuracy_score, f1_score, roc_auc_score

def get_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument('--filename',
        type=str,
        help='workpath')
    parser.add_argument('--modelpath',
        type=str,
        help='workpath')
    return parser

def sigmoid(x):
    return np.exp(x)/(1+ np.exp(x))


def calculate_ci(alpha, scores):
    lower_p = alpha
    upper_p = 100 - alpha
    lower = max(0.0, np.percentile(scores, lower_p))
    upper = min(1.0, np.percentile(scores, upper_p))
    return lower, upper


def auc_ci(dataframe, sample_frac, bootstrap):
    class_df = dataframe
    sample_frac = 0.90
    bootstrap=1000
    # 循环得到重复数据
    tpr_list = []
    fpr_list = []
    for i in range(bootstrap):
        cal_df = class_df.sample(frac=sample_frac) #,random_state=1234
        class_true = cal_df['class_true']
        final_prob = cal_df['final_prob']
        fpr,tpr,threshold  = roc_curve(class_true, final_prob)
        tpr_list.append(tpr)
        fpr_list.append(fpr)
    # tpr 与 fpr 只取长度一致的组别 rpt 与 fpr
    tpr_list_len = []
    fpr_list_len = []
    len_set = len((tpr_list[0]))
    for m in range(len(tpr_list)):    
        if len((tpr_list[m]))==len_set:
            tpr_list_len.append(tpr_list[m])
    for m in range(len(fpr_list)):
        if len((fpr_list[m]))==len_set:
            fpr_list_len.append(fpr_list[m])
    # 计算取出的组别的 auc
    roc_auc_list = []
    for i in range(len(tpr_list_len)):
        roc_auc = auc(fpr_list_len[i], tpr_list_len[i])
        roc_auc_list.append(roc_auc)
    std_auc = np.std(roc_auc_list)
    # 计算置信区间
    tpr_lower = []
    tpr_upper = []
    for m in range(len(tpr_list_len[0])): 
    #     print(m)
        tpr_cal = []
        for n in range(len(tpr_list_len)):
            tpr_cal.append(tpr_list_len[n][m])
        tpr_cal_lower, tpr_cal_upper = calculate_ci(5, tpr_cal)
        tpr_lower.append(tpr_cal_lower)
        tpr_upper.append(tpr_cal_upper)
    fpr_lower = []
    fpr_upper = []
    for m in range(len(fpr_list_len[0])): 
    #     print(m)
        fpr_cal = []
        for n in range(len(fpr_list_len)):
            fpr_cal.append(fpr_list_len[n][m])
        fpr_cal_lower, fpr_cal_upper = calculate_ci(5, fpr_cal)
        fpr_lower.append(fpr_cal_lower)
        fpr_upper.append(fpr_cal_upper)
    # 计算多次循环的选择平均值
    mean_tpr = []
    mean_fpr= []
    for m in range(len(tpr_list_len[0])): 
    #     print(m)
        tpr_cal = []
        for n in range(len(tpr_list_len)):
            tpr_cal.append(tpr_list_len[n][m])
        mean = np.mean(tpr_cal)
        mean_tpr.append(mean)
    # mean_tpr
    for m in range(len(fpr_list_len[0])): 
    #     print(m)
        fpr_cal = []
        for n in range(len(fpr_list_len)):
            fpr_cal.append(fpr_list_len[n][m])
        mean = np.mean(fpr_cal)
        mean_fpr.append(mean)
    # mean_fpr
    mean_auc = auc(mean_fpr, mean_tpr)
    #     lower_auc = auc(fpr_lower, tpr_lower)
    #     upper_auc = auc(fpr_upper, tpr_upper)
    return mean_fpr, mean_tpr, mean_auc, std_auc, tpr_lower, tpr_upper


def myplot(train_df, valid_df, test_df, sample_frac, bootstrap, save_set, save_set_WITHOUTlABEL, X_trainRandom_target, X_trainRandom_proba, X_validRandom_target, X_validRandom_proba, X_testRandom_target, X_testRandom_proba):
    import matplotlib as mpl
    mpl.use('Agg')#不依赖xmanager
    plt.figure(figsize=(12,5))
    ax=plt.subplot(121)
    # 获取数据
    train_mean_fpr, train_mean_tpr, train_mean_auc, train_std_auc, train_tpr_lower, train_tpr_upper = auc_ci(train_df, sample_frac, bootstrap)
    valid_mean_fpr, valid_mean_tpr, valid_mean_auc, valid_std_auc, valid_tpr_lower, valid_tpr_upper = auc_ci(valid_df, sample_frac, bootstrap)
    test_mean_fpr, test_mean_tpr, test_mean_auc, test_std_auc, test_tpr_lower, test_tpr_upper = auc_ci(test_df, sample_frac, bootstrap)
    # 绘图Training (AUC = %0.2f%%)' % (roc_auc[1]*100)
    # fig, ax = plt.subplots(figsize=(5,5))
    ax.plot(train_mean_fpr, train_mean_tpr, color='darkred', label=r'Training (AUC = %0.2f $\pm$ %0.2f)' % (train_mean_auc, train_std_auc), lw=6, alpha=.8)
    ax.plot(valid_mean_fpr, valid_mean_tpr, color='darkblue', label=r'Validation (AUC = %0.2f $\pm$ %0.2f)' % (valid_mean_auc, valid_std_auc), lw=6, alpha=.8)
    ax.plot(test_mean_fpr, test_mean_tpr, color='darkgreen', label=r'Test (AUC = %0.2f $\pm$ %0.2f)' % (test_mean_auc, test_std_auc), lw=6, alpha=.8)
    ax.plot([0, 1], [0, 1], linestyle='--', lw=2, color='grey', alpha=.8)
    ax.fill_between(train_mean_fpr, train_tpr_lower, train_tpr_upper, color='red', alpha=.2)
    ax.fill_between(valid_mean_fpr, valid_tpr_lower, valid_tpr_upper, color='blue', alpha=.2)
    ax.fill_between(test_mean_fpr, test_tpr_lower, test_tpr_upper, color='green', alpha=.2)
    ax.axes.xaxis.set_ticklabels([])
    ax.axes.yaxis.set_ticklabels([])
    #ax.set(xlim=[-0.05, 1.05], ylim=[-0.05, 1.05], xlabel="Specificity", ylabel="Sensitivity")
    #random-train
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    for i in [0,1]:fpr[i], tpr[i], _ = roc_curve(X_trainRandom_target,X_trainRandom_proba,drop_intermediate=False,pos_label=1);roc_auc[i] = auc(fpr[i], tpr[i])
    ax.plot(fpr[1], tpr[1], color='slategrey',lw=2, label='Random Classifiers ' )
    #random-valid
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    for i in [0,1]:fpr[i], tpr[i], _ = roc_curve(X_validRandom_target,X_validRandom_proba,drop_intermediate=False,pos_label=1);roc_auc[i] = auc(fpr[i], tpr[i])
    ax.plot(fpr[1], tpr[1], color='slategrey',lw=2)
    # plt.plot(fpr[1], tpr[1], color='rosybrown',lw=2, label='Random Classifiers (AUC = %0.2f%%)' % (roc_auc[1]*100))
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    for i in [0,1]:fpr[i], tpr[i], _ = roc_curve(X_testRandom_target,X_testRandom_proba,drop_intermediate=False,pos_label=1);roc_auc[i] = auc(fpr[i], tpr[i])
    ax.plot(fpr[1], tpr[1], color='slategrey',lw=2)
    ax.legend(loc="lower right",prop={'family' : 'Arial', 'size'   : 13,'weight' : 'bold'})    
    ax=plt.subplot(122)
    # train-pr
    precision, recall, _ = precision_recall_curve(train_df['class_true'],train_df['final_prob'])
    average_precision = average_precision_score(train_df['class_true'],train_df['final_prob'])
    ax.plot(recall, precision, color='darkred', lw=6, label='Training (AP = %0.2f%%)' % (average_precision*100))
    precision, recall, _ = precision_recall_curve(valid_df['class_true'],valid_df['final_prob'])
    average_precision = average_precision_score(valid_df['class_true'],valid_df['final_prob'])
    ax.plot(recall, precision, color='darkblue', lw=6, label='Validation (AP = %0.2f%%)' % (average_precision*100))
    precision, recall, _ = precision_recall_curve(test_df['class_true'],test_df['final_prob'])
    average_precision = average_precision_score(test_df['class_true'],test_df['final_prob'])
    ax.plot(recall, precision, color='darkgreen', lw=6, label='Test (AP = %0.2f%%)' % (average_precision*100))
    # random
    precision, recall, _ = precision_recall_curve(X_trainRandom_target,X_trainRandom_proba)
    average_precision = average_precision_score(X_trainRandom_target,X_trainRandom_proba)
    ax.plot(recall, precision, color='slategrey',lw=2, label='Random Classifiers' )
    # ax.plot(recall, precision, color='rosybrown',lw=lw, label='Random Classifiers (AP = %0.2f%%)' % (average_precision*100))
    precision, recall, _ = precision_recall_curve(X_testRandom_target,X_testRandom_proba)
    average_precision = average_precision_score(X_testRandom_target,X_testRandom_proba)
    ax.plot(recall, precision, color='slategrey',lw=2)
    # ax.plot(recall, precision, color='slategrey',lw=lw, label='Random Classifiers (AP = %0.2f%%)' % (average_precision*100))
    ax.plot([-0.01, 1.01], [-0.01, 1.01], color='grey', lw=2, linestyle='--')
    ax.axes.xaxis.set_ticklabels([])
    ax.axes.yaxis.set_ticklabels([])
    ax.set(xlim=[-0.01, 1.01])
    # ax.xticks(np.arange(0, 1.02, 0.2),fontproperties = 'Arial', size = 20)
    # ax.yticks(np.arange(0,1.02, 0.2),fontproperties = 'Arial', size = 20)
    # ax.xlabel('Recall',fontdict={'family' : 'Arial', 'size'   : 20})
    # ax.ylabel('Precision',fontdict={'family' : 'Arial', 'size'   : 20})
    # ax.legend(loc="lower left",prop={'family' : 'Arial', 'size'   : 13})
    ax.legend(loc="lower left",prop={'family' : 'Arial', 'size'   : 13,'weight' : 'bold'})    
    plt.savefig(save_set)
    # def myplot_withoutLegend(train_df, valid_df, test_df, sample_frac, bootstrap, save_set, X_trainRandom_target, X_trainRandom_proba, X_validRandom_target, X_validRandom_proba, X_testRandom_target, X_testRandom_proba):
    import matplotlib as mpl
    mpl.use('Agg')#不依赖xmanager
    plt.figure(figsize=(8,3))
    ax=plt.subplot(121)
    # 获取数据
    # 绘图Training (AUC = %0.2f%%)' % (roc_auc[1]*100)
    # fig, ax = plt.subplots(figsize=(5,5))
    ax.plot(train_mean_fpr, train_mean_tpr, color='darkred', label='', lw=2, alpha=.8)
    ax.plot(valid_mean_fpr, valid_mean_tpr, color='darkblue', label='', lw=2, alpha=.8)
    ax.plot(test_mean_fpr, test_mean_tpr, color='darkgreen', label='', lw=2, alpha=.8)
    ax.plot([0, 1], [0, 1], linestyle='--', lw=2, color='grey', alpha=.8)
    ax.fill_between(train_mean_fpr, train_tpr_lower, train_tpr_upper, color='red', alpha=.2)
    ax.fill_between(valid_mean_fpr, valid_tpr_lower, valid_tpr_upper, color='blue', alpha=.2)
    ax.fill_between(test_mean_fpr, test_tpr_lower, test_tpr_upper, color='green', alpha=.2)
    ax.axes.xaxis.set_ticklabels([])
    ax.axes.yaxis.set_ticklabels([])
    #ax.set(xlim=[-0.05, 1.05], ylim=[-0.05, 1.05], xlabel="Specificity", ylabel="Sensitivity")
    #random-train
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    for i in [0,1]:fpr[i], tpr[i], _ = roc_curve(X_trainRandom_target,X_trainRandom_proba,drop_intermediate=False,pos_label=1);roc_auc[i] = auc(fpr[i], tpr[i])
    ax.plot(fpr[1], tpr[1], color='slategrey',lw=1, label='Random Classifiers ' )
    #random-valid
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    for i in [0,1]:fpr[i], tpr[i], _ = roc_curve(X_validRandom_target,X_validRandom_proba,drop_intermediate=False,pos_label=1);roc_auc[i] = auc(fpr[i], tpr[i])
    ax.plot(fpr[1], tpr[1], color='slategrey',lw=1)
    # plt.plot(fpr[1], tpr[1], color='rosybrown',lw=2, label='Random Classifiers (AUC = %0.2f%%)' % (roc_auc[1]*100))
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    for i in [0,1]:fpr[i], tpr[i], _ = roc_curve(X_testRandom_target,X_testRandom_proba,drop_intermediate=False,pos_label=1);roc_auc[i] = auc(fpr[i], tpr[i])
    ax.plot(fpr[1], tpr[1], color='slategrey',lw=1)
    legend=ax.legend(loc="lower right",prop={'family' : 'Arial', 'size'   : 13,'weight' : 'bold'})    
    legend.remove()
    # plt.savefig(save_set_WITHOUTlABEL+'.pdf', dpi=1000)
    ax=plt.subplot(122)
    # train-pr
    precision, recall, _ = precision_recall_curve(train_df['class_true'],train_df['final_prob'])
    average_precision = average_precision_score(train_df['class_true'],train_df['final_prob'])
    ax.plot(recall, precision, color='darkred', lw=1, label='Training (AP = %0.2f%%)' % (average_precision*100))
    precision, recall, _ = precision_recall_curve(valid_df['class_true'],valid_df['final_prob'])
    average_precision = average_precision_score(valid_df['class_true'],valid_df['final_prob'])
    ax.plot(recall, precision, color='darkblue', lw=1, label='Validation (AP = %0.2f%%)' % (average_precision*100))
    precision, recall, _ = precision_recall_curve(test_df['class_true'],test_df['final_prob'])
    average_precision = average_precision_score(test_df['class_true'],test_df['final_prob'])
    ax.plot(recall, precision, color='darkgreen', lw=1, label='Test (AP = %0.2f%%)' % (average_precision*100))
    # random
    precision, recall, _ = precision_recall_curve(X_trainRandom_target,X_trainRandom_proba)
    average_precision = average_precision_score(X_trainRandom_target,X_trainRandom_proba)
    ax.plot(recall, precision, color='slategrey',lw=1, label='Random Classifiers' )
    # ax.plot(recall, precision, color='rosybrown',lw=lw, label='Random Classifiers (AP = %0.2f%%)' % (average_precision*100))\
    precision, recall, _ = precision_recall_curve(X_validRandom_target,X_validRandom_proba)
    average_precision = average_precision_score(X_validRandom_target,X_validRandom_proba)
    ax.plot(recall, precision, color='slategrey',lw=1)
    precision, recall, _ = precision_recall_curve(X_testRandom_target,X_testRandom_proba)
    average_precision = average_precision_score(X_testRandom_target,X_testRandom_proba)
    ax.plot(recall, precision, color='slategrey',lw=1)
    # ax.plot(recall, precision, color='slategrey',lw=lw, label='Random Classifiers (AP = %0.2f%%)' % (average_precision*100))
    ax.plot([0, 1], [0, 1], color='grey', lw=1, linestyle='--')
    ax.axes.xaxis.set_ticklabels([])
    ax.axes.yaxis.set_ticklabels([])
    ax.set(xlim=[0,1])
    # ax.xticks(np.arange(0, 1.02, 0.2),fontproperties = 'Arial', size = 20)
    # ax.yticks(np.arange(0,1.02, 0.2),fontproperties = 'Arial', size = 20)
    # ax.xlabel('Recall',fontdict={'family' : 'Arial', 'size'   : 20})
    # ax.ylabel('Precision',fontdict={'family' : 'Arial', 'size'   : 20})
    # ax.legend(loc="lower left",prop={'family' : 'Arial', 'size'   : 13})
    legend=ax.legend(loc="lower left",prop={'family' : 'Arial', 'size'   : 13,'weight' : 'bold'})    
    legend.remove()
    plt.savefig(save_set_WITHOUTlABEL)
    plt.savefig(save_set_WITHOUTlABEL+'2.pdf', dpi=1000)
    fig, ax = plt.subplots(dpi=300, figsize=(3.6, 3))
    # ax=plt.subplot(121)
    # 获取数据
    # 绘图Training (AUC = %0.2f%%)' % (roc_auc[1]*100)
    # fig, ax = plt.subplots(figsize=(5,5))
    ax.plot(train_mean_fpr, train_mean_tpr, color='darkred', label='', lw=2, alpha=.8)
    ax.plot(valid_mean_fpr, valid_mean_tpr, color='darkblue', label='', lw=2, alpha=.8)
    ax.plot(test_mean_fpr, test_mean_tpr, color='darkgreen', label='', lw=2, alpha=.8)
    ax.plot([0, 1], [0, 1], linestyle='--', lw=1, color='grey', alpha=.8)
    ax.fill_between(train_mean_fpr, train_tpr_lower, train_tpr_upper, color='red', alpha=.2)
    ax.fill_between(valid_mean_fpr, valid_tpr_lower, valid_tpr_upper, color='blue', alpha=.2)
    ax.fill_between(test_mean_fpr, test_tpr_lower, test_tpr_upper, color='green', alpha=.2)
    ax.axes.xaxis.set_ticklabels([])
    ax.axes.yaxis.set_ticklabels([])
    #ax.set(xlim=[-0.05, 1.05], ylim=[-0.05, 1.05], xlabel="Specificity", ylabel="Sensitivity")
    #random-train
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    for i in [0,1]:fpr[i], tpr[i], _ = roc_curve(X_trainRandom_target,X_trainRandom_proba,drop_intermediate=False,pos_label=1);roc_auc[i] = auc(fpr[i], tpr[i])
    ax.plot(fpr[1], tpr[1], color='slategrey',lw=1, label='Random Classifiers ' )
    #random-valid
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    for i in [0,1]:fpr[i], tpr[i], _ = roc_curve(X_validRandom_target,X_validRandom_proba,drop_intermediate=False,pos_label=1);roc_auc[i] = auc(fpr[i], tpr[i])
    ax.plot(fpr[1], tpr[1], color='slategrey',lw=1)
    # plt.plot(fpr[1], tpr[1], color='rosybrown',lw=2, label='Random Classifiers (AUC = %0.2f%%)' % (roc_auc[1]*100))
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    for i in [0,1]:fpr[i], tpr[i], _ = roc_curve(X_testRandom_target,X_testRandom_proba,drop_intermediate=False,pos_label=1);roc_auc[i] = auc(fpr[i], tpr[i])
    ax.plot(fpr[1], tpr[1], color='slategrey',lw=1)
    legend=ax.legend(loc="lower right",prop={'family' : 'Arial', 'size'   : 13,'weight' : 'bold'})    
    legend.remove()
    fig.savefig(save_set_WITHOUTlABEL+'.pdf', dpi=1000)
    


if __name__ == '__main__':
    args = get_parser().parse_args()
    model_path = args.modelpath
    file_name = args.filename

    path=model_path+'/'+file_name
    # open a file, where you stored the pickled data
    file = open(path+'/results.pkl', 'rb')
    # dump information to that file
    data = pickle.load(file)
    # close the file
    file.close()
    
    
    label_train = pd.DataFrame(data['train']['label'].astype(int))[0]
    label_valid = pd.DataFrame(data['valid']['label'].astype(int))[0]
    label_test = pd.DataFrame(data['test']['label'].astype(int))[0]
    pred_train =pd.DataFrame(sigmoid(data['train']['prediction'][:,1]))[0]
    pred_valid =pd.DataFrame(sigmoid(data['valid']['prediction'][:,1]))[0]
    pred_test = pd.DataFrame(sigmoid(data['test']['prediction'][:,1]))[0] 
    
    
    train_df = pd.DataFrame({'class_true':label_train, 'final_prob':pred_train})
    valid_df = pd.DataFrame({'class_true':label_valid, 'final_prob':pred_valid})
    test_df = pd.DataFrame({'class_true': label_test, 'final_prob': pred_test })
    train_df.to_csv(path+'/Ensemble_Model_trn.csv',sep="\t")
    valid_df.to_csv(path+'/Ensemble_Model_valid.csv',sep="\t")
    test_df.to_csv(path+'/Ensemble_Model_test.csv',sep="\t")
    # newtest_df.to_csv('{}Ensemble_Model_newtest.csv'.format(newtest_name),sep="\t")
    # newtest_df.to_csv(work_path+'/'+file_name+'Ensemble_Model_newtest.csv',sep="\t")
    import copy
    import random
    random.seed(1234)
    randomlabel_train=copy.deepcopy(label_train)
    random.shuffle(randomlabel_train)
    # print(label_train)
    # print(randomlabel_train)
    randomlabel_valid=copy.deepcopy(label_valid)
    random.shuffle(randomlabel_valid)
    randomlabel_test=copy.deepcopy(label_test)
    random.shuffle(randomlabel_test)
    myplot(train_df, valid_df, test_df, sample_frac=0.9, bootstrap=100, save_set=path+"/Ensemble_Model.png", save_set_WITHOUTlABEL=path+"/Ensemble_Model_withoutLegend.png",X_trainRandom_target=randomlabel_train, X_trainRandom_proba=pred_train, X_validRandom_target=randomlabel_valid, X_validRandom_proba=pred_valid,X_testRandom_target=randomlabel_test, X_testRandom_proba=pred_test)
    # myplot_withoutLegend(train_df, valid_df, test_df, sample_frac=0.9, bootstrap=100, save_set=work_path+'/'+file_name+'/'+parameter1+'/'+"Ensemble_Model_withoutLegend.png", X_trainRandom_target=randomlabel_train, X_trainRandom_proba=pred_train, X_validRandom_target=randomlabel_valid, X_validRandom_proba=pred_valid,     X_testRandom_target=randomlabel_test, X_testRandom_proba=pred_test)
    
    d0=test_df.loc[test_df['class_true']==0,['class_true','final_prob']].sort_values('final_prob'); d0=d0.reset_index()
    cutoff=d0.loc[round(d0.shape[0]*0.95-1), 'final_prob']
    predicted_class1=np.zeros(len(test_df['final_prob']))
    predicted_class1[test_df['final_prob']>cutoff]=1
    sensitivity_at_95specificity = sum(predicted_class1)/sum(test_df['class_true'])
    print(str(cutoff)+'; sensitivity_at_95specificity'+str(sensitivity_at_95specificity))
    cutoff=d0.loc[round(d0.shape[0]*0.99-1), 'final_prob']
    predicted_class1=np.zeros(len(test_df['final_prob']))
    predicted_class1[test_df['final_prob']>cutoff]=1
    sensitivity_at_99specificity = sum(predicted_class1)/sum(test_df['class_true'])
    print(str(cutoff)+'; sensitivity_at_99specificity'+str(sensitivity_at_99specificity))

    Train_acc = accuracy_score(label_train, sigmoid(data['train']['prediction']).argmax(axis=1))
    Train_F1 = f1_score(label_train, sigmoid(data['train']['prediction']).argmax(axis=1))
    Train_AUC = roc_auc_score(label_train, train_df['final_prob'])
    print(file_name+'---Train_acc\t'+str(Train_acc)+'---Train_AUC\t'+str(Train_AUC)+'---Train_F1s\t'+str(Train_F1))
    Val_acc = accuracy_score(label_valid, sigmoid(data['valid']['prediction']).argmax(axis=1))
    Val_F1 = f1_score(label_valid, sigmoid(data['valid']['prediction']).argmax(axis=1))
    Val_AUC = roc_auc_score(label_valid, valid_df['final_prob'])
    print(file_name+'---Val_acc\t'+str(Val_acc)+'---Val_AUC\t'+str(Val_AUC)+'---Val_F1s\t'+str(Val_F1))
    Test_acc = accuracy_score(label_test, sigmoid(data['test']['prediction']).argmax(axis=1))
    Test_F1 = f1_score(label_test, sigmoid(data['test']['prediction']).argmax(axis=1))
    Test_AUC = roc_auc_score(label_test, test_df['final_prob'])
    print(file_name+'---Test_acc\t'+str(Test_acc)+'---Test_AUC\t'+str(Test_AUC)+'---Test_F1s\t'+str(Test_F1))
