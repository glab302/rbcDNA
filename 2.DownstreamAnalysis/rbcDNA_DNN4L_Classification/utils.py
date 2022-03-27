import numpy as np
import torchvision
import argparse
import math
import os
import pandas as pd
import matplotlib.pyplot as plt
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.utils.data
from torch import optim
from torch.autograd import Variable
from torch.nn import Parameter
from torchvision import datasets, transforms
from torchvision.utils import save_image 
from sklearn.model_selection import train_test_split
from sklearn.decomposition import PCA
from sklearn.manifold import TSNE
from sklearn.metrics import classification_report
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

def data_extract(train, valid, test):

    df_train = pd.read_csv(train)
    df_valid = pd.read_csv(valid)
    df_test = pd.read_csv(test)
    input_dim = df_train.shape[1] - 1

    train_data = torch.tensor(
        df_train.values, requires_grad=False, dtype=torch.float).to(device)
    valid_data = torch.tensor(
        df_valid.values, requires_grad=False, dtype=torch.float).to(device)
    test_data = torch.tensor(
        df_test.values, requires_grad=False, dtype=torch.float).to(device)
    classes = torch.unique(train_data[:,-1])
    weights = []
    for cls in classes:
        cls_number = (train_data[:,-1] == cls).sum()
        weights.append(cls_number)
    weights = torch.FloatTensor(weights).to(device)
    weights = 1./weights
    weights = weights/torch.sum(weights)
    return [train_data, valid_data, test_data], input_dim, weights

def mixup_data(x, y, alpha=1.0, use_cuda=True):
    '''Returns mixed inputs, pairs of targets, and lambda'''
    if alpha > 0:
        lam = np.random.beta(alpha, alpha)
    else:
        lam = 0.1

    batch_size = x.size()[0]
    if use_cuda:
        index = torch.randperm(batch_size).cuda()
    else:
        index = torch.randperm(batch_size)

    mixed_x = lam * x + (1 - lam) * x[index, :]
    y_a, y_b = y, y[index]
    return mixed_x, y_a, y_b, lam


def calculate_auc(class_true, final_prob, title='Receiver operating characteristic', plot=False, path='temp.png'):
    from sklearn.metrics import roc_curve, auc
    fpr, tpr, threshold = roc_curve(class_true, final_prob)
    roc_auc = auc(fpr, tpr)
    if plot == True:
        lw = 2
        plt.figure(figsize=(7, 7))
        plt.plot(fpr, tpr, color='darkorange',
                 lw=lw, label='ROC curve (area = %0.4f)' % roc_auc)  # 假正率为横坐标，真正率为纵坐标做曲线
        plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
        plt.xlim([0.0, 1.0])
        plt.ylim([0.0, 1.05])
        plt.xlabel('False Positive Rate')
        plt.ylabel('True Positive Rate')

        plt.title(title)
        plt.legend(loc="lower right")
        plt.savefig(path)
        # plt.show()
        plt.close()
    return roc_auc

def calculate_metrics(label, predict):
    sample_num = label.shape[0]
    predict_label = predict.argmax(axis=1)
    acc = (predict_label == label.flatten()).sum()/sample_num
    report = classification_report(label, predict_label, output_dict=True)
    return acc, report

if __name__ == '__main__':
    label =  np.array([0, 1, 2, 2, 2])
    predict = np.array([0, 0, 2, 2, 1])
    acc, report = calculate_metrics(label, predict)
    print('finished')