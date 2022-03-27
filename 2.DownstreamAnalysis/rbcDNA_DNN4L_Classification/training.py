import math
import os
import pandas as pd
import matplotlib.pyplot as plt
from sklearn import metrics
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.utils.data
from torch import optim
from sklearn.decomposition import PCA
from sklearn.manifold import TSNE
from model import MCDNN
from loss import Losses
from utils import data_extract, calculate_auc, mixup_data, calculate_metrics
import numpy as np
from pathlib import Path
import pickle
import json

device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')


def train(args, model, optimizer, epoch, train_data, weights=None):
    model.train()
    Loss = Losses(device=device, weights=weights)
    num_train_sample = train_data.data.shape[0]
    data = train_data[:, :-1].float()
    label = train_data[:, -1].float()
    label = label.reshape(label.shape[0], -1)
    # add data to device
    optimizer.zero_grad()
    data = data.to(device)
    label = label.long().to(device)

    # forward
    data, label_a, label_b, lamb = mixup_data(data, label, alpha=args.mixup_alpha)
    latent, distribution = model(data)

    # loss and backward
    loss = Loss.loss_function_mixup(model, distribution, data, label_a, label_b, lamb)
    return latent, loss


def valid(args, model, epoch, valid_data):
    model.eval()

    with torch.no_grad():
        num_valid_sample = valid_data.data.shape[0]
        # Prepare data and label
        data = valid_data[:, :-1].float()
        label = valid_data[:, -1]
        label = label.reshape(label.shape[0], -1)
        # forward and get information
        data = data.to(device)
        label = label.to(device)
        latent, predict = model(data)

        acc, report = calculate_metrics(label.cpu().detach().numpy(),
                            predict.cpu().detach().numpy())
        values = report.values()
        recalls = []
        for v in list(values)[:args.class_number]:
            recalls.append(v['recall'])
        return acc, recalls, label.cpu().detach().numpy(), predict.cpu().detach().numpy()

def test_and_explain(args, best_epoch=0):

    datasets = ['trn', 'val', 'test']
    assert (args.explain_class < args.class_number)
    explain_class = args.explain_class
    if args.class_number == 2:
        filename = [args.dataset_dir + args.task + '_binaryc_{}.csv'.format(dataset) for dataset in datasets]
        data, dim, weights = data_extract(filename[0], filename[1], filename[2])
    elif args.class_number == 4:
        filename = [args.dataset_dir + 'lc_crc_hcc_hd_multic_{}.csv'.format(dataset) for dataset in datasets]
        data, dim, weights = data_extract(filename[0], filename[1], filename[2])
    elif args.class_number == 3:
        filename = [args.dataset_dir + args.task + '_multic_{}.csv'.format(dataset) for dataset in datasets]
        data, dim, weights = data_extract(filename[0], filename[1], filename[2])

    if args.test_epoch != 0:
        best_epoch = args.test_epoch

    model = MCDNN(args=args, input_dim=dim, z_dim=args.z_dim).to(device)
    model_path = Path('saving/' + args.task)/'saved_model'
    load_model(model_path, model, best_epoch)
    acc_train, recalls_train, train_label, train_predict = valid(args, model, best_epoch, data[0])
    acc_valid, recalls_valid, valid_label, valid_predict = valid(args, model, best_epoch, data[1])
    acc_test, recalls_test, test_label, test_predict = valid(args, model, best_epoch, data[2])

    if args.class_number == 2:
        auc_train = calculate_auc(train_label, sigmoid(train_predict[:,1]), plot=True, path='saving/' + args.task + '/train_auc.png')
        auc_valid = calculate_auc(valid_label, sigmoid(valid_predict[:,1]), plot=True, path='saving/' + args.task + '/validation_auc.png')
        auc_test = calculate_auc(test_label, sigmoid(test_predict[:,1]), plot=True, path='saving/' + args.task + '/test_auc.png')
        metrics = {'train':{'acc':acc_train, 'recalls':recalls_train, 'auc':auc_train},
                    'valid':{'acc':acc_valid, 'recalls':recalls_valid, 'auc':auc_valid},
                    'test':{'acc':acc_test, 'recalls':recalls_test, 'auc':auc_test}}
    else:
        metrics = {'train':{'acc':acc_train, 'recalls':recalls_train},
                    'valid':{'acc':acc_valid, 'recalls':recalls_valid},
                    'test':{'acc':acc_test, 'recalls':recalls_test}}

    results = {'train':{'label':train_label, 'prediction':train_predict},
                    'valid':{'label':valid_label, 'prediction':valid_predict},
                    'test':{'label':test_label, 'prediction':test_predict}}
    
    with open(Path('saving/' + args.task + '/results.pkl'), "wb") as f:
        pickle.dump(results, f)
    with open(Path('saving/' + args.task + '/metrics.json'), 'w') as file_obj:
        json.dump(metrics, file_obj)
    print(metrics)

    #explain with shapley values
    import shap
    train_data_in = data[1][:, :-1].detach().cpu().numpy()
    test_data_in = data[2][:, :-1].detach().cpu().numpy()
    f = lambda x: model(torch.from_numpy(x).to(device))[1].cpu().detach().numpy()
    explainer = shap.KernelExplainer(f, train_data_in)
    
    shap_values = explainer.shap_values(test_data_in)

    shap_mean = shap_values[explain_class].mean(axis=0)
    shap_std = shap_values[explain_class].std(axis=0)
    shap_explain = {'shap_values':shap_values, 'shap_mean':shap_mean, 'shap_std':shap_std}

    explain_save_dir = Path('saving/' + args.task + '/explain_class_{}'.format(explain_class))
    if not os.path.exists(explain_save_dir):
        os.makedirs(explain_save_dir)
    with open(explain_save_dir/ 'explain_results.pkl', "wb") as f:
        pickle.dump(shap_explain, f)

    shap.summary_plot(shap_values[explain_class], test_data_in, color='coolwarm', max_display=50) #plot_type="violin", 
    plt.savefig(explain_save_dir / 'shapval_visualization.pdf')
    plt.show()

def train_model(args):
    datasets = ['trn', 'val', 'test']
    if args.class_number == 2:
        filename = [args.dataset_dir + args.task + '_binaryc_{}.csv'.format(dataset) for dataset in datasets]
        data, dim, weights = data_extract(filename[0], filename[1], filename[2])
    elif args.class_number == 4:
        filename = [args.dataset_dir + 'lc_crc_hcc_hd_multic_{}.csv'.format(dataset) for dataset in datasets]
        data, dim, weights = data_extract(filename[0], filename[1], filename[2])
    elif args.class_number == 3:
        filename = [args.dataset_dir + args.task + '_multic_{}.csv'.format(dataset) for dataset in datasets]
        data, dim, weights = data_extract(filename[0], filename[1], filename[2])

    if args.balanced_loss == False:
        weights = None

    acc_train_best, acc_test_best, acc_valid_best = 0,0,0

    recalls_train_best = [0 for i in range(args.class_number)]
    recalls_test_best = [0 for i in range(args.class_number)]
    recalls_valid_best = [0 for i in range(args.class_number)]

    model = MCDNN(args=args, input_dim=dim, z_dim=args.z_dim).to(device)

    optimizer = optim.Adam(model.parameters(), lr=args.lr)

    for epoch in range(args.epochs):
        
        latent, loss = train(args, model, optimizer, epoch, data[0], weights=weights)
        loss.backward()
        optimizer.step()

        acc_train, recalls_train, train_label, train_predict = valid(args, model, epoch, data[0])
        acc_valid, recalls_valid, valid_label, valid_predict = valid(args, model, epoch, data[1])
        acc_test, recalls_test, test_label, test_predict = valid(args, model, epoch, data[2])

        if acc_valid_best < acc_valid:
            print('Task:{}, Best accuracy on validation set changes from {} to {}, Saved the epoch {}'\
               .format(args.task, round(acc_valid_best,4), round(acc_valid,4), epoch) )
            acc_test_best = acc_test
            acc_valid_best = acc_valid
            acc_train_best = acc_train
            recalls_test_best = recalls_test
            recalls_valid_best = recalls_valid
            recalls_train_best = recalls_train
            best_epoch = epoch
            saver = [train_label,train_predict,valid_label,valid_predict,test_label,test_predict]
            best_model_path, model_path = save_model(args, model, epoch)

    load_model(model_path, model, best_epoch)
    acc_train, recalls_train, train_label, train_predict = valid(args, model, best_epoch, data[0])
    acc_valid, recalls_valid, valid_label, valid_predict = valid(args, model, best_epoch, data[1])
    acc_test, recalls_test, test_label, test_predict = valid(args, model, best_epoch, data[2])

    if args.class_number == 2:
        auc_train = calculate_auc(train_label, sigmoid(train_predict[:,1]), plot=True, path='saving/' + args.task + '/train_auc.png')
        auc_valid = calculate_auc(valid_label, sigmoid(valid_predict[:,1]), plot=True, path='saving/' + args.task + '/validation_auc.png')
        auc_test = calculate_auc(test_label, sigmoid(test_predict[:,1]), plot=True, path='saving/' + args.task + '/test_auc.png')
        metrics = {'train':{'acc':acc_train, 'recalls':recalls_train, 'auc':auc_train},
                    'valid':{'acc':acc_valid, 'recalls':recalls_valid, 'auc':auc_valid},
                    'test':{'acc':acc_test, 'recalls':recalls_test, 'auc':auc_test}}
    else:
        metrics = {'train':{'acc':acc_train, 'recalls':recalls_train},
                    'valid':{'acc':acc_valid, 'recalls':recalls_valid},
                    'test':{'acc':acc_test, 'recalls':recalls_test}}

    results = {'train':{'label':train_label, 'prediction':train_predict},
                    'valid':{'label':valid_label, 'prediction':valid_predict},
                    'test':{'label':test_label, 'prediction':test_predict}}
    
    with open(Path('saving/' + args.task + '/results.pkl'), "wb") as f:
        pickle.dump(results, f)
    with open(Path('saving/' + args.task + '/metrics.json'), 'w') as file_obj:
        json.dump(metrics, file_obj)
    print(metrics)

    hyper_name = '/acc_{}_{},{},{},{}_{}'.format(acc_test, args.h1_dim, args.h2_dim, args.h3_dim, args.z_dim, args.mixup_alpha)
    with open(Path('saving/' + args.task + hyper_name +'.json'), 'w') as file_obj:
        json.dump(metrics, file_obj)

    return best_epoch

def save_model(args, model, epoch):
    model_path = Path('saving/' + args.task)/'saved_model'
    if not os.path.exists(model_path):
        os.makedirs(model_path)
    config = model.state_dict()
    torch.save(config, model_path/('epo%d.tar' % epoch))
    return model_path/('epo%d.tar' % epoch), model_path

def load_model(model_path, model, epoch):
    assert os.path.exists(model_path/('epo%d.tar' % epoch)), 'Weights at epoch %d not found' % epoch
    checkpoint = torch.load(model_path/('epo%d.tar' % epoch), map_location='cpu')
    model.load_state_dict(checkpoint)
    print("Loaded model at {}".format(epoch))

def sigmoid(x):
    return np.exp(x)/(1+ np.exp(x))
