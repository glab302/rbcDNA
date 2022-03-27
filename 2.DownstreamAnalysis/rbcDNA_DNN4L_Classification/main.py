import numpy as np
import torch
import argparse
from training import train_model, test_and_explain
from pathlib import Path
import os
import json
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

np.random.seed(1234)
torch.manual_seed(1234)

parser = argparse.ArgumentParser(
    description='MultiLayerPerceptron with mixup Learning for classification tasks')

parser.add_argument('--lr', default=3e-5, type=float, help='learning rate')
parser.add_argument('--h1_dim', default=180, type=int,
                    help='hidden layer_1 dimension')
parser.add_argument('--h2_dim', default=100, type=int,
                    help='hidden layer_2 dimension')
parser.add_argument('--h3_dim', default=50, type=int,
                    help='hidden layer_3 dimension')
parser.add_argument('--z_dim', default=15, type=int,
                    help='embedding layer dimension')
parser.add_argument('--epochs', default=1000,
                    type=int, help='number of epochs')
parser.add_argument('--weight_l2', default=1,
                    type=float, help='weight of l2 regularization')
parser.add_argument('--weight_ae', default=5,
                    type=int, help='weight of reconstruction loss')
parser.add_argument('--mixup_alpha', default=1.0, 
                    type=float, help='hyper parameter for mixup training')
parser.add_argument('--task', default='Healthy_ColorectalCancer', 
                    choices=[
                        'Healthy_ColorectalCancer',
                        'Healthy_LiverCancer',
                        'Healthy_LungCancer',
                        'lccrchcc_hd',
                        'lc_crc_hcc_hd',
                        'lc_crc_hcc'
                        ],
                    type=str, help='task for parameter tuning')
parser.add_argument('--dataset_dir', default='./DataMatrix/')
parser.add_argument('--class_number', default=2, type=int, help='binary classification 2, multiclass 4')
parser.add_argument('--balanced_loss', default=False, type=bool)

parser.add_argument('--only_test', default=False, type=bool)
parser.add_argument('--test_epoch', default=964, type=int)
parser.add_argument('--explain_class', default=1, type=int)

args = parser.parse_args()

path = Path('saving')/args.task
path.mkdir(exist_ok=True, parents=True)
sv_param = os.path.join(path, 'model_param.json')
with open(sv_param, 'w') as file_obj:
    json.dump(args.__dict__, file_obj)


if args.only_test:
    assert (args.test_epoch>0)
    test_and_explain(args)
else:
    best_epoch = train_model(args)
    test_and_explain(args, best_epoch)

