import numpy as np
import math
import os
import pandas as pd
import matplotlib.pyplot as plt
import torch
import torch.nn as nn
import torch.utils.data

device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

np.random.seed(1234)
torch.manual_seed(1234)


class Losses(object):
    def __init__(self, device, weights=None):

        self.mseLoss = nn.MSELoss()
        self.LogLoss = nn.CrossEntropyLoss()
        if weights is not None:
            self.LogLoss = nn.CrossEntropyLoss(weight=weights)
        self.device = device

    def mixup_criterion(self, criterion, predict, y_a, y_b, lam):
        return lam * criterion(predict, y_a.squeeze()) + (1 - lam) * criterion(predict, y_b.squeeze())

    def loss_function_mixup(self, model, predict, input_data, y_a, y_b, lam):
        lb = self.mixup_criterion(self.LogLoss, predict, y_a, y_b, lam) 
        lel = self.LelLoss(model, device, L2=0.01)

        return (lb  + lel)

    def LelLoss(self, model_c, device, L2=0.035):
        l2_reg = torch.tensor(0.).to(device)
        for name, param in model_c.named_parameters():
            if 'e' in name.split('.')[0]:
                l2_reg += torch.norm(param)
        return L2 * (l2_reg)

    def RecLoss(self, input_data, rec_data):
        rec_loss = self.mseLoss(rec_data, input_data)
        return rec_loss