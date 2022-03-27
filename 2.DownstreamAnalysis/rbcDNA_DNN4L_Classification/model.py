import torch.nn as nn
import torch.nn.functional as F

class MCDNN(nn.Module):
    # multi-class deep neural network
    def __init__(self, args, input_dim, z_dim=30):

        super().__init__()
        self.e1 = nn.Linear(input_dim, args.h1_dim)
        self.bn1 = nn.BatchNorm1d(args.h1_dim)
        self.dropout = nn.Dropout(p=0.5)
        self.e2 = nn.Linear(args.h1_dim, args.h2_dim)
        self.bn2 = nn.BatchNorm1d(args.h2_dim)
        self.e3 = nn.Linear(args.h2_dim, args.h3_dim)
        self.bn3 = nn.BatchNorm1d(args.h3_dim)
        self.e4 = nn.Linear(args.h3_dim, z_dim)
        self.bn4 = nn.BatchNorm1d(z_dim)
        self.cls = nn.Linear(z_dim, args.class_number)

    def encoder(self, x):
        x = F.relu(self.bn1(self.e1(x)))
        x = self.dropout(x)
        x = F.relu(self.bn2(self.e2(x)))
        x = F.relu(self.bn3(self.e3(x)))
        x = F.relu(self.bn4(self.e4(x)))
        return x

    def classifier(self, x):
        x = self.cls(x)
        return x

    def forward(self, x):
        
        latent = self.encoder(x)
        distribution = self.classifier(latent)
        
        return latent, distribution
