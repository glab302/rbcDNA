import pandas as pd
import numpy as np
import argparse
from pandas.testing import assert_frame_equal
from scipy import stats
import os
import re
def get_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument('--workpath',
        type=str,
        help='workpath')
    parser.add_argument('--filename',
        nargs='+',
        help='file')
    return parser


if __name__ == "__main__":
    args = get_parser().parse_args()
    print('**********************************1. data_preprocessing***********************************')
    alldataset = pd.DataFrame()
    os.chdir(args.workpath)
    name_list = args.filename
    print(name_list)
    for i in range(len(name_list)):
        dataset = pd.read_csv('./RawData/'+name_list[i], sep='\t', encoding='utf-8', index_col=0)
        feature_filtered = dataset.index[~dataset.index.str.contains('X|Y|M')].tolist()
        dataset = dataset.loc[feature_filtered,:]
        print('checking the size of the dataset: ' + str(dataset.shape))
        alldataset = pd.concat([alldataset, dataset], axis=1)
    
    alldatasetT1 = alldataset.T
    alldatasetT2 = alldatasetT1.div(alldatasetT1.sum(axis=1),axis='rows')*100000
    
    alldatasetT2.to_csv('./NormalizedData/'+name_list[i].replace('.CoveredPercent.log', '.NorDataSet.log'), sep="\t", encoding='utf-8')
    alldatasetT2.T.to_csv('./NormalizedData/'+name_list[i].replace('.CoveredPercent.log', '.t.NorDataSet.log'), sep="\t", encoding='utf-8')
    
    
    alldatasetT2['task_label'] = [i.split('_')[0] for i in alldatasetT2.index.tolist()]
    alldatasetT2['sample_label'] = [re.sub(r'[0-9]+', '', i) for i in alldatasetT2.index.tolist()]
    alldatasetT2.to_csv('./NormalizedData/'+name_list[i].replace('.CoveredPercent.log', '.NorDataSetSamplesInfo.log'), sep="\t", encoding='utf-8')
    alldatasetT2.T.to_csv('./NormalizedData/'+name_list[i].replace('.CoveredPercent.log', '.t.NorDataSetSamplesInfo.log'), sep="\t", encoding='utf-8')

