import pandas as pd
import argparse
from collections import Counter

def get_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument('--target_bed',
        type=str,
        help='target_bed')
    parser.add_argument('--samples',
        nargs='+',
        help='samples')
    parser.add_argument('--prefix',
        type=str,
        help='name')
    parser.add_argument('--suffix',
        type=str,
        help='name')
    return parser


def rowname_rename(dataset):
    # dataset = pd.read_csv(sample, sep='\t', encoding='utf-8', header=None)
    dataset = dataset.rename(columns={dataset.columns.tolist()[0]:'chr', dataset.columns.tolist()[1]:'start', dataset.columns.tolist()[2]:'stop'})
    row_name = []
    for row in range(dataset.shape[0]):
        row_name.append(str(dataset.loc[row,'chr'])+'_'+str(dataset.loc[row,'start'])+'_'+str(dataset.loc[row,'stop']))
    return(row_name)


def summary_all(target_bed, samples):
    target_df = pd.read_csv(target_bed, sep='\t', encoding='utf-8', header=None)
    target_df.index = rowname_rename(target_df)
    All_featureCov = pd.DataFrame()
    All_coveredbp = pd.DataFrame()
    All_readcount = pd.DataFrame()
    for each in samples:
        eachsample_df = pd.read_csv(each, sep='\t', encoding='utf-8', header=None)
        eachsample_df.index = rowname_rename(eachsample_df)
        eachsample_df = eachsample_df.loc[target_df.index, :]
        eachsample_readcount = eachsample_df[(target_df.shape[1]+0)]
        eachsample_coveredbp = eachsample_df[(target_df.shape[1]+1)]
        eachsample_featureCov = eachsample_df[(target_df.shape[1]+3)]
        All_readcount[each] = eachsample_readcount
        All_coveredbp[each] = eachsample_coveredbp
        All_featureCov[each] = round(eachsample_featureCov,7)
    return(All_featureCov, All_coveredbp, All_readcount)



if __name__ == "__main__":
    args = get_parser().parse_args()
    target_bed = args.target_bed
    samples_bed = args.samples
    prefix_name = args.prefix
    suffix_name = args.suffix
    # samples_bed = ["10410.R", "10414.R", "10426.R", "10427.R", "10434.R", "10435.R", "10436.R"]
    # target_bed = "/data2/xingyun/GaoData/For_manuscript/DataUpdate/Test_CFSinrbcDNA_20210217/morethan80cfs/allchr_morethan80cfs_formHumanCFS.bed"
    samples_bed = [sample+prefix_name for sample in samples_bed]
    
    processed_df, processed_df_cov, processed_df_count = summary_all(target_bed, samples_bed)
    print('-------Maybe Null Files-------: '+str(Counter(processed_df.sum(axis=0)==0)[True]))
    if Counter(processed_df.sum(axis=0)==0)[True]!=0:
        print(processed_df.sum(axis=0)[processed_df.sum(axis=0)==0].index)
    print('-------Available Files-------: '+str(Counter(processed_df.sum(axis=0)==0)[False]))
    processed_df.columns = [i.replace(prefix_name, "") for i in processed_df.columns.tolist()]
    processed_df.to_csv(suffix_name+str(Counter(processed_df.sum(axis=0)==0)[False])+'.CoveredPercent.log', sep='\t')
    processed_df_cov.columns = [i.replace(prefix_name, "") for i in processed_df_cov.columns.tolist()]
    processed_df_cov.to_csv(suffix_name+str(Counter(processed_df.sum(axis=0)==0)[False])+'.CoveredBp.log', sep='\t')
    processed_df_count.columns = [i.replace(prefix_name, "") for i in processed_df_count.columns.tolist()]
    processed_df_count.to_csv(suffix_name+str(Counter(processed_df_count.sum(axis=0)==0)[False])+'.ReadCounts.log', sep='\t')


