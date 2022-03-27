python3.7 STEP1_Datasetpreprocessing.py --workpath ../ --filename CovInBins_1000079.CoveredPercent.log

Rscript STEP2_SelectedSig.R -p ../ -i ./NormalizedData/CovInBins_1000079.t.NorDataSetSamplesInfo.log  --samples_type1 A_Ctrl --samples_type2 A_Apc --output 2.Apc

Rscript STEP2_SelectedSig.R -p ../ -i ./NormalizedData/CovInBins_1000079.t.NorDataSetSamplesInfo.log  --samples_type1 D_Ctrl --samples_type2 D_tDEN --output 1.DEN

Rscript STEP3_SelectedSig_MultiGroup.R -p ../ -i ./NormalizedData/CovInBins_1000079.t.NorDataSetSamplesInfo.log --output 3.Ctrl_Apc_DEN

Rscript STEP3_Plot.R -p ../ -i ./NormalizedData/CovInBins_1000079.t.NorDataSetSamplesInfo.log
