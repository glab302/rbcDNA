# 220325
PYTHONPATH=. python main.py --task 'lccrchcc_hd' --h1_dim 220 --h2_dim 150 --h3_dim 70 --z_dim 10 --class_number 2 --mixup_alpha 0.5 --test_epoch 603  --only_test True
PYTHONPATH=. python main.py --task 'Healthy_LiverCancer' --h1_dim 240 --h2_dim 120 --h3_dim 80 --z_dim 60 --class_number 2 --mixup_alpha 0.5 --test_epoch 194 --only_test True
PYTHONPATH=. python main.py --task 'Healthy_LungCancer' --h1_dim 200 --h2_dim 180 --h3_dim 60 --z_dim 20 --class_number 2 --mixup_alpha 0.5 --test_epoch 962 --only_test True
PYTHONPATH=. python main.py --task 'Healthy_ColorectalCancer' --h1_dim 290 --h2_dim 160 --h3_dim 60 --z_dim 10 --class_number 2 --mixup_alpha 1.0 --test_epoch 757 --only_test True
PYTHONPATH=. python main.py --task 'lc_crc_hcc_hd' --h1_dim 355 --h2_dim 150 --h3_dim 80 --z_dim 10 --class_number 4 --mixup_alpha 0.5 --test_epoch 411 --only_test True
