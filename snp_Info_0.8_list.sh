#! /bin/bash

path1='/home1/UKB_Gene_v3/disk10t_2/batch/mfi/'
path2=/share/inspurStorage/home1/Vinceyang/wbs_data/zrq_GWAS/snp/

#_004_ukb_mfi_chr${i}_v3.txt: Imputation MAF+info provided by UKB
#extract snp with Info > 0.8
for i in {1..22};
do 
awk '{ if($8 <= 0.8) print $2}' ${path1}_004_ukb_mfi_chr${i}_v3.txt > ${path2}snp_chr${i}.txt
done

