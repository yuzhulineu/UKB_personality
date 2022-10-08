clc,clear

person_data=readtable('/share/inspurStorage/home1/Royce/dengyueting_data/personality/personality_followup5yrs_50yrs_unionset.csv');
%person_data=readtable('/share/inspurStorage/home1/Vinceyang/rqz/personality/Big_five_data/Big_five.csv');

person_id=table2array(person_data(:,2));
data_person=str2double(cellstr(table2array(person_data(:,23:27))));
person_name=person_data.Properties.VariableNames(23:27);


data_cov1=table2array(person_data(:,34:35));
data_cov2=str2double(cellstr(table2array(person_data(:,36))));
% data_cov3=table2array(person_data(:,34));
data_cov=[data_cov1 data_cov2];


cov_model1=data_cov;

load('/share/inspurStorage/home1/liyz/Project/acc/Comparision/UKB_DTI.mat')
DTI_id=table2array(DTIdata(:,1));
DTI_data=table2array(DTIdata(:,2:end));

load site_cov_id
%%
for kk=1:size(data_person,2)
    
    phenodataori= data_person(:,kk);
    phenodata=phenodataori(~isnan(phenodataori));
    phenoid=person_id(~isnan(phenodataori));
    
    
final_id = intersect_multi({phenoid;DTI_id;site_id});

[~,index]=intersect(person_id,final_id);
cov_final=cov_model1(index,:);
cov_final1=nan(size(cov_final));
for zz=1:size(cov_final,2)
    cov1=cov_final(:,zz);
    cov1(isnan(cov1))=nanmean(cov1);
    cov_final1(:,zz)=cov1;
end


[~, index] = intersect(site_id, final_id);
 site_cov_final = site_cov(index,:);

 
cov_final2 = [cov_final, site_cov_final];

[~,index]=intersect(phenoid,final_id);
pheno_final=phenodata(index,:);


 [~, index] = intersect(DTI_id, final_id);
 DTI_final = DTI_data(index,:);

parfor i=1:size(DTI_final,2)
    i
    Covariate = cov_final2;
    
    [C_value_DTI(i,:), P_value_DTI(i,:)] = partialcorr(DTI_final(:,i),pheno_final,Covariate,'rows','complete');
    
end
Resultperson_DTImodel1{kk}=[table2array(stru_nameuse(:,2)) num2cell(C_value_DTI) num2cell(P_value_DTI)];
end
save Resultperson_DTImodel1 Resultperson_DTImodel1
for kk=1:5
    aa=cell2table(Resultperson_DTImodel1{1,kk});
    writetable(aa,'Resultperson_model1.xls','Sheet',[person_name{kk},'_DTI']);
end
%%
for kk=size(data_person,2)
    
    phenodataori= data_person(:,kk);
    phenodata=phenodataori(~isnan(phenodataori));
    phenoid=person_id(~isnan(phenodataori));
    
    
final_id = intersect_multi({phenoid;DTI_id;site_id});

[~,index]=intersect(person_id,final_id);
cov_final=cov_model1(index,:);
cov_final1=nan(size(cov_final));
for zz=1:size(cov_final,2)
    cov1=cov_final(:,zz);
    cov1(isnan(cov1))=nanmean(cov1);
    cov_final1(:,zz)=cov1;
end


[~, index] = intersect(site_id, final_id);
 site_cov_final = site_cov(index,:);

 
cov_final2 = [cov_final1, site_cov_final];

[~,index]=intersect(phenoid,final_id);
pheno_final=phenodata(index,:);


 [~, index] = intersect(DTI_id, final_id);
 DTI_final = DTI_data(index,:);

parfor i=1:size(DTI_final,2)
    i
    Covariate = cov_final2;
    
    [C_value_DTI(i,:), P_value_DTI(i,:)] = BWAS_Fregression(DTI_final(:,i)',[pheno_final,pheno_final.^2],Covariate);
    
end
Resultperson_DTImodel1{kk+1}=[table2array(stru_nameuse(:,2)) num2cell(C_value_DTI) num2cell(P_value_DTI)];
end

 for kk=1:6
    aa=cell2table(Resultperson_DTImodel1{1,kk});
    if kk==6
        writetable(aa,'Resultperson_model1newdata.xls','Sheet',[person_name{kk-1},'_nonlinearDTI']);
    else
    writetable(aa,'Resultperson_model1newdata.xls','Sheet',[person_name{kk},'_DTI']);
    end
 end
 
 for kk=1:size(Resultperson_DTImodel1,2)
 result_FA=Resultperson_DTImodel1{kk}(1:27,:);
result_MD=Resultperson_DTImodel1{kk}(28:end,:);

fdr1_FA = mafdr(cell2mat(result_FA(:,3)), 'BHFDR', true);
fdr1_MD = mafdr(cell2mat(result_MD(:,3)), 'BHFDR', true);

result_FAfdr=[result_FA num2cell(fdr1_FA)];
result_MDfdr=[result_MD num2cell(fdr1_MD)];

result_personality_fdr{kk}=result_FAfdr;
 end
  for kk=1:5
    aa=cell2table(result_personality_fdr{1,kk});
    
    writetable(aa,'Resultperson_DTI.xls','Sheet',[person_name{kk},'_DTI']);
    end
 