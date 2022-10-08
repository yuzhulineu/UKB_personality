clc,clear

person_data=readtable('/share/inspurStorage/home1/Royce/dengyueting_data/personality/personality_followup5yrs_50yrs_unionset.csv');
%person_data=readtable('/share/inspurStorage/home1/Vinceyang/rqz/personality/Big_five_data/Big_five.csv');

person_id=table2array(person_data(:,2));
data_person=str2double(cellstr(table2array(person_data(:,23:27))));
person_name=person_data.Properties.VariableNames(23:27);

data_disorder=table2array(person_data(:,48));

WH=readtable('/share/inspurStorage/home1/liyz/Project/acc/WMH_full.csv');
WH_id=table2array(WH(:,1));
WH_data=table2array(WH(:,2));


data_cov1=table2array(person_data(:,34:35));
data_cov2=str2double(cellstr(table2array(person_data(:,36))));
% data_cov3=table2array(person_data(:,34));
data_cov=[data_cov1 data_cov2];


cov_model1=data_cov;
%cov_model2=data_cov(:,1:5);
%cov_model3=data_cov(:,1:11);


% UKB_aparc_Area=readtable('/share/inspurStorage/home1/ISTBI_data/UKB_Freesurfer/UKB_2_0_aparc_SurfArea.csv');
% UKB_aparc_Volume=readtable('/share/inspurStorage/home1/ISTBI_data/UKB_Freesurfer/UKB_2_0_aparc_GrayVol.csv');
% UKB_aparc_Thickness=readtable('/share/inspurStorage/home1/ISTBI_data/UKB_Freesurfer/UKB_2_0_aparc_ThickAvg.csv');
% newimageid=table2array(UKB_aparc_Area(:,1));
% UKB_aparc_Area.Properties.VariableNames(2:end)=UKB_aparc_Area1.Properties.VariableNames(1:end-1);
% UKB_aparc_Volume.Properties.VariableNames(2:end)=UKB_aparc_Volume1.Properties.VariableNames(1:end-1);
% UKB_aparc_Thickness.Properties.VariableNames(2:end)=UKB_aparc_Thickness1.Properties.VariableNames(1:end-1);
% save UKB_aparcnew UKB_aparc_Area UKB_aparc_Volume UKB_aparc_Thickness
load /share/inspurStorage/home1/liyz/Project/acc/UKB_aparcnew
newimageid=table2array(UKB_aparc_Area(:,1));


UKB_aseg_Volume=readtable('/share/inspurStorage/home1/liyz/Project/acc/UKB_2_0_aseg_Volume.csv');
imageid_aseg=table2array(UKB_aseg_Volume(:,1));
load /share/inspurStorage/home1/liyz/Project/acc/site_cov_id.mat
%%
Resultperson_aparclinear=cell(1,6);
Resultperson_aseglinear=cell(1,6);
final_id=nan(6,1);
for kk=1:size(data_person,2)
    
    phenodataori= data_person(:,kk);
    phenodata=phenodataori(~isnan(phenodataori));
    phenoid=person_id(~isnan(phenodataori));
    
    
final_id(kk) = size(intersect_multi({phenoid;newimageid;site_id;imageid_aseg}),1);

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

cov_final2 = [cov_final1 site_cov_final];

[~,index]=intersect(phenoid,final_id);
pheno_final=phenodata(index,:);

[~,index]=intersect(newimageid,final_id);
UKB_area_final=UKB_aparc_Area(index,2:end);
UKB_thickness_final=UKB_aparc_Thickness(index,2:end);
UKB_volume_final=UKB_aparc_Volume(index,2:end); %image_posteriorcingulate=double(cell2mat(table2cell(UKB_volume_final(:,55))));
UKB_aparc=[UKB_area_final UKB_thickness_final UKB_volume_final];
aparcname=UKB_aparc.Properties.VariableNames;
UKB_aparc_final=table2array(UKB_aparc);

[~,index]=intersect(imageid_aseg,final_id);
UKB_aseg_final=table2array(UKB_aseg_Volume(index,2:46));
asegname=UKB_aseg_Volume.Properties.VariableNames;

C_value_aparc = nan(204,1);
P_value_aparc= nan(204,1);
parfor i=1:size(UKB_aparc,2)
    i
    Covariate = cov_final2;
    
    [C_value_aparc(i,:), P_value_aparc(i,:)] = partialcorr(UKB_aparc_final(:,i),pheno_final,Covariate,'rows','complete');
end
Resultperson_aparclinear{kk}=[aparcname' num2cell(C_value_aparc) num2cell(P_value_aparc)];


C_value_aseg = nan(45,1);
P_value_aseg= nan(45,1);
parfor i=1:size(UKB_aseg_final,2)
    i
    Covariate = cov_final2;
    
    [C_value_aseg(i,:), P_value_aseg(i,:)] = partialcorr(UKB_aseg_final(:,i),pheno_final,Covariate,'rows','complete');
end
Resultperson_aseglinear{kk}=[asegname(2:46)' num2cell(C_value_aseg) num2cell(P_value_aseg)];
end
% save Resultperson_aparclinear Resultperson_aparclinear
% save Resultperson_aseglinear Resultperson_aseglinear
% for kk=1:5
%     aa=cell2table(Resultperson_aparclinear{1,kk});
%     writetable(aa,'Resultperson_model1_complete2.xls','Sheet',[person_name{kk},'_cortex']);
% end
% 
% for kk=1:5
%     aa=cell2table(Resultperson_aseglinear{1,kk});
%     writetable(aa,'Resultperson_model1_complete2.xls','Sheet',[person_name{kk},'_subcortex']);
% end
%% nonlinear curiosity
  for kk=size(data_person,2)
    
    phenodataori= data_person(:,kk);
    phenodata=phenodataori(~isnan(phenodataori));
    phenoid=person_id(~isnan(phenodataori));
    
    
final_id = intersect_multi({phenoid;newimageid;site_id;imageid_aseg});

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

cov_final2 = [cov_final1 site_cov_final];

[~,index]=intersect(phenoid,final_id);
pheno_final=phenodata(index,:);

[~,index]=intersect(newimageid,final_id);
UKB_area_final=UKB_aparc_Area(index,2:end);
UKB_thickness_final=UKB_aparc_Thickness(index,2:end);
UKB_volume_final=UKB_aparc_Volume(index,2:end); %image_posteriorcingulate=double(cell2mat(table2cell(UKB_volume_final(:,55))));
UKB_aparc=[UKB_area_final UKB_thickness_final UKB_volume_final];
aparcname=UKB_aparc.Properties.VariableNames;
UKB_aparc_final=table2array(UKB_aparc);

[~,index]=intersect(imageid_aseg,final_id);
UKB_aseg_final=table2array(UKB_aseg_Volume(index,2:46));
asegname=UKB_aseg_Volume.Properties.VariableNames;

C_value_aparc = nan(204,1);
P_value_aparc= nan(204,1);
parfor i=1:size(UKB_aparc,2)
    i
    Covariate = cov_final2;
    
    [C_value_aparc(i,:), P_value_aparc(i,:)] = BWAS_Fregression(UKB_aparc_final(:,i)',[pheno_final,pheno_final.^2],Covariate);
    
end
Resultperson_aparclinear{kk+1}=[aparcname' num2cell(C_value_aparc) num2cell(P_value_aparc)];


C_value_aseg = nan(45,1);
P_value_aseg= nan(45,1);
parfor i=1:size(UKB_aseg_final,2)
    i
    Covariate = cov_final2;
    
    [C_value_aseg(i,:), P_value_aseg(i,:)] = BWAS_Fregression(UKB_aseg_final(:,i)',[pheno_final,pheno_final.^2],Covariate);
end
Resultperson_aseglinear{kk+1}=[asegname(2:46)' num2cell(C_value_aseg) num2cell(P_value_aseg)];
 end
 
 Resultperson_aparcmodel1new=Resultperson_aparclinear;
 Resultperson_asegmodel1new=Resultperson_aseglinear;
   save Resultperson_aparcmodel1new Resultperson_aparcmodel1new
  save Resultperson_asegmodel1new Resultperson_asegmodel1new
 
 for kk=1:6
    aa=cell2table(Resultperson_aparclinear{1,kk});
    if kk==6
        writetable(aa,'Resultperson_model1newdata.xls','Sheet',[person_name{kk-1},'_nonlinearcortex']);
    else
    writetable(aa,'Resultperson_model1newdata.xls','Sheet',[person_name{kk},'_cortex']);
    end
end

for kk=1:6
    aa=cell2table(Resultperson_aseglinear{1,kk});
    if kk==6
        writetable(aa,'Resultperson_model1newdata.xls','Sheet',[person_name{kk-1},'_nonlinearsubcortex']);
    else
    writetable(aa,'Resultperson_model1newdata.xls','Sheet',[person_name{kk},'_subcortex']);
    end
end