#workspace prep
#dataframe structure 
install.packages("naniar")
library(naniar)
metadata <- read.csv("Documents/Data/metadata.csv")
df <- read.table("Documents/Data/countdata10.txt", 
                 header=T, row.names = 1)
str(df)
res = mcar_test(df)
res
res$p.value
vis_miss(df, warn_large_data= FALSE)
gg_miss_var(df) 

#missForest imputation
install.packages("missForest")
library(missForest)
library(dplyr)
#use sample data 
s = sample_n(df, 200)
set.seed(123)
df_imp <- missForest(s)

#MICE with Cart method 
install.packages("mice")
library(mice)
#using subset sample data 
mice_imp <- mice(s, meth="cart")
complete_data <- complete(mice_imp)
summary(complete_data)

#Amelia
install.packages("Amelia")
library(Amelia)
library(dplyr)
#using sample data
preprocessed_data <- amelia(s, m=1, ncpus=1, frontend=FALSE, p2s=1)

#KNN impute 
install.packages("performanceEstimation")
library(performanceEstimation)
knn_s1 = knnImp(s, k=5)
sum(is.na(knn_s))

#Regression imputation using llsImpute
BiocManager::install("pcaMethods")
library(pcaMethods)
impute_data <- llsImpute(s, k=10, 
                         correlation ="pearson", 
                         allVariables = TRUE)
impute_data@completeObs

#XGBoost/MixGB imputation
install.packages("mixgb")
library(mixgb)
parameters <- list(subsample =0.7)
mixgb_df <- mixgb(data=s, m=1, xgb.parameters, nrounds =50,
                  early_stopping_rounds=10)

#Matrix Factorisation/SVD 
install.packages("softImpute")
library(softImpute)
s_matrix <- as.matrix(s)
soft_data = softImputw(s_matrix, rank=35, lambda=30,
                       type='svd', maxit=30)
#compute factorisation
S_sft <- soft_data$u %*% diag(soft_data$d) %*% t(soft_data$v)
#replace missing values with computed values 
Ssft[which(!is.na(s_matrix))] <- s_matrix[which(!is.na(s_matrix))]
#convert matrix to database 
s_dataframe <- as.data.frame(S_sft)

#preserve row names
rownames(s_dataframe) <- rownames(s)
colnames(s_dataframe) <- colnames(s)



#PCA for data imputation
install.packages("missMDA")
library(missMDA)
ncp_pca <- estim_ncpPCA(s, method.cv='loo') $ncp
pca_imp <- imputePCA(s, ncp =ncp_pca)
data_pca <- pca_imp$complete0bs



