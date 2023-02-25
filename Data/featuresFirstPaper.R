
# load libraries
library(readr)
library(tsfeatures)
library(tseries)
library(PerformanceAnalytics)
library(DChaos)
library(forecast)



# load dataset
#datasetEst <- read_csv("C:/META_LEARNING/M4/datasetTeSelec.csv")
datasetEst <- read_csv("C:/META_LEARNING/M3/datasetEst.csv")


str(datasetEst)
# Creating dataframe 

names <- c("size","entropy","trend","season", "entropy","trend","linearity",
           "skewness", "kurtosis", "non_linear","hurt_long_term",
           "chaos", "white_noise", "outliers","variance_sta",
           "stacionarity")
           
df <- data.frame()
for (k in names) df[[k]] <- as.numeric()


# generate dataset with features
for (i in 1:nrow(datasetEst)){
  
  # prepare time series
  serie=datasetEst[i,2:ncol(datasetEst)]
  serieF=as.numeric(serie[colSums(!is.na(serie)) > 0])
  serieF=serieF[length(serieF)-18:length(serieF)]
  serieTs <- ts(serieF,  frequency=12)
  
  # compute features
  size=length(serieF)
  ent=entropy(serieF)
  tre=stl_features(serieTs)[3]
  sea=stl_features(serieTs)[9]
  lin=stl_features(serieTs)[5]
  ske=skewness(serieF)
  kur=kurtosis(serieF)
  non=nonlinearity(serieF)
  hur=hurst(serieF)
  #cao=lyapunov(serieF, doplot=FALSE)$exponent.mean[2]
  wno=Box.test(serieF, lag = 12, type = c("Box-Pierce"))[1]
  out=length(tsoutliers(serieF)$index)/length(serieF)
  vst=lumpiness(serieF, 12)
  sta=adf.test(serieF, k=12)$p.value
  
  
  
  #df[i,]=c(size, ent, tre, sea, lin, ske, kur,non, hur, cao, wno,out, vst, sta)
  df[i,]=c(size, ent, tre, sea, lin, ske, kur,non, hur,  wno,out, vst, sta)
  
}

# save results

# include id
df_F=cbind(datasetEst[,1], df)
colnames(df_F)[1]='id'




# save results
#write.csv(df_F,"C:/META_LEARNING/Script_R_features/featuresFirstPaper.csv", row.names = FALSE)
write.csv(df_F,"C:/META_LEARNING/Script_R_features/featuresFirstPaperM3.csv", row.names = FALSE)


