rm(list = ls())
# Inport datasets
AMZN = read.table("AMZN.csv",skip = 0, header = TRUE, sep=",")
BAC = read.table("BAC.csv",skip = 0,header = TRUE, sep=",")
CAT = read.table("CAT.csv",skip = 0,header = TRUE, sep=",")
MCD = read.table("MCD.csv",skip = 0,header = TRUE, sep=",")
XOM = read.table("XOM.csv",skip = 0,header = TRUE, sep=",")

# Calculate stock returns as well as the change of volume
S1.Return = rep(1,nrow(AMZN)-1)
S1.VolumeChange = rep(1,nrow(AMZN)-1)
for(i in 1:length(S1.Return) ){
  S1.Return[i] = 100*(log(AMZN$Adj.Close[i+1])-log(AMZN$Adj.Close[i]))
  S1.VolumeChange[i] = AMZN$Volume[i+1]-AMZN$Volume[i]
}

S2.Return = rep(1,nrow(BAC)-1)
S2.VolumeChange = rep(1,nrow(BAC)-1)
for(i in 1:length(S2.Return) ){
  S2.Return[i] = 100*(log(BAC$Adj.Close[i+1])-log(BAC$Adj.Close[i]))
  S2.VolumeChange[i] = BAC$Volume[i+1]-BAC$Volume[i]
}

S3.Return = rep(1,nrow(CAT)-1)
S3.VolumeChange = rep(1,nrow(CAT)-1)
for(i in 1:length(S3.Return) ){
  S3.Return[i] = 100*(log(CAT$Adj.Close[i+1])-log(CAT$Adj.Close[i]))
  S3.VolumeChange[i] = CAT$Volume[i+1]-CAT$Volume[i]
}

S4.Return = rep(1,nrow(MCD)-1)
S4.VolumeChange = rep(1,nrow(MCD)-1)
for(i in 1:length(S4.Return) ){
  S4.Return[i] = 100*(log(MCD$Adj.Close[i+1])-log(MCD$Adj.Close[i]))
  S4.VolumeChange[i] = MCD$Volume[i+1]-MCD$Volume[i]
}

S4.Return = rep(1,nrow(MCD)-1)
S4.VolumeChange = rep(1,nrow(MCD)-1)
for(i in 1:length(S4.Return) ){
  S4.Return[i] = 100*(log(MCD$Adj.Close[i+1])-log(MCD$Adj.Close[i]))
  S4.VolumeChange[i] = MCD$Volume[i+1]-MCD$Volume[i]
}

S5.Return = rep(1,nrow(XOM)-1)
S5.VolumeChange = rep(1,nrow(XOM)-1)
for(i in 1:length(S5.Return) ){
  S5.Return[i] = 100*(log(XOM$Adj.Close[i+1])-log(XOM$Adj.Close[i]))
  S5.VolumeChange[i] = XOM$Volume[i+1]-XOM$Volume[i]
}

#Histogram the distribution of the returns
library(ggplot2)

qplot(S1.Return, geom = 'histogram') + xlab('AMZN Return') 
qplot(S2.Return, geom = 'histogram') + xlab('BAC Return')
qplot(S3.Return, geom = 'histogram') + xlab('CAT Return')
qplot(S4.Return, geom = 'histogram') + xlab('MOD Return')
qplot(S5.Return, geom = 'histogram') + xlab('XOM Return')

#Calculate the Mean Variance Skewness Kurtosis
library(moments)
MeanReturn = c(mean(S1.Return),mean(S2.Return),mean(S3.Return),mean(S4.Return),mean(S5.Return))
Varance = c(var(S1.Return),var(S2.Return),var(S3.Return),var(S4.Return),var(S5.Return))
Skewness = c(skewness(S1.Return),skewness(S2.Return),skewness(S3.Return),skewness(S4.Return),skewness(S5.Return))
Kurtosis =c(kurtosis(S1.Return),kurtosis(S2.Return),kurtosis(S3.Return),kurtosis(S4.Return),kurtosis(S5.Return))
MomentCaculation = cbind(MeanReturn,Varance,Skewness,Kurtosis)
rownames(MomentCaculation) = c("AMZN","BAC","CAT","MOD","XOM")
MomentCaculation#print the 1st-4th moment of Stock returns


#Do you think they are normally distributed?
#No because even though the Skeness is close to zero, the Kurtosis is far from 3

#Calculat the correlation between the return and volume for each and construct a correlation table for 5 stocks
S1 = c(cor(S1.Return,AMZN$Volume[-1]),cor(S1.Return,S1.VolumeChange))
S2 = c(cor(S2.Return,BAC$Volume[-1]),cor(S2.Return,S2.VolumeChange))
S3 = c(cor(S3.Return,CAT$Volume[-1]),cor(S3.Return,S3.VolumeChange))
S4 = c(cor(S4.Return,MCD$Volume[-1]),cor(S4.Return,S4.VolumeChange))
S5 = c(cor(S5.Return,XOM$Volume[-1]),cor(S5.Return,S5.VolumeChange))

CorrelationCalculation = rbind(S1,S2,S3,S4,S5)
colnames(CorrelationCalculation) = c("Return Vs. Volume", "Return Vs. dif.Volume")
CorrelationCalculation #print the correlation between return and volume/volume change


#(a)Computing following probabilities
#i.Prob(Return>0 at t|Return>0 at t-1)
#ii.Prob(Return>0 at t|Return<0 at t-1)
#iii.Prob(Return<0 at t|Return>0 at t-1)
#iv.Prob(Return<0 at t|Return<0 at t-1)
Count_i = 0
Count_ii = 0
Count_iii= 0
Count_iv = 0
Condition_Increase = 0
Condition_Decrease = 0

StockReturn = cbind(S1.Return,S2.Return,S3.Return,S4.Return,S5.Return)
StockVolumeChange = cbind(S1.VolumeChange,S2.VolumeChange,S3.VolumeChange,S4.VolumeChange,S5.VolumeChange)

Sr.Prob = matrix(NA,4,ncol(StockReturn))
for (c in 1:ncol(StockReturn)){
  for(i in 1:(nrow(StockReturn)-1)){
    
    if(StockReturn[i,c]>0){
      Condition_Increase = Condition_Increase+1
      if(StockReturn[i+1,c]>0){ Count_i = Count_i +1 }  
      else if(StockReturn[i+1,c]<0){ Count_iii = Count_iii+1  } 
    }
    else if(StockReturn[i,c]<0){
      Condition_Decrease = Condition_Decrease +1
      if(StockReturn[i+1,c]>0){
        Count_ii = Count_ii+1
      }else if(StockReturn[i+1,c]<0){
        Count_iv = Count_iv +1
      }
    }
  }
  Sr.Prob[1,c] = Count_i/Condition_Increase
  Sr.Prob[2,c] = Count_ii/Condition_Decrease
  Sr.Prob[3,c] = Count_iii/Condition_Increase
  Sr.Prob[4,c] = Count_iv/Condition_Decrease
}
colnames(Sr.Prob)=c("AMZN","BAC","CAT","MCD","XOM")
rownames(Sr.Prob)=c("P1","P2","P3","P4")
print(Sr.Prob)#Print the conditional probablity regarding lagged stock price

#(b)Computing following probabilities
#i.Prob(Return>0 at t|VolumeChange>0 at t-1)
#ii.Prob(Return>0 at t|VolumeChange<0 at t-1)
#iii.Prob(Return<0 at t|VolumeChange>0 at t-1)
#iv.Prob(Return<0 at t|VolumeChange<0 at t-1)


Count_i = 0
Count_ii = 0
Count_iii= 0
Count_iv = 0
Condition_Volume.Increase = 0
Condition_Volume.Decrease = 0

Sv.Prob = matrix(NA,4,ncol(StockReturn))

for (c in 1:ncol(StockReturn)){
for(i in 1:(nrow(StockReturn)-1)){
  if(StockVolumeChange[i,c]>0){
    Condition_Volume.Increase = Condition_Volume.Increase+1
    if(StockReturn[i+1,c]>0){ Count_i = Count_i +1 }  
    else if(StockReturn[i+1,c]<0){ Count_iii = Count_iii+1  } 
  }
  else if(StockVolumeChange[i,c]<0){
    Condition_Volume.Decrease = Condition_Volume.Decrease +1
    if(StockReturn[i+1,c]>0){
      Count_ii = Count_ii+1
    }else if(StockReturn[i+1,c]<0){
      Count_iv = Count_iv +1
    }
  }
}
Sv.Prob[1,c] = Count_i/Condition_Volume.Increase
Sv.Prob[2,c] = Count_ii/Condition_Volume.Decrease
Sv.Prob[3,c] = Count_iii/Condition_Volume.Increase
Sv.Prob[4,c] = Count_iv/Condition_Volume.Decrease
}
colnames(Sv.Prob)=c("AMZN","BAC","CAT","MCD","XOM")
rownames(Sv.Prob)=c("P1","P2","P3","P4")
print(Sv.Prob)#Print the conditional probablity regarding volume change

#Simulate T-1 data points for epsilon
epsilon = rnorm(length(S1.Return),2,3)

# Run a Regression in the order of AMZN, BAC, CAT, MOD and XOM.
for (i in 1:ncol(StockReturn)){
  model = lm(StockReturn[,i]~epsilon)
  print(c("This is the regression result of ",colnames(Sv.Prob)[i]))
  print(summary(model))
}

# Repeat Regression 1000 times test significance
iter = 1000
P_epsilon = matrix(NA, nrow = iter, ncol =5) #generate a iter*5 matrix
R_dgp.model = matrix(NA, nrow = iter, ncol =5) #generate a iter*5 matrix

for (i in 1:iter){
  set.seed(i)
  epsilon = rnorm(nrow(StockReturn),2,3)
  for (j in 1:ncol(StockReturn)){
    dgp.model = lm(StockReturn[,j]~epsilon)
    P_epsilon[i,j] = summary(dgp.model)$coefficients[2,4]
    R_dgp.model[i,j] = summary(dgp.model)$r.squared
  }
}
regressor_significant = ifelse(P_epsilon<0.005,1,0)
sum =colSums(regressor_significant,na.rm = T,dims=1)

Prob.regressor_significant = sum/iter

sigprob = matrix(NA, nrow=1,ncol=5)
sigprob[1,]=Prob.regressor_significant
colnames(sigprob)=c("AMZN","BAC","CAT","MOD","XOM")
rownames(sigprob)=c("P(coefficient's |P|<0.05)")
#We now know that the significant probabilities is very low
print(sigprob)

Rtab = matrix(NA, nrow=4, ncol = 5)
colnames(Rtab)=c("AMZN","BAC","CAT","MOD","XOM")
rownames(Rtab)=c("min","mean","median","max")
Rtab[1,] = min(R_dgp.model)
Rtab[2,] = mean(R_dgp.model)
Rtab[3,] = median(R_dgp.model)
Rtab[4,] = max(R_dgp.model)
print(Rtab)
#The R-square is also quite low and which means the model lacks credibility