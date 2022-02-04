icc<-function(data, item){
  library(psych)
pseudob<-0 #JUST CREATING A PLACEHOLDER FOR pseudob SO THE FUNCTION BELOW CAN RUN
ahat<-function(x){
  r<-(((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)

  ((0.51+(0.02*pseudob)+(0.301*pseudob^2))*r)+((0.57-(0.009*pseudob)+(0.19*pseudob^2))*r)

}#FUNCTION TO ESTIMATE THE CTT-A STATISTIC, WHICH IS THE EQUIVALENT TO THE DISCRIMINATION STATISTIC IN IRT


alphas<-alpha(data, check.keys=TRUE)#COMPUTING ALPHAS FOR ALL 100 ITEMS. WE NEED THIS IN ORDER TO GET THE CORRECTED ITEM-TOTAL CORRELATIONS, WHICH WE THEN USE FOR COMPUTING THE CTT-A STATISTIC.
citcs<-data.frame(alphas$item.stats$r.drop)#ACCESSING THE CORRECTED ITEM-TOTAL CORRELATIONS INSIDE alphas.
pseudoA<-data.frame(ahat(citcs))#USING THE ahat FUNCTION TO CALCULATE THE CTT-A PARAMETER FOR ALL 100 ITEMS. CORRECTED ITEM-TOTAL CORRELATION ARE ENTERED AS AN ARGUMENT.
pseudoB<-data.frame(qnorm(colMeans(data)))#CALCULATING THE CTT-B PARAMETER, WHICH IS JUST THE PROBABILITIES OF ANSWERING RIGHT FOR EACH ITEM.
df<-as.data.frame(cbind(citcs, pseudoA, pseudoB))#PUTTING ALL RELEVANT STATISTIC TOGETHER
colnames(df)<-c("CITC", "PseudoA", "PseudoB")#RENAMING COLUMN HEADERS
c<-0
pseudob<-df$PseudoB[item]
pseudoa<-df$PseudoA[item]
# pseudob
# pseudoa
eq <- function(x){c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob))))))}#FUNCTION THAT CREATES ICC BASED ON pseudob AND pseudoa

curve(eq, col="red", xlim=c(-4,4), ylim=c(0,1), main="Item Characteristic Curve")#PLOTTING CTT-ICC AND IRT-ICC SIDE BY SIDE.
output<-cbind(pseudob, pseudoa)
return(output)
}

# library(psych)                            ## Need to add this as dependency
# data <- read.csv("testdata.csv")          ## Example data, maybe add within package
# data<- na.omit(data)
# icc(data,2)
# library(cttv)

icc(testdata, 3:20)

