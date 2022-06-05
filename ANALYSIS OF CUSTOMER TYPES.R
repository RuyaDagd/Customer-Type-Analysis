
#Reading the data
library(readxl)
data <- read_excel("Data.xls")
head(data)
str(data)
unique(data$`Last Installment Payment Before Regular/Late Payment`)
str(data)
#Creating new column that shows last payment amount for both regular amounts and others
data$LastPayment<-ifelse(data$`Last Installment Payment Before Regular/Late Payment`=="Regular Amount",(data$`Credit Amount`)/(data$`Installment (Months)`)*(1.10),data$`Last Installment Payment Before Regular/Late Payment`)
data$LastPayment
unique(data$`Installment (Months)`)
data$closemonth<-ifelse(data$`Last Installment Payment Before Regular/Late Payment`!="Regular Amount",(data$`Credit Amount`-as.numeric(data$`Last Installment Payment Before Regular/Late Payment`))/((data$`Credit Amount`)/(data$`Installment (Months)`))*(1.10),0)
round(data$closemonth,3)

#PROBLEM1
#Arranging the dataset
data1<-data
data1$`Customer Type`<-ifelse(data1$`Customer Type`=="Deferred","deferred","non-default")
head(data1)

#Explaratory Data Analysis

#Marital status vs Customer types
maritaltable <- table(data1$`Marital Status`, data1$`Customer Type`)
maritalproptable <- round(prop.table(table(data1$`Marital Status`, data1$`Customer Type`), margin = 1), 2)
maritaldata <- data.frame(maritalproptable)
colnames(maritaldata) <- c("MaritalStatus", "CustomerTypes", "Frequency")
maritaldata$label <- paste(maritaldata$Frequency*100, "%", sep = "")
maritaldata

library(ggplot2)
maritalPlot <- ggplot(maritaldata, aes(MaritalStatus, CustomerTypes, label = label)) +
  geom_point(aes(size = Frequency), alpha=0.8, color = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"), show.legend=FALSE) +
  geom_label(size = 3,colour = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"),fontface = "bold.italic")+
  scale_size(range = c(15,50)) +
  theme_bw() + theme(panel.grid.major = element_line(colour = "gray86"), 
                     panel.grid.minor = element_line(colour = "gray93"), 
                     panel.background = element_rect(fill = "white"))+labs(x = "Marital Status")
maritalPlot
table(data1$`Marital Status`)
# Gender vs Customer types
maritaltable <- table(data1$`Gender`, data1$`Customer Type`)
maritalproptable <- round(prop.table(table(data1$Gender, data1$`Customer Type`), margin = 1), 2)
maritaldata <- data.frame(maritalproptable)
colnames(maritaldata) <- c("Gender", "CustomerTypes", "Frequency")
maritaldata$label <- paste(maritaldata$Frequency*100, "%", sep = "")
maritaldata

library(ggplot2)
maritalPlot <- ggplot(maritaldata, aes(Gender, CustomerTypes, label = label)) +
  geom_point(aes(size = Frequency), alpha=0.8, color = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"), show.legend=FALSE) +
  geom_label(size = 3,colour = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"),fontface = "bold.italic")+
  scale_size(range = c(15,50)) +
  theme_bw() + theme(panel.grid.major = element_line(colour = "gray86"), 
                     panel.grid.minor = element_line(colour = "gray93"), 
                     panel.background = element_rect(fill = "white"))+labs(x = "Gender")
maritalPlot
table(data1$Gender)
# Age vs Customer types
library(ggplot2)
library(ggpubr)
ageDensity<-ggdensity(data1, x = "Age",
                      add = "mean",
                      color = "Customer Type", fill = "Customer Type",
                      palette = c("deepskyblue3","deepskyblue4"))+ 
  theme(legend.title = element_text(face = "bold.italic")) +labs(colour = "Customer Type", fill = "Customer Type") + theme(plot.title = element_text(hjust = 0.5), 
                                                                                                                           legend.text = element_text(face = "italic"), 
                                                                                                                           legend.position = "top") +labs(title = "")+labs(y = "Density of Age")
ageDensity
mean(data1$Age[data1$`Customer Type`=="non-default"])
mean(data1$Age[data1$`Customer Type`=="deferred"])

#Installment month vs Customer types
median(data1$`Installment (Months)`)
mean(data1$`Installment (Months)`)
data2<-data1
data2$`Installment (Months)`<-ifelse(data2$`Installment (Months)`<=48,"2 or less","more than 2 years")
tablemonth <- table(data2$`Customer Type`, data2$`Installment (Months)`)
tablemonth <- round(prop.table(tablemonth, margin = 2),2)
tablemonth
plotmonth <- data.frame(tablemonth)
plotmonth$label<- paste(plotmonth$Freq*100, paste("%",sep=""),sep="")

library(ggplot2)
month_plot <- ggplot(plotmonth, aes(x = Var2, y = Freq, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_label(size = 4, position = position_stack(vjust = 0.5),colour = "white",fontface = "bold")+
  scale_fill_manual(values=c("deepskyblue3","deepskyblue4")) + 
  theme_minimal() + 
  xlab("installment(year)")+
  ylab(" ")+
  theme(plot.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10, 
                                                                                 face = "italic"), legend.title = element_text(size = 10, 
                                                                                                                               face = "italic")) +labs(title = "",                                                                                                                                                 fill = "Customer Type") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
month_plot

#Credit amount vs Customer types
mean(data1$`Credit Amount`)
class(data1$`Credit Amount`)
data1$`Credit Amount`
data3<-data1
data3$CreditAmount<-ifelse(data3$`Credit Amount`<=396531.7,"below average","above avarage")
tablemonth <- table(data3$`Customer Type`, data3$CreditAmount)
tablemonth <- round(prop.table(tablemonth, margin = 2),2)
tablemonth
plotmonth <- data.frame(tablemonth)
plotmonth$label<- paste(plotmonth$Freq*100, paste("%",sep=""),sep="")

library(ggplot2)
month_plot <- ggplot(plotmonth, aes(x = Var2, y = Freq, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_label(size = 4, position = position_stack(vjust = 0.5),colour = "white",fontface = "bold")+
  scale_fill_manual(values=c("deepskyblue3", "deepskyblue4")) + 
  theme_minimal() + 
  xlab("Credit Amount")+
  ylab(" ")+
  theme(plot.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10, 
                                                                                 face = "italic"), legend.title = element_text(size = 10, 
                                                                                                                               face = "italic")) +labs(title = "", 
                                                                                                                                                       fill = "Customer Type") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
month_plot

#Credit Type
maritaltable <- table(data1$`Credit Type`, data1$`Customer Type`)
maritalproptable <- round(prop.table(table(data1$`Credit Type`, data1$`Customer Type`), margin = 1), 2)
maritaldata <- data.frame(maritalproptable)
colnames(maritaldata) <- c("CreditType", "CustomerTypes", "Frequency")
maritaldata$label <- paste(maritaldata$Frequency*100, "%", sep = "")
maritaldata

library(ggplot2)
maritalPlot <- ggplot(maritaldata, aes(CreditType, CustomerTypes, label = label)) +
  geom_point(aes(size = Frequency), alpha=0.8, color = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"), show.legend=FALSE) +
  geom_label(size = 3,colour = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"),fontface = "bold.italic")+
  scale_size(range = c(15,50)) +
  theme_bw() + theme(panel.grid.major = element_line(colour = "gray86"), 
                     panel.grid.minor = element_line(colour = "gray93"), 
                     panel.background = element_rect(fill = "white"))+labs(x = "Credit Type")
maritalPlot

#PROBLEM2
data4<-data
data4<-data4[!(data4$`Customer Type`=="Deferred") ,]
unique(data4$`Customer Type`)

#Explaratory Data Analysis
#Marital status vs Customer types
maritaltable <- table(data4$`Marital Status`, data4$`Customer Type`)
maritalproptable <- round(prop.table(table(data4$`Marital Status`, data4$`Customer Type`), margin = 1), 2)
maritaldata <- data.frame(maritalproptable)
colnames(maritaldata) <- c("MaritalStatus", "CustomerTypes", "Frequency")
maritaldata$label <- paste(maritaldata$Frequency*100, "%", sep = "")
maritaldata

library(ggplot2)
maritalPlot <- ggplot(maritaldata, aes(MaritalStatus, CustomerTypes, label = label)) +
  geom_point(aes(size = Frequency), alpha=0.8, color = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"), show.legend=FALSE) +
  geom_label(size = 3,colour = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"),fontface = "bold.italic")+
  scale_size(range = c(15,50)) +
  theme_bw() + theme(panel.grid.major = element_line(colour = "gray86"), 
                     panel.grid.minor = element_line(colour = "gray93"), 
                     panel.background = element_rect(fill = "white"))+labs(x = "Marital Status")
maritalPlot

# Gender vs Customer types
maritaltable <- table(data4$`Gender`, data4$`Customer Type`)
maritalproptable <- round(prop.table(table(data4$Gender, data4$`Customer Type`), margin = 1), 2)
maritaldata <- data.frame(maritalproptable)
colnames(maritaldata) <- c("Gender", "CustomerTypes", "Frequency")
maritaldata$label <- paste(maritaldata$Frequency*100, "%", sep = "")
maritaldata

library(ggplot2)
maritalPlot <- ggplot(maritaldata, aes(Gender, CustomerTypes, label = label)) +
  geom_point(aes(size = Frequency), alpha=0.8, color = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"), show.legend=FALSE) +
  geom_label(size = 3,colour = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"),fontface = "bold.italic")+
  scale_size(range = c(15,50)) +
  theme_bw() + theme(panel.grid.major = element_line(colour = "gray86"), 
                     panel.grid.minor = element_line(colour = "gray93"), 
                     panel.background = element_rect(fill = "white"))+labs(x = "Gender")
maritalPlot
# Age vs Customer types
library(ggplot2)
library(ggpubr)
ageDensity<-ggdensity(data4, x = "Age",
                      add = "mean",
                      color = "Customer Type", fill = "Customer Type",
                      palette = c("deepskyblue3", "deepskyblue4"))+ 
  theme(legend.title = element_text(face = "bold.italic")) +labs(colour = "Customer Type", fill = "Customer Type") + theme(plot.title = element_text(hjust = 0.5), 
                                                                                                                           legend.text = element_text(face = "italic"), 
                                                                                                                           legend.position = "top") +labs(title = "")+labs(y = "Density of Age")

ageDensity
#Installment month vs Customer types
median(data4$`Installment (Months)`)
mean(data4$`Installment (Months)`)
data5<-data4
data5$`Installment (Months)`<-ifelse(data5$`Installment (Months)`<=48,"2 or less","more than 2 years")
tablemonth <- table(data5$`Customer Type`, data5$`Installment (Months)`)
tablemonth <- round(prop.table(tablemonth, margin = 2),2)
tablemonth
plotmonth <- data.frame(tablemonth)
plotmonth$label<- paste(plotmonth$Freq*100, paste("%",sep=""),sep="")

library(ggplot2)
month_plot <- ggplot(plotmonth, aes(x = Var2, y = Freq, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_label(size = 4, position = position_stack(vjust = 0.5),colour = "white",fontface = "bold")+
  scale_fill_manual(values=c("deepskyblue3", "deepskyblue4")) + 
  theme_minimal() + 
  xlab("Installment(years")+
  ylab(" ")+
  theme(plot.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10, 
                                                                                 face = "italic"), legend.title = element_text(size = 10, 
                                                                                                                               face = "italic")) +labs(title = "", 
                                                                                                                                                       fill = "Customer Types") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
month_plot

#Credit amount vs Customer types
mean(data4$`Credit Amount`)
data6<-data4
data6$`Credit Amount`<-ifelse(data6$`Credit Amount`<=420099.2,"below avarage","above avarage")
unique(data6$`Credit Amount`)
tablemonth <- table(data6$`Customer Type`, data6$`Credit Amount`)
tablemonth <- round(prop.table(tablemonth, margin = 2),2)
tablemonth
plotmonth <- data.frame(tablemonth)
plotmonth$label<- paste(plotmonth$Freq*100, paste("%",sep=""),sep="")

library(ggplot2)
month_plot <- ggplot(plotmonth, aes(x = Var2, y = Freq, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_label(size = 4, position = position_stack(vjust = 0.5),colour = "white",fontface = "bold")+
  scale_fill_manual(values=c("deepskyblue3", "deepskyblue4")) + 
  theme_minimal() + 
  xlab("Credit Amount")+
  ylab(" ")+
  theme(plot.title = element_text(size = 12)) + theme(legend.text = element_text(size = 10, 
                                                                                 face = "italic"), legend.title = element_text(size = 10, 
                                                                                                                               face = "italic")) +labs(title = "", 
                                                                                                                                                       fill = "Customer Types") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
month_plot

#Credit Type
maritaltable <- table(data4$`Credit Type`, data4$`Customer Type`)
maritalproptable <- round(prop.table(table(data4$`Credit Type`, data4$`Customer Type`), margin = 1), 2)
maritaldata <- data.frame(maritalproptable)
colnames(maritaldata) <- c("CreditType", "CustomerTypes", "Frequency")
maritaldata$label <- paste(maritaldata$Frequency*100, "%", sep = "")
maritaldata

library(ggplot2)
maritalPlot <- ggplot(maritaldata, aes(CreditType, CustomerTypes, label = label)) +
  geom_point(aes(size = Frequency), alpha=0.8, color = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"), show.legend=FALSE) +
  geom_label(size = 3,colour = c("deepskyblue3", "deepskyblue3","deepskyblue4", "deepskyblue4"),fontface = "bold.italic")+
  scale_size(range = c(15,50)) +
  theme_bw() + theme(panel.grid.major = element_line(colour = "gray86"), 
                     panel.grid.minor = element_line(colour = "gray93"), 
                     panel.background = element_rect(fill = "white"))+labs(x = "Credit Type")
maritalPlot


#####MODELLING PART#####

#PROBLEM1- MODELLING PART

#Checking normality

#Converting the variable categories
data1$Gender<-ifelse(data1$Gender=="Female",1,0)
data1$`Marital Status`<-ifelse(data1$`Marital Status`=="Married",1,0)
data1$`Customer Type`<-ifelse(data1$`Customer Type`=="deferred",1,0)
data1$Gender<-as.factor(data1$Gender)
data1$`Marital Status`<-as.factor(data1$`Marital Status`)
data1$`Customer Type`<-as.factor(data1$`Customer Type`)
data1$`Credit Type`<-as.factor(data1$`Credit Type`)
str(data1)
data1$LastPayment<-as.numeric(data1$LastPayment)
#Chi-square test
tablecredittype<-table(data1$`Customer Type`,data1$`Credit Type`)
tablecredittype
chisq.test(tablecredittype)
#since p value is less than 0.05 significant level, there is a relationship between variables.
#H0:There is no relationship between customer type and credit type
tablecreditamount<-table(data1$`Customer Type`,data1$`Credit Amount`)
tablecreditamount
chisq.test(tablecreditamount)
#since chi square approach is not suitable in here, we use fisher test.
fisher.test(tablecreditamount,simulate.p.value = TRUE)
#since p value is less than 0.05 significant level, there is a relationship between variables.
tableinstallment<-table(data1$`Customer Type`,data1$`Installment (Months)`)
chisq.test(tableinstallment)
fisher.test(tableinstallment,simulate.p.value = TRUE)
tablelatepayment<-table(data1$`Customer Type`,data1$`Late Payment (Days)`)
chisq.test(tablelatepayment)
fisher.test(tablelatepayment,simulate.p.value = TRUE)
tableage<-table(data1$`Customer Type`,data1$Age)
chisq.test(tableage)
fisher.test(tableage,simulate.p.value = TRUE)
tablegender<-table(data1$`Customer Type`,data1$Gender)
chisq.test(tablegender)
tablemaritalstatus<-table(data1$`Customer Type`,data1$`Marital Status`)
chisq.test(tablemaritalstatus)
tablenumberofdependents<-table(data1$`Customer Type`,data1$`# of Dependents`)
chisq.test(tablenumberofdependents)
tablelastpayment<-table(data1$`Customer Type`,data1$LastPayment)
chisq.test(tablelastpayment)
fisher.test(tablelastpayment,simulate.p.value = TRUE)

#Summary stat
summary(data1)
#Correlation
pr1numeric<-data1
str(pr1numeric)
pr1numeric<-pr1numeric[,-c(1,2,6,8,9,11)]

pr1numeric
CorrTable<-cor(pr1numeric)
CorrTable
library(corrplot)
corrplot(CorrTable)

#VIF
#Box plot
#Variable importance
#Transformation

#Normality check
str(data1)
set.seed(1)
creditamount<-data1$Age
creditamount2 <- creditamount - mean(creditamount) #centering
creditamount3 <- (creditamount - mean(creditamount)) / sd(creditamount) #scaling
d<-data.frame(creditamount,creditamount2,creditamount3)
head(d)
library(ggplot2)
library(gridExtra)

p1<-ggplot(d,aes(x=creditamount))+geom_histogram(aes(y=stat(density)))+labs(title="Histogram of Original X",y="Count",x="Carat")+geom_density(col="darkred")
p2<-ggplot(d,aes(x=creditamount2))+geom_histogram(aes(y=stat(density)))+labs(title="Histogram of Centered X",y="Count",x="Carat")+geom_density(col="darkred") # Center of x has changed
p3<-ggplot(d,aes(x=creditamount3))+geom_histogram(aes(y=stat(density)))+labs(title="Histogram of Scaled X",y="Count",x="Carat")+geom_density(col="darkred")# Both the location and scale of x shanged

grid.arrange(p1,p2,p3,nrow=3)

df<-data.frame(data1$`Credit Amount`)
ggplot(df, aes(sample = data1$`Credit Amount`))+ stat_qq() + stat_qq_line()
#########################################################
#str(data1)
#Zstandardization <- function(givenVector){return((givenVector-mean(givenVector))/sd(givenVector))}

#data1$`Credit Amount` <- Zstandardization(data1$`Credit Amount`)
#data1$`Installment (Months)` <- Zstandardization(data1$`Installment (Months)`)
#data1$`Late Payment (Days)` <- Zstandardization(data1$`Late Payment (Days)`)
#data1$Age <- Zstandardization(data1$Age)
#data1$`# of Dependents` <- Zstandardization(data1$`# of Dependents`)
#data1$LastPayment <- Zstandardization(data1$LastPayment)
#str(data1)
#min-max normalization
data1$LastPayment<-as.numeric(data1$LastPayment)
data1$`Credit Amount` <-(data1$`Credit Amount`-min(data1$`Credit Amount`)) /
  (max(data1$`Credit Amount`)-min(data1$`Credit Amount`))
data1$`Installment (Months)` <-(data1$`Installment (Months)`-min(data1$`Installment (Months)`)) /
  (max(data1$`Installment (Months)`)-min(data1$`Installment (Months)`))
data1$`Late Payment (Days)` <-(data1$`Late Payment (Days)`-min(data1$`Late Payment (Days)`)) /
  (max(data1$`Late Payment (Days)`)-min(data1$`Late Payment (Days)`))
data1$Age <-(data1$Age-min(data1$Age)) /
  (max(data1$Age)-min(data1$Age))
data1$`# of Dependents` <-(data1$`# of Dependents`-min(data1$`# of Dependents`)) /
  (max(data1$`# of Dependents`)-min(data1$`# of Dependents`))
data1$LastPayment <-(data1$LastPayment-min(data1$LastPayment)) /
  (max(data1$LastPayment)-min(data1$LastPayment))
####
#Modelling for problem1
#Splitting data into training and testing
smpsize <- floor(0.8 * nrow(data1))
set.seed(123)
train1 <- sample(seq_len(nrow(data1)), size = smpsize)
train <- data1[train1, ]
test <- data1[-train1, ]

#Logistic Regression

str(data1)
train$`Customer Type`<-as.factor(train$`Customer Type`)
model_glm = glm(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`+LastPayment, data = train, family = "binomial")
summary(model_glm)
#model_glm1 = glm(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+`# of Dependents`+LastPayment, data = train, family = "binomial")
#summary(model_glm1)

model_glm_pred = ifelse(predict(model_glm, type = "response") > 0.465, 1,0)

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
calc_class_err(actual = train$`Customer Type`, predicted = model_glm_pred)
train_tab = table(predicted = model_glm_pred, actual = train$`Customer Type`)
library(caret)
train_con_mat = confusionMatrix(train_tab, positive = "1")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])

model_glm_pred_test= ifelse(predict(model_glm, type = "response",newdata = test) > 0.5, 1, 0)
test_tab = table(predicted = model_glm_pred_test, actual = test$`Customer Type`)
library(caret)
test_con_mat = confusionMatrix(test_tab, positive = "1")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])

library(pROC)
test_prob=predict(model_glm, type = "response",newdata = test)
test_roc = roc(test$`Customer Type` ~ test_prob, plot = TRUE, print.auc = TRUE)

####
table(data$`Last Installment Payment Before Regular/Late Payment`)
table(data$`Customer ID`)
data[which(data$`Late Payment (Days)`>550),]
a<-data[which(data$closemonth<0),]

#Random forest for problem 1

#RANDOM FOREST
library(rpart)
fit8 <- rpart(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`+LastPayment, data = train, method = 'class') #method = class is necessary for classification task
print(fit8)
head(train)
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
## Train data performance.
str(train)
train_pred <-predict(fit8,train[,c(2,3,4,5,7,8,9,10,12,13)],type = 'class')
round(calc_class_err(train_pred,train[,11]),3)
train_pred<-as.data.frame(train_pred)
sum(is.na(train_pred))
sum(is.na(train))
# Accuracy Sensivity and Specificity
library(caret)
train_tab = table(predicted = train_pred , actual = train[,11])
train_con_mat = confusionMatrix(train_tab)
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])

#Naive bayes for problem1

#NAIVE BAYES Classifier 
library(e1071)
NBclassfier=naiveBayes(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`+LastPayment, data = train)
print(NBclassfier)
#CE function
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
## Train data performance.

train_pred <-predict(NBclassfier,train[,c(2,3,4,5,7,8,9,10,12,13)],type = 'class')
round(calc_class_err(train_pred,train[,11]),3)
train_pred1<-predict(NBclassfier, newdata = train, type = "class")
train_tab1 = table(predicted = train_pred , actual = train$`Customer Type`)
library(caret)
train_con_mat1 = confusionMatrix(train_tab1)
c(train_con_mat1$overall["Accuracy"], 
  train_con_mat1$byClass["Sensitivity"], 
  train_con_mat1$byClass["Specificity"])

#MODEL FOR PROBLEM2

#Converting the variable categories
data4$Gender<-ifelse(data4$Gender=="Female",1,0)
data4$`Marital Status`<-ifelse(data4$`Marital Status`=="Married",1,0)
data4$`Customer Type`<-ifelse(data4$`Customer Type`=="Early Payment",1,0)
data4$Gender<-as.factor(data4$Gender)
data4$`Marital Status`<-as.factor(data4$`Marital Status`)
data4$`Customer Type`<-as.factor(data4$`Customer Type`)
data4$`Credit Type`<-as.factor(data4$`Credit Type`)
str(data4)
data4$LastPayment<-as.numeric(data4$LastPayment)
#Chi-square test
tablecredittype<-table(data4$`Customer Type`,data4$`Credit Type`)
tablecredittype
chisq.test(tablecredittype)
#since p value is less than 0.05 significant level, there is a relationship between variables.
#H0:There is no relationship between customer type and credit type
tablecreditamount<-table(data4$`Customer Type`,data4$`Credit Amount`)
tablecreditamount
chisq.test(tablecreditamount)
#since chi square approach is not suitable in here, we use fisher test.
fisher.test(tablecreditamount,simulate.p.value = TRUE)
#since p value is less than 0.05 significant level, there is a relationship between variables.
tableinstallment<-table(data4$`Customer Type`,data4$`Installment (Months)`)
chisq.test(tableinstallment)
fisher.test(tableinstallment,simulate.p.value = TRUE)
tablelatepayment<-table(data4$`Customer Type`,data4$`Late Payment (Days)`)
chisq.test(tablelatepayment)
fisher.test(tablelatepayment,simulate.p.value = TRUE)
tableage<-table(data4$`Customer Type`,data4$Age)
chisq.test(tableage)
fisher.test(tableage,simulate.p.value = TRUE)
tablegender<-table(data4$`Customer Type`,data4$Gender)
chisq.test(tablegender)
tablemaritalstatus<-table(data4$`Customer Type`,data4$`Marital Status`)
chisq.test(tablemaritalstatus)
tablenumberofdependents<-table(data4$`Customer Type`,data4$`# of Dependents`)
chisq.test(tablenumberofdependents)
fisher.test(tablenumberofdependents,simulate.p.value = TRUE)
tablelastpayment<-table(data1$`Customer Type`,data1$LastPayment)
chisq.test(tablelastpayment)
fisher.test(tablelastpayment,simulate.p.value = TRUE)

#MIN-MAX NORMALIZATION
data4$LastPayment<-as.numeric(data4$LastPayment)
data4$`Credit Amount` <-(data4$`Credit Amount`-min(data4$`Credit Amount`)) /
  (max(data4$`Credit Amount`)-min(data4$`Credit Amount`))
data4$`Installment (Months)` <-(data4$`Installment (Months)`-min(data4$`Installment (Months)`)) /
  (max(data4$`Installment (Months)`)-min(data4$`Installment (Months)`))
data4$`Late Payment (Days)` <-(data4$`Late Payment (Days)`-min(data4$`Late Payment (Days)`)) /
  (max(data4$`Late Payment (Days)`)-min(data4$`Late Payment (Days)`))
data4$Age <-(data4$Age-min(data4$Age)) /
  (max(data4$Age)-min(data4$Age))
data4$`# of Dependents` <-(data4$`# of Dependents`-min(data4$`# of Dependents`)) /
  (max(data4$`# of Dependents`)-min(data4$`# of Dependents`))
data4$LastPayment <-(data4$LastPayment-min(data4$LastPayment)) /
  (max(data4$LastPayment)-min(data4$LastPayment))

#Modelling for problem2

#splitting data into training and testing
a<-data4
training.samples <- a$`Customer Type` %>%createDataPartition(p = 0.8, list = FALSE) #createDataPartition helps you define train set index
train4 <- a[training.samples, ]
test4<- a[-training.samples, ]
dim(train4)
dim(data4)

#Logistic Regression

str(data4)
levels(data4$`Customer Type`)
table(train4$`Customer Type`)
train4$`Customer Type`<-as.factor(train4$`Customer Type`)
model_glm = glm(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`+LastPayment, data = train4, family = "binomial")
summary(model_glm)
model_glm1 = glm(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`, data = train4, family = "binomial")
summary(model_glm1)
#model_glm1 = glm(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+`# of Dependents`+LastPayment, data = train, family = "binomial")
#summary(model_glm1)

model_glm_pred = ifelse(predict(model_glm, type = "response") > 0.9, 1,0)

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
#prediction error yerine accuracy yaz
calc_class_err(actual = train4$`Customer Type`, predicted = model_glm_pred)
train_tab = table(predicted = model_glm_pred, actual = train4$`Customer Type`)
library(caret)
train_con_mat = confusionMatrix(train_tab, positive = "1")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])

model_glm_pred_test= ifelse(predict(model_glm, type = "response",newdata = test4) > 0.5, 1, 0)
test_tab = table(predicted = model_glm_pred_test, actual = test4$`Customer Type`)
library(caret)
test_con_mat = confusionMatrix(test_tab, positive = "1")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"])

library(pROC)
test_prob=predict(model_glm, type = "response",newdata = test4)
test_roc = roc(test4$`Customer Type` ~ test_prob, plot = TRUE, print.auc = TRUE)

####
table(data$`Last Installment Payment Before Regular/Late Payment`)
table(data$`Customer ID`)
data[which(data$`Late Payment (Days)`>550),]
a<-data[which(data$closemonth<0),]


#Random forest for problem 1

#RANDOM FOREST
library(rpart)
fit8 <- rpart(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`+LastPayment, data = train4, method = 'class') #method = class is necessary for classification task
print(fit8)
model_rf <- caret::train(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`+LastPayment,
                         data = train4,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))
final <- data.frame(actual = test4$`Customer Type`,
                    predict(model_rf, newdata = test4, type = "prob"))
final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
cm_original <- confusionMatrix(final$predict, test_data$classes)
head(train4)
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
## Train data performance.
str(train)
train_pred <-predict(fit8,train4[,c(2,3,4,5,7,8,9,10,12,13)],type = 'class')
round(calc_class_err(train_pred,train4[,11]),3)
train_pred<-as.data.frame(train_pred)
sum(is.na(train_pred))

sum(is.na(train))
# Accuracy Sensivity and Specificity
library(caret)
train_tab = table(predicted = train_pred , actual = train4[,11])
train_con_mat = confusionMatrix(train_tab)
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])

#Naive bayes for problem1

#NAIVE BAYES CLASSIFIER
library(e1071)
NBclassfier=naiveBayes(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`+LastPayment, data = train4)
print(NBclassfier)
#CE function
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
## Train data performance.
str(train4)
train_pred <-predict(NBclassfier,train4[,c(2,3,4,5,7,8,9,10,12,13)],type = 'class')
round(calc_class_err(train_pred,train4[,11]),3)

train_pred1<-predict(NBclassfier, newdata = train4, type = "class")

train_tab1 = table(predicted = train_pred , actual = train4$`Customer Type`)
library(caret)
train_con_mat1 = confusionMatrix(train_tab1)
c(train_con_mat1$overall["Accuracy"], 
  train_con_mat1$byClass["Sensitivity"], 
  train_con_mat1$byClass["Specificity"])

#XGBOOST

set.seed(123)
numeric4<-data4
str(numeric4)
numeric4<-numeric4[,-c(1,2,6,8,9)]
training.samples <- numeric4$`Customer Type` %>%
  createDataPartition(p = 0.8, list = FALSE) 
train.data.xgboost  <- numeric4[training.samples, ]
test.data.xgboost <- numeric4[-training.samples, ]


xgboost_model <- xgboost(data = as.matrix(train.data.xgboost[, -6]), 
                         label = as.numeric(train.data.xgboost$`Customer Type`)-1,
                         max_depth = 3, 
                         objective = "binary:logistic", 
                         nrounds = 10, 
                         verbose = FALSE,
                         prediction = TRUE)
xgboost_model
head(train.data.xgboost)

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
## Train data performance.

str(train.data.xgboost)
train_pred <-predict(xgboost_model,train.data.xgboost[,c(1,2,3,5,7,8)],type = 'class')
round(calc_class_err(train_pred,train.data.xgboost[,6]),3)
data<-predict(xgboost_model, as.matrix(test.data.xgboost[,-6]),type="response")
data<-as.factor(ifelse(data>0.5,1,0))
confusionMatrix(data,reference = as.factor(as.numeric(test.data.xgboost$`Customer Type`)-1))
#######
detach(data4)
b<-data4                
b$`Customer Type`
table(b$`Customer Type`)
b$`Customer Type`[which(b$`Customer Type`==0)]
which(b(0, b$`Customer Type`))
x<-which(b$`Customer Type` == 0, arr.ind=TRUE)
y<-sample(x,2500)
b<-b[-y,]
dim(b)
####
training.samples <- b$`Customer Type` %>%createDataPartition(p = 0.8, list = FALSE) #createDataPartition helps you define train set index
train4 <- b[training.samples, ]
test4<- b[-training.samples, ]
dim(train4)
dim(data4)

#Logistic Regression

str(data4)
levels(data4$`Customer Type`)
table(train4$`Customer Type`)
train4$`Customer Type`<-as.factor(train4$`Customer Type`)
model_glm = glm(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`+LastPayment, data = train4, family = "binomial")
summary(model_glm)
model_glm1 = glm(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`, data = train4, family = "binomial")
summary(model_glm1)
#model_glm1 = glm(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+`# of Dependents`+LastPayment, data = train, family = "binomial")
#summary(model_glm1)

model_glm_pred = ifelse(predict(model_glm, type = "response") > 0.9, 1,0)

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
calc_class_err(actual = train4$`Customer Type`, predicted = model_glm_pred)
train_tab = table(predicted = model_glm_pred, actual = train4$`Customer Type`)
library(caret)
train_con_mat = confusionMatrix(train_tab, positive = "1")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])
##########
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
orig_fit <- train(`Customer Type` ~ `Credit Type`+`Credit Amount`+`Installment (Months)`+Age+Gender+`Marital Status`+`# of Dependents`+LastPayment,
                  data = train4,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)