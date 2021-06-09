#######################################################################################
###############################   Hunter Douglas Data   ###############################
#######################################################################################



###############################################################################
############################   INITIALIZATION   ###############################
###############################################################################

## Libraries
library(ggplot2)



ls()

###############################################################################
## TO RESET DATA, RUN BELOW CODE UNTIL NEXT HEADER
###############################################################################


### Start Here

###############################################################################
cat(paste("","","","","","",'      INITIALIZING ....................',"","","","","","","","","","","",'      Uploading Data .................... Step 1/4',"","","","","", sep="\n"))
library(data.table)
library(plyr)
install.packages("gbm")
library(gbm)

# Data
data <- read.csv('F:\\Masters\\CU Boulder\\Sem 3\\BUAN Experimental Project\\Hunter Douglas\\Dataset\\Hunter Douglas Quality Data.csv',stringsAsFactors=FALSE)
dim(data)   #3159252      25

########## Data Trim for HoneyComb at Factory B ##########
###############################################################################
cat(paste("","","","","",'      Data Trim for HoneyComb at Factory B .................... Step 2/4',"","","","","", sep="\n"))
## Clean Product Category ##
data$PRODUCT_CATEGORY <- substr(data$PRODUCT_CATEGORY,4,999999)
## Cut Sample ##
data <- data[data$ORIGINAL_PLANT=="B" & data$PRODUCT_CATEGORY=='Honeycomb Shades',]
head(data)

########## General Data Cleaning ##########
###############################################################################
cat(paste("","","","","",'      Data Cleaning .................... Step 3/4',"","","","","", sep="\n"))
## Force Integer Columns ##
for (i in c(4,8,10,11,12,13)){
	if(class(data[,i])=="character"){
		data[,i] <- as.integer(data[,i])
	}
}

## Change Date to Correct Format ##
data[,25] <- as.Date(paste(substr(data[,25],1,4),substr(data[,25],5,6),substr(data[,25],7,8)),format = "%Y %m %d")

## Remove Leading Zeros in Color Code ##
data$COLOR_ID <- gsub("(?<![0-9])0+","",data$COLOR_ID,perl = TRUE)

## Remove Leading Zeros in Fabric Code ##
data$FABRIC_ID <- gsub("(?<![0-9])0+","",data$FABRIC_ID,perl = TRUE)

## Change Column Names ##
setnames(data,old=c('ORIGINAL_ORDER','ORIGINAL_ORDER_LINE','SALES_ORDER','SALES_ORDER_LINE','PRODUCT_CATEGORY','ORIGINAL_PLANT','ALLIANCE_LEVEL_ID','REGIONAL_SALES_MGR_ID','REGION_STATE_ID','NET_SALES_UNITS','REMAKE_UNITS','WIDTH','HEIGHT','FABRIC_ID','COLOR_ID','SLAT_SIZE','ORDER_REASON_ID','ORIGINAL_MATERIAL_ID','OPERATING_SYSTEM_ID','OPERATING_SYS_OPT_ID','RESPONSIBILITY_CODE_ID','REASON_CODE_ID','REASON_CODE','SOLD_TO_ID','SO_CREATED_DATE'
),new=c("originalOrder",'originOrderLine','salesOrder','salesOrderLine','productLine','plant','allianceID','salesManagerID','orderState','salesUnits','remakeUnits','width','height','fabricID','colorID','slatSize','orderReasonID','materialID','operatingSysID','operatingSysOptID','responcibilityID','reasonCodeID','reasonCode','customerID','date'))

## Get rid of parts orders ##
data <- data[data$orderReasonID != "PAR",]
data <- data[data$orderReasonID != "CRR",]


############ Order Numbers and Find Returned Orders ###########
###############################################################################
cat(paste("","","","","",'      Finding Returned Orders .................... Step 4/4',"","","","","", sep="\n"))

## Remove Null Original Orders ##
data <- data[data[,1] != "NULL",]

## combine order numbers ##
# Add leading zeros
data[,2] <- sprintf("%06d",data[,2])
data[,4] <- sprintf("%06d",data[,4])

# concat order number and line number
data$originalOrder <- paste(data[,1],data[,2],sep="")
data$salesOrder <- paste(data[,3],data[,4],sep="")

# Flag Returns #
data$flagReturn <- ((data$originalOrder != data$salesOrder) & (substr(data$originalOrder,17,18) != "00"))
head(data$flagReturn, 20)
head(data,20)
## Get rid of unnessisary columns ##
data$productLine <- NULL
data$plant <- NULL
data$originOrderLine <- NULL
data$salesOrderLine <- NULL
data$slatSize <- NULL
head(data)

# Get rid of orders over 90 days #
origReturn <- sum(data$flagReturn)
dataCOR <- unique(merge(data,data,by.x="originalOrder",by.y="salesOrder"))
dataCOR$dateDiff <- rep(0,length.out=nrow(dataCOR))
dataCOR$dateDiff <- 
dataCOR$date.x - dataCOR$date.y
dataCOR$notReturn <- dataCOR$dateDiff >= 90

# Create Unique ID for each row #
dataCOR$ID_orig <- paste(dataCOR$originalOrder.y,dataCOR$originalOrder,sep="")
dataCOR$ID <- paste(dataCOR$originalOrder,dataCOR$salesOrder,sep="")
data$ID <- paste(data$originalOrder,data$salesOrder,sep="")

# Flag Returned Original Orders #
flagReturnCOR <- dataCOR$flagReturn.x
flagReturnCOR[dataCOR$notReturn==TRUE] <- FALSE
dataCOR$trueReturn <- flagReturnCOR==TRUE

# Create Return Flag of Original Orders for main dataset #
flagReturnOrig <- (data$ID %in% dataCOR$ID_orig[dataCOR$trueReturn==TRUE])
data$flagReturnOrig <- flagReturnOrig

# Create Return Flag of Returned Orders for main dataset #
flagReturnOrder <- (data$ID %in% dataCOR$ID[dataCOR$trueReturn==TRUE])
data$flagReturnOrder <- flagReturnOrder

# Flag Orders with No Returns #
flagNotReturn <- ((data$flagReturnOrig != TRUE) & (data$flagReturnOrder != TRUE))
data$notReturn <- flagNotReturn

# Clean Duplictes #
data$flagReturnOrig[(data$flagReturnOrig == TRUE) & (data$flagReturnOrder == TRUE)] <- FALSE

# Remove flagReturn Column #
data$flagReturn <- NULL



###############################################################################
cat(paste("","","","","","","","","","","","","","","","","",'      Initialization Finished',"","","","","","", sep="\n"))




## End Here



###############################################################################
###############################   Further Data Cleaning   #####################
###############################################################################


## Get rid of other unnessisary columns for regression analysis ##
data$operatingSysID <- NULL
data$operatingSysOptID <- NULL
data$responcibilityID <- NULL
data$reasonCodeID <- NULL
data$reasonCode <- NULL
head(data)




########## Checks ##########


unique(data$productLine)
head(data,5)
class(data[,5])



###############################################################################
############################### Linear Modeling ###############################
###############################################################################




# subset for NN modeling #
data1 <- data[data$flagReturnOrig==TRUE | data$notReturn==TRUE,]
dataR <-data[data$flagReturnOrder==TRUE | data$notReturn==TRUE,]

nrow(data1)

summary(lm(flagReturnOrig ~ width,data=data1,family="binomial"))

summary(lm(fabricReturnPer[,2] ~ fabricReturnPer[,5]))

unique(as.factor(data1$fabricID))


###############################################################################
################################ Neural Network ##############################
###############################################################################

data1 <- data[data$flagReturnOrig==TRUE | data$notReturn==TRUE,]
head(data1)
nrow(data1) #730100

install.packages("neuralnet")
library(neuralnet)

colSums(is.na(data1))
data1 <- data1[!is.na(data1$height),]
data1 <- data1[!is.na(data1$width),]


colSums(is.na(data1))
model=neuralnet(formula = flagReturnOrig ~ height + width,
                data = data1,
                hidden=5,
                threshold=0.01)


##After splitting the data, now running the neural network model.

model=neuralnet(formula = flagReturnOrig ~ height + width,
                data = data1,
                hidden=5,
                threshold=0.01)

model=neuralnet(formula = flagReturnOrig ~ height + width, data = data1, hidden=5, linear.output = F, threshold=0.01)
model
plot(model)

################################ End ##############################

###############################################################################
################################ Neural Network 2##############################
###############################################################################

#Splitting Data into training and test set by 60:40 ratio with the intention of using 70 percent
#of the data at our disposal to train the network and the remaining 30 percent to test the network.

install.packages("caTools")
library(caTools)
split <- sample.split(data1, SplitRatio = 0.60)
train <- subset( data1, split == TRUE) #Training set
test <- subset(data1, split == FALSE)  #Test set

model4 <- neuralnet(formula = flagReturnOrig ~ height + width,
                    data = train,
                    hidden=10,
                    linear.output = F,
                    threshold=0.01 )
model4
plot(model4)

# Make predictions using the neural network results. 
cancel.pred <- predict(model4, data)

#Now we have the network we can use it ot make predictions.
#We have set 30 percent of the available data to do this.

predict_neuralnet <- compute(model4,test[,1:13]) #Taking columns 1 to 13 representing the input variables of the network.




predict_neuralnet_start <- predict_neuralnet$net.result*(max(data$medv)-min(data$medv))+min (data$test_start <- as.data.frame((test_data$medv)*(max(data$medv)-min(data$medv))+min(data$medv))
                                                                                          MSE.net_data <- sum((test_start - predict_net_test_start)^2)/nrow(test_start)


################################ End ##############################


dataR <- data[data$flagReturnOrder==TRUE | data$notReturn==TRUE,]




# subset for linear modeling #
data1 <- data[data$flagReturnOrig==TRUE | data$notReturn==TRUE,]
dataR <-data[data$flagReturnOrder==TRUE | data$notReturn==TRUE,]




######### Fabrics ##########
fabricOrders <- aggregate(data1$originalOrder ~ as.factor(data1$fabricID),FUN=length)
fabricReturns <- aggregate(data1$flagReturnOrig ~ as.factor(data1$fabricID),FUN=sum)
fabricReturnPer <- data.frame("Fabric Type" = fabricOrders[,1]
	,"Return Count" = fabricReturns[,2]
	,"Order Count" = fabricOrders[,2]
	,"Return Percentage" = round(fabricReturns[,2]/fabricOrders[,2]*100,2))
nrow(fabricReturns)
str(data1$fabricID)
str(fabricReturns[,1])
head(fabricReturnPer,30)
nrow(fabricReturns)

## Fabric Type Summary
# Order by Order Count
fabricReturnPer[order(fabricReturnPer[,3],decreasing=TRUE),]
# Order by Return Percentage
fabricReturnPer[order(fabricReturnPer[,2],decreasing=TRUE),]

## Bin Fabric Types

flagnone <- data1$fabricID %in% fabricReturnPer[fabricReturnPer[,4] == 0]
flagunder1 <- data1$fabricID %in% fabricReturnPer[fabricReturnPer[,4] <= 1.5 & fabricReturnPer[,4] !=0,1]
flag1to2 <- data1$fabricID %in% fabricReturnPer[fabricReturnPer[,4] > 1.5 & fabricReturnPer[,4] <= 1.75,1]
flag2to3 <- data1$fabricID %in% fabricReturnPer[fabricReturnPer[,4] > 1.75 & fabricReturnPer[,4] <= 2.5,1]
flag3to4 <- data1$fabricID %in% fabricReturnPer[fabricReturnPer[,4] > 2.5 & fabricReturnPer[,4] <= 3.5,1]
flagover4 <- data1$fabricID %in% fabricReturnPer[fabricReturnPer[,4] > 3.5,1]

data1$fabricBin <- rep(0,length.out=nrow(data1))
data1$fabricBin[flagnone==TRUE] <- "None"
data1$fabricBin[flagunder1==TRUE] <- "Low"
data1$fabricBin[flag1to2 ==TRUE] <- "Low-Mid"
data1$fabricBin[flag2to3 ==TRUE] <- "Mid"
data1$fabricBin[flag3to4 ==TRUE] <- "Mid-High"
data1$fabricBin[flagover4 ==TRUE] <- "High"

# Drop zero return fabric types #
data1 <- data1[data1$fabricBin != "None",]
data1$fabricBin <- as.factor(data1$fabricBin)

######### Colors ##########
colorOrders <- aggregate(data1$originalOrder ~ data1$colorID,FUN=length)
colorReturns <- aggregate(data1$flagReturnOrig ~ data1$colorID,FUN=sum)
colorReturnPer <- data.frame("Color Type" = colorOrders[,1]
	,"Return Count" = colorReturns[,2]
	,"Order Count" = colorOrders[,2]
	,"Return Percentage" = round(colorReturns[,2]/colorOrders[,2]*100,2))

## Color Type Summary
# Order by Order Count
colorReturnPer[order(colorReturnPer[,3],decreasing=TRUE),]
# Order by Return Percentage
colorReturnPer[order(colorReturnPer[,2],decreasing=TRUE),]

## Bin Color Types

flagnone <- data1$colorID %in% colorReturnPer[colorReturnPer[,4] == 0,1]
flagunder1 <- data1$colorID %in% colorReturnPer[colorReturnPer[,4] <= 1.5 & colorReturnPer[,4] !=0,1]
flag1to2 <- data1$colorID %in% colorReturnPer[colorReturnPer[,4] > 1.5 & colorReturnPer[,4] <= 2,1]
flag2to3 <- data1$colorID %in% colorReturnPer[colorReturnPer[,4] > 1.75 & colorReturnPer[,4] <= 2.5,1]
flag3to4 <- data1$colorID %in% colorReturnPer[colorReturnPer[,4] > 2.5 & colorReturnPer[,4] <= 3.5,1]
flagover4 <- data1$colorID %in% colorReturnPer[colorReturnPer[,4] > 3.5,1]

data1$colorBin <- rep(0,length.out=nrow(data1))
data1$colorBin[flagnone==TRUE] <- "None"
data1$colorBin[flagunder1==TRUE] <- "Low"
data1$colorBin[flag1to2 ==TRUE] <- "Low-Mid"
data1$colorBin[flag2to3 ==TRUE] <- "Mid"
data1$colorBin[flag3to4 ==TRUE] <- "Mid-High"
data1$colorBin[flagover4 ==TRUE] <- "High"

sum(data1$colorBin=="None")
# Drop zero return color types #
data1 <- data1[data1$colorBin != "None",]
data1$colorBin <- as.factor(data1$colorBin)

head(data1)

############## other binning method
data1$fabricIDD <- data1$fabricID
fabs <- fabricReturnPer[fabricReturnPer[,3] < 2000,1]
fabs
levels(data1$fabricIDD) <- c(levels(data1$fabricIDD),"Other")
data1$fabricIDD[factor(data1$fabricID) %in% fabs] <- as.factor("Other")
data1$fabricIDD <- factor(data1$fabricIDD)


# fabricReturnPer[order(fabricReturnPer[,3],decreasing=TRUE),]
# data1$fabricIDD <- NULL

# summary
fabricOrders1 <- aggregate(data1$originalOrder ~ data1$fabricIDD,FUN=length)
fabricReturns1 <- aggregate(data1$flagReturnOrig ~ data1$fabricIDD,FUN=sum)
fabricReturnPer1 <- data.frame("Fabric Type" = fabricOrders1[,1]
	,"Return Count" = fabricReturns1[,2]
	,"Order Count" = fabricOrders1[,2]
	,"Return Percentage" = round(fabricReturns1[,2]/fabricOrders1[,2]*100,2))
fabricReturnPer1  <- fabricReturnPer1[order(fabricReturnPer1[,3],decreasing=TRUE),]
fabricReturnPer1




###############################################################################
###############################  Models  ######################################
###############################################################################



mod1 <- lm(data1$flagReturnOrig ~ flaglow + flagmid + flaghigh + data1$operatingSysID)
summary(mod1)
mod2 <- glm(flagReturnOrig ~ fabricBin-1 + width + height + colorBin-1, data=data1,family=binomial(link="logit"))
summary(mod2)
par(mfrow=c(2,2))
plot(mod2)

mod3 <- lm(flagReturnOrig ~ orderState-1, data=data1)
summary(mod3)
par(mfrow=c(2,2))
plot(mod3)










head(data1)
################# Smaller Subset #################
# subset for easier regressions
subset <- data1[sample(nrow(data1),nrow(data1)*.05),]
nrow(subset)

# regression
sub.lin <- glm(flagReturnOrig ~ fabricBin-1 + width + height + colorBin-1, data=subset,family=binomial(link="logit"))
summary(sub.lin)

# plots
par(mfrow=c(2,2))
plot(sub.lin)

# Residuals #
#standardized residuals
s.res <- unlist(rstandard(sub.lin))
s.res

summary(s.res)
sum(s.res > 1.5)

###############
tester <- lm(as.numeric(flagReturnOrig) ~ fabricBin,data=subset)
summary(tester)
# plots
par(mfrow=c(2,2))
plot(tester)

flagHPFFab <- subset$fabricID=="HPF"
tester <- lm(as.numeric(flagReturnOrig) ~ flagHPFFab,data=subset)
summary(tester)
# plots
par(mfrow=c(2,2))
plot(tester)






###############
# start here for sample and prediction #
notReturn <- data1[data1$notReturn==TRUE & !is.na(data1$width) & !is.na(data1$height) & !is.na(data1$flagReturnOrig),]
subset2 <- notReturn[sample(nrow(notReturn),sum(data1$flagReturnOrig==TRUE & !is.na(data1$width) & !is.na(data1$height) & !is.na(data1$flagReturnOrig))),]

testdata <- rbind(subset2,data1[data1$flagReturnOrig==TRUE & !is.na(data1$width) & !is.na(data1$height) & !is.na(data1$flagReturnOrig),])
nrow(testdata)/2
sum(testdata$flagReturnOrig)


sube.lin <- glm(flagReturnOrig ~ fabricBin-1 + width + height + colorBin-1, data=testdata,family=binomial(link="logit"))
summary(sube.lin)

# plots
par(mfrow=c(2,2))
plot(sube.lin)

sube.lin.fitted <- sube.lin$fitted.values
sum(is.na(testdata$height))
summary(sube.lin.fitted)
# hist(sube.lin.fitted)
predict <- ifelse(sube.lin.fitted >= .7,"TRUE","FALSE")
cross <- table(testdata$flagReturnOrig, unlist(predict))
cross
length(testdata$flagReturnOrig)
length(predict)

# accuracy
sum(diag(cross))/sum(cross)

## done
#################


# residuals
hist(unlist(rstandard(sube.lin)))

##################################################

unique(data1$materialID)
summary(glm(flagReturnOrig ~ materialID,data=data1,family=binomial))


acf(tester)

hist()

test <- glm(flagReturnOrig ~ width + height, data=subset,family=binomial)
par(mfrow=c(2,2))
plot(test)


flagHighFab <- data1$fabricBin=="High"
lin.hf <- lm(data1$flagReturnOrig ~ flagHighFab,family=binomial)
summary(lin.hf)
par(mfrow=c(2,2))
plot(lin.hf)

###############################################################################
###############################  Returned Orders Modeling  ####################
###############################################################################


modr.1 <- lm(flagReturnOrder ~ height,data=dataR)
summary(modr.1)
# plots
par(mfrow=c(2,2))
plot(modr.1)


unique(data1$salesManagerID)



###############

set.seed(200)

train01 <- sample(nrow(data1), nrow(data1)*0.6)
data1.train01 <- data1[train01, ]
data1.test01 <- data1[-train01, ]
testmodel01 <- gbm(flagReturnOrig ~ fabricBin-1 + width + height + colorBin-1 + factor(salesManagerID), data=data1.train01,distribution="bernoulli",n.trees=1000)
return.prob01 <- predict(testmodel01, newdata = data1.test01, type = "response",n.trees=1000,single.tree=FALSE)
return.pred01 <- as.numeric(return.prob01 > 0.027)
summary(return.prob01)
# table
table(data1.test01$flagReturnOrig,return.pred01)
confusion01 <- table(data1.test01$flagReturnOrig, return.pred01)

# accuracy
paste("Accuracy is ",round(sum(diag(confusion01))/sum(confusion01)*100,2),"%",sep="")

# hist(return.prob01)
# par(mfrow=c(2,2))
# plot(testmodel01)
###############
testmodel01 <- gbm(flagReturnOrig ~ fabricBin-1 + width + height + colorBin-1 + factor(salesManagerID), data=data1.train,distribution="bernoulli",n.trees=100)
summary(testmodel01)



?gbm






# use cv function
###############################################################################
###############################  Confusion Matrix  ############################
###############################################################################
?sample
head(data1)
head(data1.train01)

###############

set.seed(200)

train01 <- sample(nrow(data1), nrow(data1)*0.6)
data1.train01 <- data1[train01, ]
data1.test01 <- data1[-train01, ]
testmodel01 <- glm(flagReturnOrig ~ fabricBin-1 + width + height + colorBin-1 + factor(salesManagerID), data=data1.train01,family=binomial(link="logit"))
return.prob01 <- predict(testmodel01, newdata = data1.test01, type = "response")
return.pred01 <- as.numeric(return.prob01 > 0.5)
summary(return.prob01)
# table
table(data1.test01$flagReturnOrig,return.pred01)
confusion01 <- table(data1.test01$flagReturnOrig, return.pred01)

# accuracy
paste("Accuracy is ",round(sum(diag(confusion01))/sum(confusion01)*100,2),"%",sep="")

# hist(return.prob01)
# par(mfrow=c(2,2))
# plot(testmodel01)
###############

###############

set.seed(200)

train02 <- sample(nrow(data1), nrow(data1)*0.6)
data1.train02 <- data1[train02, ]
data1.test02 <- data1[-train02, ]
testmodel02 <- glm(flagReturnOrig ~ fabricBin-1 + width + height + colorBin-1 + factor(salesManagerID), data=data1.train02,family=binomial(link="logit"))
summary(testmodel02)
return.prob02 <- predict(testmodel02, newdata = data1.test02, type = "response")
return.pred02 <- as.numeric(return.prob02 > 0.027)
summary(return.prob02)
# table
table(data1.test02$flagReturnOrig,return.pred02)
confusion02 <- table(data1.test02$flagReturnOrig, return.pred02)

# accuracy
paste("Accuracy is ",round(sum(diag(confusion02))/sum(confusion02)*100,2),"%",sep="")

# hist(return.prob02)
# par(mfrow=c(2,2))
# plot(testmodel02)
###############
sum(return.prob01,na.rm)


predict(testmodel02)


length(data1.train$flagReturnOrig)


###### cross validate on whole dataset








###############################################################################
###############################  Return Summary Generator  ####################
###############################################################################

head(data1)

Input <- data1$fabricID
Summary <- NULL
table <- NULL

## Run Below Code for Summary Table ##
################################

Returns <- aggregate(data1$flagReturnOrig ~ Input,FUN=sum)
OrderCount <- aggregate(data1$flagReturnOrig ~ Input,FUN=length)
Summary <- data.frame("Value" = Returns[,1]
	,"ReturnCount" = Returns[,2]
	,"OrderCount" = OrderCount[,2]
	,"ReturnPercentage" = round((Returns[,2]/OrderCount[,2])*100,2))

Summary <- Summary[order(Summary[,3],decreasing=TRUE),]

table <- rbind(data.frame("Value" = "TOTAL"
	,"ReturnCount" = sum(Summary[,2])
	,"OrderCount" = sum(Summary[,3])
	,"ReturnPercentage" = round((sum(Summary[,2])/sum(Summary[,3]))*100,2))
	,Summary)
table <- table[order(table[,3],decreasing=TRUE),]
head(table,50)

################################




















