# read the training and test datasets
train <- read.csv("C://Analytics Documents//Data Science with R//Kaggle//shelter-animal-outcomes//train.csv",na.strings = "true")
test <- read.csv("C://Analytics Documents//Data Science with R//Kaggle//shelter-animal-outcomes//test.csv",na.strings = "true")

#check the data quality
str(train)
str(test)

dim(train)
dim(test)

colSums(is.na(train))
colSums(is.na(test))

summary(train$OutcomeType)
table(train$OutcomeType,train$AnimalType)

library(dplyr)
library(ggplot2)


p <- ggplot(train,aes(x=train$OutcomeType,fill=train$SexuponOutcome))
p + geom_bar()

# Data Transformation

library(lubridate)

train$DateTime <- ymd_hms(train$DateTime)

train <- train%>% mutate(Yr = year(train$DateTime))
train <- train %>% mutate(Mth = months.POSIXt(train$DateTime))

test$DateTime <- ymd_hms(test$DateTime)

test <- test%>% mutate(Yr = year(test$DateTime))
test <- test %>% mutate(Mth = months.POSIXt(test$DateTime))


q <- ggplot(train ,aes(x=train$Mth,fill=train$OutcomeType))
q + geom_bar()

#converting AgeUponOutcome Variable into weeks


library(stringr)
dat1 <-train %>% filter(str_detect(AgeuponOutcome,"month*+")) %>% mutate(Age_Extract=unlist(strsplit(AgeuponOutcome,split = "months")))
dat1$Age_Extract <- gsub("month","",dat1$Age_Extract)
dat1$Age_Extract <- as.numeric(dat1$Age_Extract)
dat1$Age_Extract <- dat1$Age_Extract * 4

dat_1 <- test %>% filter(str_detect(AgeuponOutcome,"month*+")) %>% mutate(Age_Extract=unlist(strsplit(AgeuponOutcome,split = "months")))
dat_1$Age_Extract <- gsub("month","",dat_1$Age_Extract)
dat_1$Age_Extract <- as.numeric(dat_1$Age_Extract)
dat_1$Age_Extract <- dat_1$Age_Extract * 4

dat2 <-train %>% filter(str_detect(AgeuponOutcome,"year*+")) %>% mutate(Age_Extract=unlist(strsplit(AgeuponOutcome,split = "years")))
dat2$Age_Extract <- gsub("year","",dat2$Age_Extract)
dat2$Age_Extract <- as.numeric(dat2$Age_Extract)
dat2$Age_Extract <- dat2$Age_Extract * 52

dat_2 <-test %>% filter(str_detect(AgeuponOutcome,"year*+")) %>% mutate(Age_Extract=unlist(strsplit(AgeuponOutcome,split = "years")))
dat_2$Age_Extract <- gsub("year","",dat_2$Age_Extract)
dat_2$Age_Extract <- as.numeric(dat_2$Age_Extract)
dat_2$Age_Extract <- dat_2$Age_Extract * 52

dat3 <-train %>% filter(str_detect(AgeuponOutcome,"week*+")) %>% mutate(Age_Extract=unlist(strsplit(AgeuponOutcome,split = "weeks")))
dat3$Age_Extract <- gsub("week","",dat3$Age_Extract)
dat3$Age_Extract <- as.numeric(dat3$Age_Extract)

dat_3 <-test %>% filter(str_detect(AgeuponOutcome,"week*+")) %>% mutate(Age_Extract=unlist(strsplit(AgeuponOutcome,split = "weeks")))
dat_3$Age_Extract <- gsub("week","",dat_3$Age_Extract)
dat_3$Age_Extract <- as.numeric(dat_3$Age_Extract)

dat4 <- train %>% filter(str_detect(AgeuponOutcome,"day*+")) %>% mutate(Age_Extract=unlist(strsplit(AgeuponOutcome,split = "days")))
dat4$Age_Extract <- gsub("day","",dat4$Age_Extract)
dat4$Age_Extract <- as.numeric(dat4$Age_Extract)
dat4$Age_Extract <- dat4$Age_Extract /7
dat4$Age_Extract <- round(dat4$Age_Extract,1)

dat_4 <- test %>% filter(str_detect(AgeuponOutcome,"day*+")) %>% mutate(Age_Extract=unlist(strsplit(AgeuponOutcome,split = "days")))
dat_4$Age_Extract <- gsub("day","",dat_4$Age_Extract)
dat_4$Age_Extract <- as.numeric(dat_4$Age_Extract)
dat_4$Age_Extract <- dat_4$Age_Extract /7
dat_4$Age_Extract <- round(dat_4$Age_Extract,1)

dat1 <- dat1 %>% select(-Age_Extract_Weeks)

train1 <- rbind(dat1,dat2,dat3,dat4)
test1 <- rbind(dat_1,dat_2,dat_3,dat_4)

diff <- anti_join(train,train1)
diff1 <- anti_join(test,test1)



diff$Age_Extract <- "NA"
diff1$Age_Extract <-"NA"
train1 <- rbind(train1,diff)
test1 <- rbind(test1,diff1)

train1$Age_Extract <- as.numeric(train1$Age_Extract)
test1$Age_Extract <- as.numeric(test1$Age_Extract)

p <- ggplot(train1,aes(x=train1$OutcomeType,y=train1$Age_Extract))
p + geom_boxplot()

#Imputing Missing Values in Age derived variable

table1 <- table(train1$Age_Extract,train1$OutcomeType)
Transfer_Age <-  table1[,5]/rowSums(table1)
miss <- which(is.na(train1$Age_Extract))

table2 <- table(test1$Age_Extract,test1$SexuponOutcome)
Unknown_Sex <- table2[,5]/rowSums(table2)
miss1 <- which(is.na(test1$Age_Extract))

table(train1$OutcomeType[miss])/length
table(test1$SexuponOutcome[miss1])/length(miss1)

train1$Age_Extract[miss] <- 0.1
test1$Age_Extract[miss1] <- 3

# Data Binning to reduce variable levels

train1$Adoption_dummy <- ifelse(train1$OutcomeType=="Adoption",1,0)
test1$Adoption_dummy <- ifelse(test1$OutcomeType=="Adoption",1,0)

dec1 <-train1 %>% count(Adoption_dummy,levels=Breed)%>% filter(Adoption_dummy==1)
dec1$N <- unclass(train1 %>% filter(Breed %in% dec1$levels) %>% count(Adoption_dummy))[[2]]
dec1$percent <- dec1$n/dec1$N



temp1 <- data.frame(unclass(table(train1$Breed,train1$AnimalType)))

# Breed Optimization
train1$Breed_PureMix <- ifelse(grepl("Mix",train1$Breed),1,0)
test1$Breed_PureMix <-  ifelse(grepl("Mix",test1$Breed),1,0)

#Ignore
train1$Breed_New <-sub("([A-Za-z]+).*", "\\1", train1$Breed)
train1$Breed_New <- word(train1$Breed,2,-1)
train1$Breed_New1 <- strsplit(train1$Breed,"/")  #Ignore

train1$Breed_New2<- str_split_fixed(train1$Breed,"/",3)[,1]
train1$Breed_New2 <- gsub("mix","",train1$Breed_New2)
train1$Breed_New2 <- str_trim(train1$Breed_New2,side="both")

test1$Breed_New2<- str_split_fixed(test1$Breed,"/",3)[,1]
test1$Breed_New2 <- gsub("Mix","",test1$Breed_New2)
test1$Breed_New2 <- str_trim(test1$Breed_New2,side="both")

count_breed <- train1 %>% group_by(Breed_New2,AnimalType) %>% summarize(N=n())

p <- ggplot(train1,aes(x=train1$Breed_New2))
p + geom_bar(aes(fill=train1$OutcomeType))

 
#Run Multinomail Logistic Regression model
library(nnet)
train1$OutcomeType <- relevel(train1$OutcomeType,ref="Adoption")
mod1 <- multinom(OutcomeType~AnimalType+SexuponOutcome+Breed+Color+Yr+Mth+Age_Extract,data = train1)
mod2 <-multinom(OutcomeType~AnimalType+SexuponOutcome+Yr+Mth+Age_Extract+Breed_PureMix,data = train1)

summary(mod2)

#Calculate Z-score and p-value to determine predictors significance

z <- summary(mod2)$coefficients/summary(mod2)$standard.errors

# 2-tailed Z test
p <- (1- pnorm(abs(z),0,1)) *2

#Extract Coefficients and Exponentiate for relative risk ratio
exp(coef(mod2))

model_fitted <- fitted(mod2)
head(model_fitted)

# Test the model on test Data

pred<-predict(mod2,type="probs",newdata=test)
head(pred)



#Deriving majority variable from fitted

df2 <- colnames(model_fitted)[max.col(model_fitted,ties.method="first")] 
df3 <- colnames(pred)[max.col(pred,ties.method="first")]

colnames(df3) <- "OutcomeType"

test1 <- cbind(test1,df3)

test1$Adoption <- ifelse(test1$OutcomeType=="Adoption",1,0)
test1$Died <- ifelse(test1$OutcomeType=="Died",1,0)
test1$Euthanasia <- ifelse(test1$OutcomeType=="Euthanasia",1,0)
test1$Return_to_owner <- ifelse(test1$OutcomeType=="Return_to_owner",1,0)
test1$Transfer <- ifelse(test1$OutcomeType=="Transfer",1,0)

# Writing final_Outcome Data

final_data <- select(test1,ID,Adoption,Died,Euthanasia,Return_to_owner,Transfer)
final_data <- final_data[order(final_data$ID),]

write.xlsx(final_data,file="AnimalShelterOutcome.xlsx")
write.csv(final_data,file="AnimalShelterOutcome1.csv")
