##install.packages("data.table")
##install.packages("reshape2")
##setwd("C:/Users/SIAVHSA/Documents/R/Datos/")
##path <- getwd()
library(data.table)
library(reshape2)
fileUrl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
f <- 'Dataset.zip'
download.file(fileUrl,f)
d <- 'UCI HAR Dataset'
unzip(f)

##Read Subjects
dtsubject_train <- data.table(read.table(file.path(path, d, 'train', 'subject_train.txt')))
dtsubject_test <- data.table(read.table(file.path(path, d, 'test', 'subject_test.txt')))
dtsubject <- rbind(dtsubject_train, dtsubject_test)
names(dtsubject) <- c('Subject')
remove(dtsubject_train,dtsubject_test)

##Read activities
dtY_train <- data.table(read.table(file.path(path, d, 'train','Y_train.txt')))
dtY_test <- data.table(read.table(file.path(path,d,'test','Y_test.txt')))
dtAct <- rbind(dtY_train,dtY_test)
names(dtAct) <- c('Activity')
remove(dtY_train,dtY_test)

##Combine subject and activity
dtsubject <- cbind(dtsubject,dtAct)
remove(dtAct)

##Read feature data
dtX_train <- data.table(read.table(file.path(path,d,'train','X_train.txt')))
dtX_test <- data.table(read.table(file.path(path,d,'test','X_test.txt')))
dt <- rbind(dtX_train,dtX_test)
remove(dtX_train,dtX_test)

#merge into one table subject/activity/feature
dt <- cbind(dtsubject,dt)

##set key to subject/activity
setkey(dt,Subject,Activity)
remove(dtsubject)

#read feature names, get only std and mean features
dtfeatures <- data.table(read.table(file.path(path,d,'features.txt'))) 
names(dtfeatures) <- c('ftNum','ftName')
dtfeatures <- dtfeatures[grepl("mean\\(\\)|std\\(\\)",ftName)]
dtfeatures$ftCode <- paste('V', dtfeatures$ftNum, sep = "")

#select only the filtered features (with=FALSE to dynamically pick cols)
dt <- dt[,c(key(dt), dtfeatures$ftCode),with=F]

##read activity names
dtActNames <- data.table(read.table(file.path(path, d, 'activity_labels.txt')))
names(dtActNames) <- c('Activity','ActivityName')
dt <- merge(dt,dtActNames,by='Activity')
remove(dtActNames)

##add activityname as a key
setkey(dt,Subject,Activity,ActivityName)

##reshape data
dt <- melt(dt, key(dt), variable.name='ftCode',value.name='ftValue')
dt$Activity <- NULL

##merge in ftName
dt <- merge(dt,dtfeatures,by='ftCode')
setkey(dt,Subject,ActivityName,ftName)

dtTidy <- dt[,.(ftAvg=mean(ftValue)),by=key(dt)]

##start seperating out featName column to seperate columns
#ftDomain: TIME FREQ
dtTidy$ftDomain[grepl('^t',dtTidy$ftName)] <- 'Time'
dtTidy$ftDomain[grepl('^f',dtTidy$ftName)] <- 'Freq'

##ftInstrment:  Accelerometer Gyroscope
dtTidy$ftInstrment[grepl('Acc',dtTidy$ftName)] <- 'Accelerometer'
dtTidy$ftInstrment[grepl('Gyro',dtTidy$ftName)] <- 'Gyroscope'

##ftAcceleration:  Body Gravity
dtTidy$ftAcceleration[grepl('BodyAcc',dtTidy$ftName)] <- 'Body'
dtTidy$ftAcceleration[grepl('GravityAcc',dtTidy$ftName)] <- 'Gravity'

##ftStatVariable:  mean std
dtTidy$ftStatVariable[grepl('mean()',dtTidy$ftName)] <- 'mean'
dtTidy$ftStatVariable[grepl('std()',dtTidy$ftName)] <- 'std'

##ftJerk: Y
dtTidy$ftJerk[grepl('Jerk', dtTidy$ftName)] <- 'Y'

##ftMagnitude: Y
dtTidy$ftMagnitude[grepl('Mag', dtTidy$ftName)] <- 'Y'

##ftAxis:  X Y Z
dtTidy$ftAxis[grepl('-X', dtTidy$ftName)] <- 'X'
dtTidy$ftAxis[grepl('-Y', dtTidy$ftName)] <- 'Y'
dtTidy$ftAxis[grepl('-Z', dtTidy$ftName)] <- 'Z'

dtTidy$ftName <- NULL

##Write table
write.table(dtTidy, file.path(path, 'tidy.txt'), row.names=FALSE)
