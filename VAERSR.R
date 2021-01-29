
library(readr)
X2019VAERSDATA <- read_csv("2019VAERSDATA.csv")
library(readr)
X2019VAERSDATA <- read_csv("2019VAERSSYMPTOMS.csv")
library(readr)
X2019VAERSVAX <- read_csv("2019VAERSVAX.csv")
d.df <- read.csv("2019VAERSDATA.csv")
s.df <- read.csv("2019VAERSSYMPTOMS.csv")
v.df <- read.csv("2019VAERSVAX.csv")
#Creating a data frame for each file 
x1.df = merge(d.df, s.df)
dsv.df = merge(x1.df, v.df)
#Merging all the data frames on one data frames by VAERS ID
install.packages("Hmisc")
library(Hmisc)
describe(dsv.df)
summary(dsv.df)
d.df$VAERS_ID
ID <- d.df$VAERS_ID
AGE <-d.df$AGE_YRS
HOSPITALIZATION <- d.df$NUMDAYS
ONSET <- d.df$ONSET_DATE
VAX <- d.df$VAX_DATE
plot(x=HOSPITALIZATION, y=AGE, main = "AGE VS NUMBER OF DAYS HOSPITALIZED")
install.packages ("ggplot2")
library ("ggplot2")
ggplot() + geom_histogram(data= dsv.df, aes(x=AGE_YRS),fill="blue", 
                          color="black")
# Displays a histogram where the age of the person and the person on the list. 
max.col(HOSPITALIZATION)
range(HOSPITALIZATION)
HOSPITALIZATION[ is.na(HOSPITALIZATION)] <- 0
AGE[ is.na(AGE)] <- 0
summary(AGE)
max.col(HOSPITALIZATION)
HOSPITALIZATION
plot(x=AGE, y=HOSPITALIZATION, main = "AGE VS NUMBER OF DAYS HOSPITALIZED")

SEX<- d.df$SEX
summary(SEX)
ifelse(SEX == "F", 1, ifelse(SEX == "M",2, 0))
pie(SEX)
SEX(F)
SEX[ is.na(SEX)] <- 0
as.numeric(SEX)
pie(SEX)
table1 <- as.data.frame(table(VaxSite));table1
barplot(table$Freq,main="Injection Site",names.arg=table$VAXSITE)
library(ggplot2)
plot= ggplot(table, aes(x=table$SEX, y=table$Freq))
plot + geom_area()        
VaxSite <- v.df$VAX_SITE

plot= ggplot(table1, aes(x=VaxSite, y=Freq, fill=))
plot+geom_area()
slices <- c(28076, 13735,6643)
lbls <- c("Females", "Males", "Unknown")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="SEX")
a <- c("AR", "GM" "la", "LG", "LL","MO","NS","OT","RA","RL","UN")
b <- c(948, 21,21253, 684, 3470, 726, 41, 54, 9727, 2657, 4090)
table2<- table(a,b)
barplot(b, main = "INJECTION SITE FREQUENCY", xlab = "Injection Site",
ylab = "Frequency",names.arg = c("AR", "GM" "LA", "LG", "LL","MO","NS","OT","RA","RL","UN")

barplot(b, main = "Injection Site Frequency", xlab ="Injection Site", ylab = "frequency"  names.arg = c("AR", "GM","LA","LG","LL", "MO", "NS","OT", "RA", "RL","UN"))
plot(b,type = "o", ylab = "frequency",names.arg = c("AR", "GM","LA","LG","LL", "MO", "NS","OT", "RA", "RL","UN"))
table2 <- as.data.frame(table(HOSPITALIZATION));table2
ta