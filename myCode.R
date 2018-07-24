#listing 2.1 reading the UCI car data
uciCar <- read.table(
  'http://www.win-vector.com/dfiles/car.data.csv',
  sep = ',',
  header=T)
class(uciCar) #df
summary(uciCar) #details on the df
dim(uciCar) #7col/1728row numbers, number of row is -1 of total to exclude the header


#2.3 loading the credit dataset
d <- read.table(paste('http://archive.ics.uci.edu/ml/',
                      'machine-learning-databases/statlog/german/german.data',sep=''),
                stringsAsFactors=F,header=F)
print(d[1:3,])
#assign the names onto the dataset
colnames(d) <- c('Status.of.existing.checking.account',
                 'Duration.in.month','Credit.history','Purpose',
                 'Credit.amount','Savings account/bonds', 'Present.employment.since',
                 'Installment.rate.in.percentage.of.disposable.income',
                 'Personal.status.and.sex', 'Other.debtors/guarantors',
                 'Present.residence.since', 'Property', 'Age.in.years',
                 'Other.installment.plans', 'Housing',
                 'Number.of.existing.credits.at.this.bank', 'Job',
                 'Number.of.people.being.liable.to.provide.maintenance.for',
                 'Telephone', 'foreign.worker', 'Good.Loan')
d$Good.Loan <- as.factor(ifelse(d$Good.Loan == 1, 'GoodLoan', 'BadLoan'))
print(d[1:3,])
mapping <- list('A11'='... < 0 DM',
                'A12'='0 <= ... < 200 DM',
                'A13'='... >= 200 DM / salary assignments for at least 1 year',
                'A14'='no checking account',
                'A30'='no credits taken/all credits paid back duly',
                'A31'='all credits at this bank paid back duly',
                'A32'='existing credits paid back duly till now',
                'A33'='delay in paying off in the past',
                'A34'='critical account/other credits existing (not at this bank)',
                'A40'='car (new)',
                'A41'='car (used)',
                'A42'='furniture/equipment',
                'A43'='radio/television',
                'A44'='domestic appliances',
                'A45'='repairs',
                'A46'='education',
                'A47'='(vacation - does not exist?)',
                'A48'='retraining',
                'A49'='business',
                'A410'='others',
                'A61'='... < 100 DM',
                'A62'='100 <= ... < 500 DM',
                'A63'='500 <= ... < 1000 DM',
                'A64'='.. >= 1000 DM',
                'A65'='unknown/ no savings account',
                'A71'='unemployed',
                'A72'='... < 1 year',
                'A73'='1 <= ... < 4 years',
                'A74'='4 <= ... < 7 years',
                'A75'='.. >= 7 years',
                'A91'='male : divorced/separated',
                'A92'='female : divorced/separated/married',
                'A93'='male : single',
                'A94'='male : married/widowed',
                'A95'='female : single',
                'A101'='none',
                'A102'='co-applicant',
                'A103'='guarantor',
                'A121'='real estate',
                'A122'='if not A121 : building society savings agreement/life insurance',
                'A123'='if not A121/A122 : car or other, not in attribute 6',
                'A124'='unknown / no property',
                'A141'='bank',
                'A142'='stores',
                'A143'='none',
                'A151'='rent',
                'A152'='own',
                'A153'='for free',
                'A171'='unemployed/ unskilled - non-resident',
                'A172'='unskilled - resident',
                'A173'='skilled employee / official',
                'A174'='management/ self-employed/highly qualified employee/ officer',
                'A191'='none',
                'A192'='yes, registered under the customers name',
                'A201'='yes',
                'A202'='no')

#for loop convert the A- into meaningful expression using the list defined above
# dimension of d, dim(d) -> 1000, 21
for (i in 1:(dim(d))[2]){ #take the col number = 21 of df ==> in each column form 1 to 21
  if(class(d[,i]) == 'character') {# if the 1st element in each col is a character
    #find the cooresponding character elements of A- in the list
    #convert each column into factor variables
    #each factor have levels for different categories for viewing purposes
    d[,i] <- as.factor(as.character(mapping[d[,i]])) 
  }
}
#CHAPTER 3=========================================================================================================

#use the factor variable for viewing via levels
print(d[1:3,"Purpose"]) #see the purposes of the first three loan
summary(d$Purpose)
table(d$Purpose, d$Good.Loan) #table uses the cross-classifying factors to build a contingency 
                              #table of the counts at each combination of factor levels.

#play with example dataframe
iris = as.data.frame(iris)
summary(iris)

#make names db safe: no "." or illegal characters,
#all lower case & unique
dbSafeNames = function(names){
   names = gsub('[^a-z0-9]+','_',tolower(names))
   names = make.names(names, unique = TRUE, allow_ = TRUE) 
   names = gsub('.','_',names, fixed = TRUE)
}
colnames(iris) = dbSafeNames(colnames(iris))
summary(iris)

#3.2 data vitualization
#WRONG custdata = read.csv("./Custdata/custdata.tsv")
custdata <- read.table('./Custdata/custdata.tsv',header=TRUE,sep='\t')
summary(custdata)

library(ggplot2)
#3.6 make a histogram of the population age
#binwidth is the size of interval in the x-axis = 5 years
#fill = "gray", the default colour is black
ggplot(custdata) + geom_histogram(aes(x=age),binwidth = 5,fill = "gray")

#3.7 make a density plot
library(scales) #brings in the dollar scale notation
ggplot(custdata) + geom_density(aes(x=income)) + scale_x_continuous(labels = dollar)

#3.8 create a log-scaled density plot to see more details of the plot in 3.7
#interpretation: most customers have income of 20 - 100,000, peak at 40,000$
ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_log10(breaks = c(100,1000,10000,100000),labels = dollar) + 
  annotation_logticks(sides = "bt") #scales from 1 to 10 of the unit on the x-axis

#3.9 bar chart = histogram of discrete variables
#vertical bar chart
ggplot(custdata) + geom_bar(aes(x = marital.stat),fill = "grey")

#horizontal bar chart
#by default, the categories are sorted in alphabetical #s
ggplot(custdata) + 
  geom_bar(aes(x=state.of.res),fill = "gray") + #plot: state.of.res on X-axis; count on Y-axis
  coord_flip() + #flip the x & y axes
  theme(axis.text.y=element_text(size=rel(0.8))) #reduce the size fo the y-axis tick labels to 80% of the default size for legibility

#3.10
#sorted barchart for better visualization
statesums <- table(custdata$state.of.res) #this table give the name of the states + frequencies of appearence
statef <- as.data.frame(statesums)
#class(statef$state.of.res) -> factor
colnames(statef) <- c("state.of.res","count") # rename the default names "var" & "freq" to be "state.of.state" & "count"
summary(statef) #default order is alphabet
#reorder the data.frame by counts
statef <-transform(statef,state.of.res= reorder(state.of.res, count))
summary(statef)

#make a horizontal bar chart
ggplot(statef) + geom_bar(aes(x=state.of.res,y=count), stat = "identity", fill = "gray") + 
  #*stat = "identity" when the y-axis is a column; stat = "bin" when the y-axis is NOT a column
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))

#3.11 producing a line plot
x <- runif(100) # x = data uniformly distributed between 0 - 1
y<-x^2 + 0.2*x #y is a quadratic function of x
ggplot(data.frame(x=x, y=y), aes(x=x, y=y)) + geom_line() #plot the line

#3.12 examing the correlation be age and income
custdata2 <- subset(custdata, (custdata$age >0 & custdata$age <100 & custdata$income >0))
cor(custdata2$age, custdata2$income)

  #make a scatter plot about $age vs $income + a smooth line
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + 
  stat_smooth(method='lm') + #make a straight line to depict the relationship bw the 2 variables
  ylim(0,200000)
    #analysis: on the left side of the graph, income increase as age inc; right side: income decrease as age inc

  #plotting a smoothing curve through the data instead of a curve
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + 
  stat_smooth() + #make a smooth line(curve) to depict the relationship bw the 2 variables
  ylim(0,200000)

#3.13 plot age and health insurance status
ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) +  # "y=as.numeric(health.ins)" converts boolean into numbers  of 1/0
  geom_point(position=position_jitter(w=0.05, h=0.05)) + # add random noise to a plot -> easier to read, avoid overplotting
  geom_smooth()

  #make a hexbin plot
install.packages("hexbin")
library(hexbin)
ggplot(custdata2, aes(x=age, y=income)) + 
  geom_hex(binwidth=c(5, 10000)) + #create hexbin with age binned into 5-year increments, income increments of 10,000$
  geom_smooth(color="white", se=F) +
  ylim(0, 200000)

#3.15 barchart for categorical variables x3 , specifying different styles of bar chart
  #1 stacked bar chart:
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill = health.ins)) 
  #2 side-by-side bar chart: 
    #harder to compare abs values of customers in each categories, 
    #but easier to compare insured/uninsured across categories
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins),position="dodge")
  #3 filled bar chart: better for comparison of ratios in each categories, 
    #each bar represents the population of the category normalized to 1
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins), position="fill")

#3.16 plotting data with a rug
ggplot(custdata, aes(x=marital.stat)) +
  geom_bar(aes(fill=health.ins),position="fill")+
  geom_point(aes(y=-0.05), size = 0.75, alpha = 0.3,
             position = position_jitter(h=0.01))

  #1 side-by-side bar chart
ggplot(custdata2) + geom_bar(aes(x=housing.type, fill = marital.stat), position = "dodge") +
  #coord_flip()+ : flip the x and y aces
  theme(axis.text.x=element_text(angle=45,hjust=1))

  #2 faceted bar chart by housing types to split the graphs based on housing types
ggplot(custdata2) + geom_bar(aes(x=marital.stat), position="dodge", fill = "darkgray") +
  facet_wrap(~housing.type, scales='free_y') +  #each y-axis has different scales in each group
  theme(axis.text.x=element_text(angle = 45, hjust=1))

#CHAPTER 4 managing data===========================================================================

#4.1 chekcing the locations of missing data

#1)missing data in categorical variables
summary(custdata[is.na(custdata$housing.type), c("recent.move", "num.vehicles")])
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),"missing",#if T, assign the "missing" to this category
                                   ifelse(custdata$is.employed==T, "employed","not employed")) 
                                    #if is.employed == T, then assign "employed"; 
                                    #if is.employed == F, assign "not employed".
summary(as.factor(custdata$is.employed.fix)) #turn the assigned info from string back to factor
#instead of assigning them into "missing data", it is more appropriate to mean "not in active workforce"
#use a new variable to store the assigned information
#the ifelse(boolean,T-do this, F-do this)
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),"not in active workforce",#if T, assign the "missing" to this category
                                   ifelse(custdata$is.employed==T, "employed","not employed")) 

#2)missing values in numeric data
summary(custdata$income)
#2.1)when values are missing randomly
meanIncome <- mean(custdata$income, na.rm = T)
Income.fix <- ifelse(custdata$income < 0, meanIncome, custdata$income) #replace the negative value to a meanIncome
summary(Income.fix) #the min is non-negative anymore

#2.2)when the are missing systematically
breaks <- c(0, 10000, 50000, 100000, 250000, 1000000)
Income.groups <- cut(custdata$income, breaks = breaks, include.lowest = T) 
#by default, the $0 wont be included into the categories
#the last boolean = T -> gets the $0 into the categories

#use the cut() function to produce factor vairables to categorize the income groups. NAs are preserved 
summary(Income.groups)
Income.groups <- as.character(Income.groups)
Income.groups <- ifelse(is.na(Income.groups),"no income", Income.groups) #put the "NA" into "no income" group
summary(as.factor(Income.groups))
missingIncome <- is.na(custdata$income)
Income.fix <- ifelse(is.na(custdata$income), 0, custdata$income) #replace the "NA" with 0

#4.1.2 Data transformations
medianincome <- aggregate(income~state.of.res,custdata,FUN=median)
head(medianincome)
colnames(medianincome) <- c('State','Median.Income')
summary(medianincome)

#merge median income info into the custdata df 
#by matching the col-custdata$state.of.res to the col-medianincome$$State
custdata <- merge(custdata, medianincome, by.x="state.of.res", by.y = "State") 
summary(custdata[,c("state.of.res", "income", "Median.Income")])

#normalize income by "Median.Income"
custdata$income.norm <- with(custdata, income/Median.Income)
summary(custdata$income.norm)
custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)

#convert age into ranges cut()

#select the age ranges of interest. 
#The upper and lower bounds should encompass the full range of the data
brks <- c(0, 25, 65, Inf) 

#"include.lowest = T" make sure that the 0 age data is included in the lowest age range categories
# it is excluded by default
custdata$age.range <- cut(custdata$age,breaks = brks, include.lowest = T)
summary(custdata$age.range) #factor output

#normalize to the mean age
summary(custdata$age)
meanage <- mean(custdata$age)
custdata$age.normalized <- custdata$age/meanage
summary(custdata$age.normalized)
meanage <- mean(custdata$age)
stdage <- sd(custdata$age) #take the standard deviation
meanage
stdage

#use the mean value as the origin/reference point ->
#rescale the distance from the mean by the standard deviation
custdata$age.normalized <- (custdata$age - meanage)/stdage
summary(custdata$age.normalized)
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

#4.2 Sampling for modeling and validation
#4.2.2 creating a sample group column-splitting into test and training using a random group mark
custdata$gp <- runif(dim(custdata)[1]) #runif(1000) to make 1000 random and uniformly distributed nums to the df
testSet <- subset(custdata, custdata$gp <= 0.1) #test set if about 10% of the the data
trainingSet <- subset(custdata, custdata$gp > 0.1) #training uses about 90% of the data
dim(testSet)[1]
dim(trainingSet)[1]

#4.2.3  record grouping

# hh <- unique(hhdata$household_id) #get all unique household IDs from your df
# households <-data.frame(household_id = hh, gp = runif(length(hh))) #create a temporary df of household IDs and a uniformly random #s from 0 to 1
# hhdata <- merge(hhdata,households, by = "household_id") #merge new random ssample group column back into original data frame

#4.2.4 data provenance
#END OF Chpt #4

