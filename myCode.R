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

