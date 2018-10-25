
##Task Market Basket Analysis
#Load Package:

library(arules)
library(arulesViz)

###Set the Working Directory:
setwd("C:/Users/Ruthesan-PC/Desktop/Ubiqum_class/R_studio/Task_4")

## Read Transaction data in basket format
Transactions <- read.transactions(file = "ElectronidexTransactions2017.csv", 
                                  format = "basket", sep=",")

## Print Item Lables from the data:
inspect(Transactions[1]) #print first transcation 
LIST(Transactions)[1] #print first transaction as list
itemLabels(Transactions)

## Summary of the data provides size of data, most frequent item 
##                          & freq of each size of transactions
summary(Transactions)

## Plot top 15 products sold based on volume
itemFrequencyPlot(Transactions, topN=15, type='absolute', ylab="Item Frequency (No's)")


RulesName<- apriori(Transactions, parameter = list(supp = 0.005, conf = 0.5, target = "rules"))

summary(RulesName)

#inspect(RulesName)

plot(RulesName)


inspect(sort(RulesName, by = "lift"))











