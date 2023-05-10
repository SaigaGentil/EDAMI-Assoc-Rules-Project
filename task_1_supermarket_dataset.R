#### Information ####


# Author: Bruno Axel Kamere
# Github: https://github.com/SaigaGentil
# Course: 103B-CSCSN-MSA-EDAMI
# Professor:
# Period: Summer Semester 2022/2023
## Warsaw University of Technology ##

## Task Description ##
# Given a supermarket dataset, analyze the data and generate association rules such the the goals set below (0.1) are achieved.


#### 0. Plan ####


# 1. Load libraries and dataset
# 2. Data preparation
# 3. Find Association rules
# 4. Conclusion


##### 0.1 Analysis Goals ####


# a) Identify frequently co-occurring products
# b) Identify frequently bought single items to optimize inventory
# c) Identify how likely a customer is to buy product B if they have already bought product A

# To achieve the above goals, association rules need to be generated and analysed. They can be generated using multiple algorithms but the most popular being Apriori and Eclat algorithm. For this experiment, I will focus more on Eclat algorithm.
 
##### 0.2 Definitions and terminologies #####


# Def. Rules in data mining
# Rules refer to patterns or relationships found in the data that describe a certain behavior or occurrence. These rules typically take the form of "if-then" statements, where the antecedent (the "if" part) describes a particular condition or set of conditions, and the consequent (the "then" part) describes the outcome or action that follows from those conditions.

# Def. Association rules
# Association rules are a set of rules that are used to identify relationships between items in a dataset. Specifically, they are used to identify patterns and relationships between variables that frequently co-occur together in a given dataset. These rules are based on the concept of 2 main things; support and confidence (See them defined below).The higher the support and confidence, the stronger the association rule between the variables.

# Def. Support
# Support is the probability of have 2 items (A and B) being bought together

# Def. Confidence
# Confidence is the probability that item B will be bought given that item A was bought as well.

# Def. Lift
# Lift is the ratio of observed support to expected support given that A and B Are independent. A lift greater than 1 indicates a positive association.

# Def. Eclat algorithm
# Eclat (Equivalence Class Clustering and bottom-up Lattice Traversal) is a popular algorithm, among many others, used for mining frequent itemsets and generating association rules in data mining. It generates itemsets based on their frequency of occurrence in a transactional dataset. This is the algorithm that we wil focus on in this experiement.


#### 1. Load libraries and dataset ####


library(arules) # association rules
library(arulesViz) # visualization of rules

# Download supermarket dataset
download.file(
  'http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/supermarket.csv',
  'supermarket.csv'
)
# Read the dataset and set separator to semicolon
marketSet = read.csv('supermarket.csv', sep = ';')


#### 2. Data preparation ####


##### 2.1 Convert dataset to logical Boolean values #####


# See dataset structure
str(marketSet)

# Apply a "as.logical" function to each column of the dataset to convert the values to logical Boolean values.
# This needs to be done since the data to be analyzed is Boolean in nature.
marketSetLogical = as.data.frame(sapply(marketSet, function(x)
  as.logical(x)))


##### 2.2 Get the dataset statistics #####


# See dataset structure
str(marketSetLogical)

# See the summary of the dataset
summary(marketSetLogical)

# Read the first six rows of the dataset
head(marketSetLogical)

# Get the number of rows in the dataset
nrow(marketSetLogical)

# Get column names
colnames(marketSetLogical)

# Get column names with NA values
colnames(marketSetLogical)[apply(is.na(marketSetLogical), 2, any)]

# With the above command "total" column was identified to have NA values.
# See how many records have NA values in the column
length(which(is.na(marketSetLogical$total) == TRUE))


##### 2.3 Remove dispensable columns #####
# Since the "total" column contains NA values and is irrelevant for the next data analysis, it needs to be removed


marketSetLogical["total"] <- NULL


##### 2.4 Convert dataset from "data.frame" type to "transaction" type #####
# This is because algorithms only work on transactions


marketSetLogicalTR <- as(marketSetLogical, "transactions")

# See dataset structure
str(marketSetLogicalTR)


#### 3. Find Association rules ####

##### 3.1 Find the Support #####
# The support is the proportion of transactions that contain both A and B items

# Use the itemFrequency function to get the frequency or support for single items
freqTable = itemFrequency(marketSetLogicalTR, type = "relative")


length(marketSetLogicalTR)

str(freqTable)
print(freqTable)

# Sort according  relative support values
freqTable = sort(freqTable, decreasing= TRUE)
print(freqTable)

# Number of items with support >= 10%
length(freqTable[freqTable >= 0.1])

# Chart the frequent items with support >= 50%
itemFrequencyPlot(marketSetLogicalTR, type = "relative", support = 0.05)


##### 3.2 Eclat algorithm ####
###### 3.2.1 Discover frequent itemsets - Eclat algorithm ######
#  This algorithm works by counting the number of transactions in which itemsets occur and iteratively combining frequent itemsets to form larger ones.


# Initialize Eclat parameters
eclatParam  = new("ECparameter", "confidence" = 0.8, "support" = 0.05) 
print(eclatParam)

# Discover frequent itemsets
frequentsetsEclat <- eclat(marketSetLogicalTR,eclatParam)

# Get the length of discovered itemsets
length(frequentsetsEclat)

# Inspect the discovered itemsets
inspect(sort(frequentsetsEclat, by = "support" ))


###### 3.2.2 Generate Association rules - Eclat algorithm ######

?ruleInduction
eclatAssocRules = ruleInduction(frequentsetsEclat, marketSetLogicalTR, confidence = 0.8)
summary(eclatAssocRules)
# Get amount of generated rules
length(eclatAssocRules)


###### 3.2.4 Analyze Association rules - Eclat algorithm ######

?sort
# Inspect and sort by "lift" (See the introduction section for "lift" definition)
inspect(sort(eclatAssocRules, by = "lift"))
# Inspect and sort by "support" (See the introduction section for "support" definition)
inspect(sort(eclatAssocRules, by = "support"))
# Inspect and sort by "confidence" (See the introduction section for "confidence" definition)
inspect(sort(eclatAssocRules, by = "confidence"))

?subset
# Filter rules by selecting rules of interest
# Get rules with lift >1.2
# This is so that we can only see rules with the reasonably high strength of association between the antecedent (lhs - left-hand-side) and consequent item(s)(rhs - right-hand-side).
eclatAssocRulesLift1.2 <- subset(eclatAssocRules, subset = lift > 1.1)
# Get how many rules are found
length(eclatAssocRulesLift1.2)
# Inspect the rules from low to high lift value
inspect(sort(eclatAssocRulesLift1.2, by = "lift", decreasing = FALSE))

#charts presenting association rules
size(eclatAssocRulesLift1.2)

# Extract rules given that rhs from eclatAssocRules is bread.and.cake and lift >=1.2.
?subset
eclatAssocRulesInGivenConseq <- subset(eclatAssocRules, subset = rhs %in% "bread.and.cake" & lift >=1.2)
inspect(eclatAssocRulesInGivenConseq)

# Visualize association rules with lift > 1.2
plot(eclatAssocRulesInGivenConseq, method = "graph", engine = "htmlwidget")
plot(eclatAssocRulesLift1.2, shading="order", control=list(main = "Two-key plot" ))
plot(eclatAssocRulesLift1.2, method="matrix", measure="lift", interactive = TRUE)


# Extract rules based on maximal frequent itemsets
# Maximal frequent itemsets refer to a set of items that occurs frequently in the data and there are no supersets of this set that have the same frequency. This means that if any item is added to this set, the frequency of the resulting superset decreases. In simple words, maximal frequent itemsets represent the most frequent combinations of items that cannot be further extended
eclatAssocmaxRules <- eclatAssocRules[is.maximal(eclatAssocRules) == TRUE]
summary(eclatAssocmaxRules)
inspect(eclatAssocmaxRules[1:10])


#### 4. Conclusion - Eclat algorithm ####


# With a supermarket dataset of 4627 transactions and multiple items/item categories being used for this experiment, below are the conclusions based on the set objectives of the experiment.

length(marketSetLogicalTR) # Amount of supermarket transactions used for the experiment.

# Applying the Eclat algorithm helped us achieve our set goals for this experiment.
# a) Identify frequently co-occurring products
# With the output of the command(s) below, it has been identified that, among other very frequently bought itemsets, items "bread.and.cake" and "margarine" are the most frequently bought together where they cover 39% of all transactions. This can help the supermarket set out promotions including both of these products and other items like items that are about to expire in coming months to get more people to saturate the inventory of products about to expire thus reducing losses. The same promotions can also be used to promote new products.

inspect(sort(eclatAssocRules, by = "support")) # Get assoc. rules ranked by support.. Top to down.


# b) Identify frequently bought single items to optimize inventory
# With the output of the command(s) below, it has been identified that "bread.and.cake" is the most frequently bought item/item category, with 70% of all transactions including bread.and.cake, and "gourmet.meat" being the least frequently bought item/item category at the rate of 0.04%. This indicates that the supermarket should optimize inventory to have more of "bread.and.cake" and way less of "gourmet.meat".

sort(freqTable, decreasing= TRUE) # Ranked high to low
(freqTable) # Ranked low to high



# c) Identify how likely a customer is to buy product(s) B if they have already bought product(s) A
# With the output of the command(s) below, it has been identified that the probability that customers buy product(s) "bread.and.cake" given that they have also bought products "biscuits, frozen.foods, pet.foods, milk.cream, vegetables" is 92%. This means that customer are likely to also buy "bread.and.cake" if they have also bought at the same time "biscuits, frozen.foods, pet.foods, milk.cream, vegetables". This can help the supermarket better create promotions involving a combination of these, or even help the supermarket better layout where products are placed across the supermarket.

inspect(sort(eclatAssocRules, by = "confidence")) # Get Asooc. rules ranked by confidence.. Top to down.
