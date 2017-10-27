#tuto: https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic

# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

# Load data csv files into R datasets 
train <- read.csv('train.csv',stringsAsFactors = FALSE)
test <- read.csv('test.csv',stringsAsFactors = FALSE)

full <- bind_rows(train,test) # bind training & test data 
#bind_rows different de rbind: permet de bind 2 dataset même s'il ne sont pas de la même largeur (nb de colonne) en rajouttant des "N/A"
#rbind permet de bind 2 dataset obligatoirement de la même largeur 
#ici test et train n'ont pas la même largeur donc on utilise le bind_rows

# check data (afficher la proprité des données)
str(full)

# Survived	Survived (1) or died (0)
# Pclass	Passenger’s class
# Name	Passenger’s name
# Sex	Passenger’s sex
# Age	Passenger’s age
# SibSp	Number of siblings/spouses aboard
# Parch	Number of parents/children aboard
# Ticket	Ticket number
# Fare	Fare
# Cabin	Cabin
# Embarked	Port of embarkation


#Feature Engineering
# Grab title from passenger names and create a new column in full dataset 
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex (now we can retreive Title vs Sex)
#table permet de faire un Count() 
table(full$Sex, full$Title) 

#Count a selected element in "full$Title" column (for example: count only 'Mlle')
table(full$Title[full$Title == 'Mlle'])

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly (on reassigne de nouvelles valeurs à la colonne Title
full$Title[full$Title == 'Mlle'] <- 'Miss' #toutes les valeurs 'Mlle' deviendront 'Miss' dans la colonne Title  
full$Title[full$Title == 'Mme'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare Title' #toutes les valeurs dans notre vecteur rare_title deviendront 'Rare Title' dans la colonne Title  


# Finally, grab surname from passenger name
#sapply parcours la colonne full$name en assignant chacun de ses elements à 'x'
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,]')[[1]][1])

#return all Unique Surnames in full$Surnames column 
factor(full$Surname)
#return the number of Unique Surnames in full$Surnames column 
nlevels(factor(full$Surname))
#convert the number of Unique Surnames in full$Surnames column in STRING type 
# CAT: It converts its arguments to character vectors
cat(nlevels(factor(full$Sex)))



#-----------------------------------
#Do families sink or swim together?
#-----------------------------------

# Create a family size variable including the passenger themselves
# we add "+1" because their is always one person in the family even if Number of siblings/spouses aboard & Number of parents/children aboard = 0
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable in full dataset: "surname_familysize"
full$Family <- paste(full$Surname, full$Fsize, sep='_')


# Use ggplot2 to visualize the relationship between family size & survival
# more information on ggplot here: http://ggplot2.tidyverse.org/reference/geom_bar.html 
ggplot(data = full[1:891,], mapping = aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size', y = 'Survivors') +  
  theme(legend.position = "top")

# Discretize family size
full$FsizeD[full$Fsize == '1'] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# count survivors group by FsizeD
table(full$FsizeD, full$Survived)

# Plot mosaic chart 
mosaicplot(table(full$FsizeD, full$Survived), main = 'Family size survivors', shade = TRUE)

# Create a Deck variable. Get passenger deck A - F:
full$Deck <- sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1])


# plot the number of passager / Deck number
ggplot(data=subset(full[1:1309,], !is.na(Deck)), aes(x=Deck, fill=factor(Count))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Deck number', y = 'Number of passager') +  
  theme(legend.position = "top")

# plot the Survivors / Deck number
ggplot(data=subset(full[1:891,], !is.na(Deck)), aes(x=Deck, fill=factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Deck number', y = 'Survivors') +  
  theme(legend.position = "top")


#-----------------------------------
#start exploring missing data
#-----------------------------------

#goal-> find Embarkment gate of passengerId = 62 & passengerId = 830


# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked'] #result before changement: NA,NA

#before using pipe operator %>% import-> library(magrittr)
embark_fare <- full %>% filter(full$PassengerId != 62 & full$PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(full, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() + #plot box chart 
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=1) #display red line Fare = 80$


# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'


#goal-> find Fare of passengerId = 1044

full[1044,] #result before changement: "Fare"=NA

#analytical method (median Fare calculation)
median(full[full$Embarked == 'S' & full$Pclass == '3',]$Fare, na.rm = TRUE)

#chart method
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ],aes(x = Fare)) +
  geom_density(fill = 'yellow', alpha=0.5) + 
  scale_x_continuous(breaks = seq(0, 80, 5)) + #to modified the scale axis 
  #coord_cartesian(xlim=c(5, 11)) + #to zoom 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),colour='red', linetype='dashed', lwd=1)


# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

#---------------------------------------------------------------------------------------------------------
#How do we calculate a median  ? 


#select range of data 
testsort <- full[full$Pclass == '3' & full$Embarked == 'S',]$Fare 

#sort it by ascending 
testsort <- sort(testsort, decreasing = FALSE)

#check the length of the list 
length(testsort)

#if the lengh is impaire then the median is the value at length(testsort)/2 of the list
#if the lengh is paire then the median is the value between (length(testsort)/2)-1 and (length(testsort)/2)+1 of the list

#get the median value
testsortfilter <- testsort[245:250,]
testsortfilter
#---------------------------------------------------------------------------------------------------------

#-----------------------------------
#sPredictive imputation
#-----------------------------------

#goal-> find Age missing 

# Show number of missing Age values
sum(is.na(full$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# More here: https://www.rdocumentation.org/packages/mice/versions/2.30/topics/mice
# Perform mice imputation, excluding certain less-than-useful variables:
# mice function will predict some variable thanks to columns givin in parameter
# method='rf'= Random forest imputations (any)
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived', 'Count', 'Mother', 'Child')], method='rf')

#see mice detail
mice_mod

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions

#split plot screen in 2 
par(mfrow=c(1,2))
#first chart 
hist(full$Age, freq=F, main='Age: Original Data', col='darkgreen', ylim=c(0,0.04))
#Second chart 
hist(mice_output$Age, freq=F, main='Age: MICE Output', col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))


#-----------------------------------
# Feature Engineering: Round 2
#-----------------------------------

# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex)

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)


# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

# Display missing-data patterns
md.pattern(full)

# We have finally finished treating all of the relevant missing values



#-----------------------------------
# 4 Prediction
#-----------------------------------

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]


# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
# do perform a RF  model here, "Survived" have to be full of data (if one value is missing you cannot run the RF)
rf_model <- randomForest(factor(Survived) ~ 
                         Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother,
                         data = train)
# Show model error
# The black line shows the overall error rate which falls below 20%. 
# The red and green lines show the error rate for ‘died’ and ‘survived’ respectively. 
# We can see that right now we’re much more successful predicting death than we are survival. 
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,1],2))
#sorted by Importance ASC (only to try sorting)
varImportanceSorted <- varImportance[with(varImportance, order(-Importance)), ]

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

#end 




# create a new list 'testwithSolution' with the same value of 'test' list 
testwithSolution <- data.frame(test)

# add solution Survived value to the new list 'testwithSolution'
testwithSolution$Survived <- solution$Surivived

# Survived type = factor (not good for manipulation)
str(testwithSolution$Survived)

# As the Survived column is a factor we need to convert it in integer same as 'train$Survived' list
# first we convert it in CHARACTER type 
testwithSolution$Survived <- sapply(testwithSolution$Survived, function(x) as.character(x))
# then we convert it in INTEGER type 
testwithSolution$Survived <- sapply(testwithSolution$Survived, function(x) as.integer(x))

# as testwithSolution$Survived and train$Survived have the same type, we can now bind the two lists 
full_s <- bind_rows(train, testwithSolution)

# Finally we can retreive all the survivors of the titanic 
table(full_s$Survived)

# 0 value missing in full_s$Survived column compare to "md.pattern(full) "
md.pattern(full_s)
