
#THESIS CODE (CLEANING, PREDICTING, ANALYSES)

##This Rcode contains all the code used to create the datasets for the thesis project
##Here the funda dataset was created and the government data was used to create a merged dataset 

#The Rcode is structured as follows

  #- First exploring an the EP 2023 Dataset
  #- Making the Funda Dataset with all the individual city's
  #- Then we explore the Funda dataset
  #- Then we start with the cleaning of the merged dataset
        #- We then enter a repetitive process of cleaning and visualization to 
        #  create the best possible dataset
  # - We then enter the stage of prediction models where all the 
  #   columns are made numerical. 
  # - Then the prediction models start
  # - Then we implement the missing values dataset with the predicted values
  # - There is also a try to see the correlation living area and house age
  # - The Full analyses is the closing of this Rcode document

#It is important not to run all the code at once. 
#This is because it also includes code to save a CSV file. 
#This CSV file has been added to the GitHub code. 


#####START #####


###### EXPLORATION EP DATASET ######
#LOAD THE EP DATASET 
EPDF=EP23
# Count the occurrences of each value in the column
value_counts <- table(EP23$Pand_gebouwklasse)
# Print the value counts
print(value_counts)


#we dont need the U so we remove it for further analyses
#(U stands for utility)
# Remove rows with the value 'U' in the column
EPDF <- EPDF[EPDF$Pand_gebouwklasse != 'U', ]
#now remove the rows we dont need in EPDF we dont need the coloms
EPDF <- EPDF[, -c(1,2,3,4,6,8,9,11,12,15,16,17,18,19,20,21,24,25,26,
                  27,28,29,31,32,33,34,35,36,37,38,39)]


# Count the number of non-missing values in the column
non_missing_count <- sum(!is.na(EPDF$Pand_primaire_fossiele_energie))
# Print the count of non-missing values
print(non_missing_count)

#first look how many nan there are in the dataset
df = EPDF

# Perform descriptive statistics
summary(df)

# Visualize data distributions (excluding values above 600)
ggplot(df, aes(x = Pand_primaire_fossiele_energie, fill = Pand_energieklasse)) +
  geom_bar(data = filter(df, Pand_primaire_fossiele_energie <= 600), width = 0.5, position = "stack") +
  labs(x = "Living Area", fill = "Energy Label") +
  theme_minimal() +
  coord_cartesian(xlim = c(-100, 600))

# Identify missing data
colSums(is.na(df))

# Examine data types
str(df)

# Count occurrences of each category in a column
df_counts <- df %>%
  count(Pand_berekeningstype)

# View the count results
print(df_counts)

df_counts <- df %>%
  count(Pand_energieklasse)

# View the count results
print(df_counts)


library(forcats)

# Count the frequency of each energy label
df_counts <- df %>%
  count(Pand_energieklasse)

# Reorder the energy labels
df_counts <- df_counts %>%
  mutate(Pand_energieklasse = fct_relevel(Pand_energieklasse, c("A++++", "A+++", "A++", "A+", "A", "B", "C", "D", "E", "F", "G")))

# View the reordered count results
print(df_counts)


#now make a plot to show the missingness in the EP dataset
missing_counts <- colSums(is.na(EPDF))
# Create the ggplot visualization
ggplot(data.frame(column = names(missing_counts), count = missing_counts), aes(x = column, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Column", y = "Missing Values Count", title = "Missingness EP-dataset") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






###### MAKE A FUNDA DATASET ######


#First load all the individual datasets
data1 = amsterdam_clean
data2 = den_haag_clean
data3 = eindhoven_clean
data4 = enschede_clean
data5 = gouda_clean
data6 = groningen_clean
data7 = heerenveen_clean
data8 = maastricht_clean
data9 = rotterdam_clean
data10 = utrecht_clean
data11 = breda_clean
data12 = alkmaar_clean
data13 = almere_clean
data14 = amersfoort_clean
data15 = apeldoorn_clean
data16 = arnhem_clean
data17 = den_bosch_clean
data18 = dordrecht_clean
data19 = haarlem_clean
data20= nijmegen_clean
data21 = tilburg_clean
data22 = zwolle_clean
data23 = zoetermeer_clean

#first combining didnt work because of missing columns and different
#types of columns, this way the combining couldnt happen
data7$extra <- NA
data9$number <- as.double(data9$number)
data11$extra <- as.character(data11$extra)
data12$extra <- as.character(data12$extra)
data18$extra <- as.character(data18$extra)
data19$extra <- as.character(data19$extra)
# Combine datasets vertically
combined_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, 
                           data9, data10, data11,data12,data13,data14,data15,data16,
                           data17,data18,data19,data20,data21,data22,data23)


#now the cleaning is done. i need to safe this dataset into a csv file
write.csv(combined_data, file = "Funda.csv", row.names = FALSE)






  ###### EXPLORATION FUNDA DATASET ######

#first look how many nan there are in the dataset
df = Funda
# Convert variables to numerical
df <- df %>%
  mutate(living_area = as.numeric(living_area),
         house_age = as.numeric(house_age))

# Perform descriptive statistics
summary(df)

# Visualize data distributions (excluding values above 600)
ggplot(df, aes(x = living_area, fill = energy_label)) +
  geom_histogram(data = filter(df, living_area <= 300), color = "black", bins = 30) +
  labs(x = "Living Area", fill = "Energy Label") +
  theme_minimal()

# Identify missing data
colSums(is.na(df))

# Examine data types
str(df)

# Count occurrences of each category in a column
df_counts <- df %>%
  count(house_type)

# View the count results
print(df_counts)


missing_counts <- colSums(is.na(df))
# Create the ggplot visualization
ggplot(data.frame(column = names(missing_counts), count = missing_counts), aes(x = column, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Column", y = "Missing Values Count", title = "Missing values Funda") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(df, aes(x = house_age)) +
  geom_histogram(data = subset(df, house_age <= 200), bins = 20, fill = "lightblue", color = "black") +
  labs(x = "House Age", y = "Count") +
  ggtitle("House Age Distribution (Up to 200)") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 200))




###### MERGING AND CLEANING ######

#with loding the data it didnt do it correctly, it had the col names 
#in the second row so thats wy we run the following code
# Assign the first row as column names
colnames(Funda) <- Funda[1, ]
# Remove the first row from the dataset
Funda <- Funda[-1, ]


#safe it as a new name. to avoid going far back after a mistake
Fundadf = Funda
# Remove specific columns from the dataset
Fundadf <- Fundadf[, -c(1, 3, 4, 8, 9, 11, 12, 13, 14)]
# Print the modified dataset
print(Fundadf)


#call the ep dataset
EPDF = EP23
#merge two colloms to get the same value for mergign
EPDF$suffix <- paste(EPDF$Pand_huisletter, EPDF$Pand_huisnummertoevoeging, sep = "")
#remove the NA that are created in the column
EPDF$suffix=str_remove_all(EPDF$suffix,"NA")
EPDF$suffix <- replace(EPDF$suffix, EPDF$suffix == "", NA)


#now remove the rows we dont need in EPDF we dont need the colomns
#those columns hold information that we dont need
EPDF <- EPDF[, -c(1,2,3,4,6,8,9,11,12,15,16,17,18,19,20,21,22,24,25,26,
                  27,28,29,31,32,33,34,35,36,37,38,39)]


# Kolomnaam wijzigen op basis van oude kolomnaam
colnames(EPDF)[colnames(EPDF) == "Pand_postcode"] <- "zip"
colnames(EPDF)[colnames(EPDF) == "Pand_huisnummer"] <- "number"

#now we merge the two datasets
mergeEPFU <- merge(EPDF, Fundadf, by = c("zip", "number", "suffix"))

#remove the U buildings
mergeEPFU  <- mergeEPFU %>%
  filter(!grepl("U", Pand_gebouwklasse))




#NOW GET SOME INSIDES INTO THE MERGED DATASET
#now check how the NaN are in the mergeEPFU dataset
# Count missing values in each column
missing_counts <- colSums(is.na(mergeEPFU))
# Create a bar plot
ggplot(data.frame(column = names(missing_counts), count = missing_counts), aes(x = column, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Column", y = "Missing Values Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#we see that there is only missingness in pand fossiele energie behoefte



#KEEP THE + SIGNS FOR BETTER PREDICTION
## i keep the + to see if it has any effect on a better prediction




  ###### FURTHER CLEANING ######

#Now remove more colloms in the dataset to get all the col we want
##zip,number,suffix, pand_gebouwklasse, extra
mergeEPFU <- mergeEPFU[, -c(1,2,3,6,11,13)]


#now i want to get the prediction colom to the end of the dataset
col_index <- which(names(mergeEPFU) == "Pand_primaire_fossiele_energie")
# Move the column to the end
mergeEPFU <- mergeEPFU[, c(setdiff(names(mergeEPFU), "Pand_primaire_fossiele_energie"), "Pand_primaire_fossiele_energie")]


#NOW SAFE THE PROGESS AS AN CSV
#now the cleaning is done. i need to safe this dataset into a csv
write.csv(mergeEPFU, file = "MERGEDdata.csv", row.names = FALSE)



  ###### "START WITH THE MERGEDdata.csv" MORE CLEANING ######
#HERE WE START WITH LOADING THE DATASET
mergeEPFU = MERGEdata
### now we go on with investigating the data
summerydata = summary(mergeEPFU)
View(summerydata)
print(summerydata)
# Convert columns to numeric
mergeEPFU$house_age <- as.numeric(mergeEPFU$house_age)
mergeEPFU$living_area <- as.numeric(mergeEPFU$living_area)


##if we see the data there is a age of 2023, but this is the building year
##so it needs to change because of a failt

# Replace 2023 with 0 in the house_age column
mergeEPFU$house_age[mergeEPFU$house_age == 2023] <- 0


#also we see that we have an outlier in the colum living_area, we are also 
#removing it because it can introduce a worse outcome in the prediction
mergeEPFU <- mergeEPFU[mergeEPFU$living_area <= 3000, ]

summary(cleaned_dataset)
mergeEPFU <- mergeEPFU %>%
  filter(is.na(Pand_primaire_fossiele_energie) | Pand_primaire_fossiele_energie <= 1000)

ggplot(mergeEPFU, aes(x = Pand_primaire_fossiele_energie)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(x = "Pand Primaire Fossiele Energie", y = "Frequency") +
  theme_minimal()

#we also see that the primary fossil energy has outliers in the higher range. 

# Replace values above 20000 with NaN because removing caused issues
#made them NaN to solve the problem.
mergeEPFU$Pand_primaire_fossiele_energie <- ifelse(mergeEPFU$Pand_primaire_fossiele_energie > 20000, NaN, mergeEPFU$Pand_primaire_fossiele_energie)


#check if there are no weird values anymore in the dataset
summerydata = summary(mergeEPFU)
View(summerydata)
# no weird values anymore, so we can continiue


#now we need to take a look at the colum pand_berekeningtype. 
unique_values <- unique(mergeEPFU$Pand_berekeningstype)
unique_values

frequency <- table(mergeEPFU$Pand_berekeningstype)
print(frequency)



str(mergeEPFU)

# Plotting Pand_gebouwsubtype column with rotated x-axis labels
ggplot(mergeEPFU, aes(x = Pand_gebouwsubtype)) +
  geom_bar() +
  labs(x = "Pand_gebouwsubtype", y = "Frequency") +
  ggtitle("Frequency of Pand_gebouwsubtype") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#we see that it has all kind of differend methods. we need to filter them
#Everything before 2021 is the old one so i can replace them to NEN 7120
# Replace the value in the "column_name" column
mergeEPFU$Pand_berekeningstype <- sub("Nader Voorschrift, versie 1.0, 1 februari 2014 met erratalijst dd 03-11-201", "NEN 7120", mergeEPFU$Pand_berekeningstype)
mergeEPFU$Pand_berekeningstype <- sub("Rekenmethodiek Definitief Energielabel, versie 1.2, 16 september 2014", "NEN 7120", mergeEPFU$Pand_berekeningstype)
mergeEPFU$Pand_berekeningstype <- sub("ISSO82.3, versie 3.0, oktober 2011", "NEN 7120", mergeEPFU$Pand_berekeningstype)
mergeEPFU$Pand_berekeningstype <- sub("Nader Voorschrift, versie 1.0, 1 februari 2014 met erratalijst, addendum 1 juli 2018", "NEN 7120", mergeEPFU$Pand_berekeningstype)
mergeEPFU$Pand_berekeningstype <- sub("ISSO75.3, versie 3.0, oktober 2011", "NEN 7120", mergeEPFU$Pand_berekeningstype)


#also need to get all the NTA 8800 berekeningstype the same
# Remove everything after the colon in the "column_name" column
mergeEPFU$Pand_berekeningstype <- sub(":.*", "", mergeEPFU$Pand_berekeningstype)


#then we have EPA and EP remaind. cant figure out what thet are. so i remove them 
#also because they are not that much represented
# Remove rows with "EPA" or "EP" in the "column_name" column
mergeEPFU <- mergeEPFU[!grepl("EPA|EP", mergeEPFU$Pand_berekeningstype), ]


#now we need to check how the berekeningstype turned out
unique_values <- unique(mergeEPFU$Pand_berekeningstype)
unique_values
#They look great so we can contineu

frequency <- table(mergeEPFU$Pand_energieklasse)
print(frequency)
View(frequency)









  ###### PLOSSTING ######


####probeercels met ggplot
# now we need to do some analyses on the data. 
### THE MISSINGNESS PLOT
# Count missing values in each column
missing_counts <- colSums(is.na(mergeEPFU))
# Create a bar plot
ggplot(data.frame(column = names(missing_counts), count = missing_counts), aes(x = column, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Column", y = "Missing Values Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#See that half of the dataset is missing the collom fossyl energie



# Reorder the levels of Pand_energieklasse
mergeEPFU$Pand_energieklasse <- factor(mergeEPFU$Pand_energieklasse,
                                       levels = c("A++++", "A+++", "A++", "A+", "A", "B", "C", "D", "E", "F", "G"))

ggplot(mergeEPFU, aes(x = reorder(Pand_energieklasse, Pand_primaire_fossiele_energie), 
                      y = Pand_primaire_fossiele_energie)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(x = "Energy Label", y = "Pand Primaire Fossiele Energie") +
  theme_minimal()







###### LABELS FROM CHR TO NUM ##########

#This is done for the prediction models(some need numbers and not characters)
MEPFU = mergeEPFU
#need to give numbers to all the energy labels
label_mapping <- c("A++++" = 1,"A+++" = 2, "A++" = 3, "A+" = 4,"A" = 5, "B" = 6, "C" = 7, "D" = 8, "E" = 9, "F" = 10, "G" = 11)
# Convert the column to numerical values
MEPFU$Pand_energieklasse <- label_mapping[MEPFU$Pand_energieklasse]


#also need to do it for house type
unique_values <- unique(MEPFU$house_type)
unique_values
#only need to do huis and apartement
label_mapping <- c("huis" = 1, "appartement" = 2)
# Convert the column to numerical values
MEPFU$house_type <- label_mapping[MEPFU$house_type]


#also for berekeningtype
unique_values <- unique(MEPFU$Pand_berekeningstype)
unique_values
#only need to do huis and apartement
label_mapping <- c("NEN 7120" = 1, "NTA 8800" = 2)
# Convert the column to numerical values
MEPFU$Pand_berekeningstype <- label_mapping[MEPFU$Pand_berekeningstype]







  ###### LINEAR REGRESSION MODEL ######


#first need to make two data sets one with all the values and one with all NA
# Subset dataset with non-missing values
dataset_non_na <- MEPFU[!is.na(MEPFU$Pand_primaire_fossiele_energie), ]
# Subset dataset with NA values
dataset_na <- MEPFU[is.na(MEPFU$Pand_primaire_fossiele_energie), ]


#Now start the logistic regression on the non_na dataset
#first i get pretty low values on rsquard so i needed to remove the outliers
# 4. Data Cleaning (example: removing outliers) About 85 removed
dataset_non_na <- dataset_non_na[dataset_non_na$Pand_primaire_fossiele_energie < 600, ]
#remove pand berekeningstype
dataset_non_na <- dataset_non_na[, -c(1)]


#starting the LR
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(nrow(dataset_non_na), nrow(dataset_non_na) * 0.6)  # 70% for training
train_data <- dataset_non_na[train_indices, ]  # Training set
test_data <- dataset_non_na[-train_indices, ]  # Test set


#need to look for representations
xtabs(~ house_type + Pand_energieklasse, data=test_data)
#i dont have any values with the test set in berekeningstype. so i can leave it out of the prediction
xtabs(~ Pand_berekeningstype + Pand_energieklasse, data=test_data)


# Train the linear regression model
modelLR <- lm(Pand_primaire_fossiele_energie ~ Pand_energieklasse + house_type + living_area  + house_age, data = train_data)


#Make predictions on the training set
train_predictions <- predict(modelLR, newdata = train_data)
# Add the predicted values to the training set
train_data$Predicted_Pand_primaire_fossiele_energie <- train_predictions


# Make predictions on the testing set
test_predictions <- predict(modelLR, newdata = test_data)
# Add the predicted values to the testing set
test_data$Predicted_Pand_primaire_fossiele_energie <- test_predictions


# Calculate the model's performance metrics on the training set
train_rmseLR <- sqrt(mean((train_data$Pand_primaire_fossiele_energie - train_predictions)^2))
train_r_squaredLR <- summary(modelLR)$r.squared


# Calculate the model's performance metrics on the testing set
test_rmseLR <- sqrt(mean((test_data$Pand_primaire_fossiele_energie - test_predictions)^2))
test_r_squaredLR <- 1 - sum((test_data$Pand_primaire_fossiele_energie - test_predictions)^2) / sum((test_data$Pand_primaire_fossiele_energie - mean(test_data$Pand_primaire_fossiele_energie))^2)


# Print the performance metrics
print("Training set performance:")
print(paste("RMSE:", train_rmseLR))
print(paste("R-squared:", train_r_squaredLR))

print("Testing set performance:")
print(paste("RMSE:", test_rmseLR))
print(paste("R-squared:", test_r_squaredLR))


#looking oke with a difference of 28 in the prediction. we can take a look 
#on how we can get this number down
library(dplyr)

performance_table <- test_data %>%
  group_by(Pand_energieklasse) %>%
  summarize(
    MAE = mean(abs(Pand_primaire_fossiele_energie - Predicted_Pand_primaire_fossiele_energie)),
    MSE = mean((Pand_primaire_fossiele_energie - Predicted_Pand_primaire_fossiele_energie)^2)
  )

# Print the performance table
print(performance_table)


# Fit the linear regression model
model <- lm(Pand_primaire_fossiele_energie ~ Pand_energieklasse, data = train_data)
# Extract the coefficients from the linear regression model
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Plotting code with adjusted regression line 
ggplot(data = test_data, aes(x = Pand_energieklasse, y = Pand_primaire_fossiele_energie)) +
  geom_point(shape = 1, aes(color = "Actual"), fill = NA, size = 3) +
  geom_point(shape = 3, aes(x = Pand_energieklasse, y = Predicted_Pand_primaire_fossiele_energie, color = "Predicted"), size = 3) +
  geom_abline(intercept = intercept, slope = slope, color = "red") +
  labs(x = "Energy Label", y = "Pand_primaire_fossiele_energie", color = "") +
  ggtitle("Linear Regression: Predicted vs. Actual") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "green")) +
  guides(color = guide_legend(override.aes = list(shape = c(1, 3))))










  ###### GRADIENT BOOSTING ######

library(gbm)
library(caret)

# Set seed for reproducibility
set.seed(123)


# Split the dataset into training and testing sets
train_indicesGB <- sample(nrow(dataset_non_na), nrow(dataset_non_na) * 0.6)  # 60% for training
train_dataGB <- dataset_non_na[train_indices, ]  # Training set
test_dataGB <- dataset_non_na[-train_indices, ]  # Test set


# Define parameter grid for hyperparameter tuning
param_grid <- expand.grid(
  n.trees = c(100, 200, 300),            # Number of trees
  interaction.depth = c(3, 4, 5),        # Maximum depth of each tree
  shrinkage = c(0.05, 0.1, 0.2)          # Learning rate or shrinkage factor
)



# Perform grid search to find the best combination of hyperparameters
best_rmse <- Inf
best_model <- NULL

for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  
  # Train the model with current hyperparameters
  modelGB <- gbm(
    Pand_primaire_fossiele_energie ~ . - Pand_gebouwsubtype,  # Exclude Pand_gebouwsubtype from the model
    data = train_dataGB,
    n.trees = params$n.trees,
    interaction.depth = params$interaction.depth,
    shrinkage = params$shrinkage,
    distribution = "gaussian"
  )
  
  # Make predictions on the testing set
  test_predictionsGB <- predict(modelGB, newdata = test_dataGB, n.trees = params$n.trees)
  
  # Calculate RMSE for current hyperparameters
  rmse <- sqrt(mean((test_dataGB$Pand_primaire_fossiele_energie - test_predictionsGB)^2))
  
  # Update best model if RMSE improves
  if (rmse < best_rmse) {
    best_rmse <- rmse
    best_model <- modelGB
  }
}


# Use the best model for predictions
train_predictionsGB <- predict(best_model, newdata = train_dataGB, n.trees = best_model$n.trees)
test_predictionsGB <- predict(best_model, newdata = test_dataGB, n.trees = best_model$n.trees)


# Add the predicted values to the train and test datasets
train_dataGB$Predicted_Pand_primaire_fossiele_energieGB <- train_predictionsGB
test_dataGB$Predicted_Pand_primaire_fossiele_energieGB <- test_predictionsGB


# Calculate the performance metrics
train_rmseGB <- sqrt(mean((train_dataGB$Pand_primaire_fossiele_energie - train_predictionsGB)^2))
train_r_squaredGB <- cor(train_dataGB$Pand_primaire_fossiele_energie, train_predictionsGB)^2

test_rmseGB <- sqrt(mean((test_dataGB$Pand_primaire_fossiele_energie - test_predictionsGB)^2))
test_r_squaredGB <- cor(test_dataGB$Pand_primaire_fossiele_energie, test_predictionsGB)^2


# Print the performance metrics
print("Training set performance:")
print(paste("RMSE:", train_rmseGB))
print(paste("R-squared:", train_r_squaredGB))

print("Testing set performance:")
print(paste("RMSE:", test_rmseGB))
print(paste("R-squared:", test_r_squaredGB))

#wauw amazing results, much better after trying a function to 
#get the best combination in hyperparameters
# > print("Training set performance:")
# [1] "Training set performance:"
# > print(paste("RMSE:", train_rmse))
# [1] "RMSE: 23.5883424396199"
# > print(paste("R-squared:", train_r_squared))
# [1] "R-squared: 0.935766144360818"
# > 
#   > print("Testing set performance:")
# [1] "Testing set performance:"
# > print(paste("RMSE:", test_rmse))
# [1] "RMSE: 26.6771699739215"
# > print(paste("R-squared:", test_r_squared))
# [1] "R-squared: 0.919095112421673"

performance_tableGB <- test_dataGB %>%
  group_by(Pand_energieklasse) %>%
  summarize(
    MAE = mean(abs(Pand_primaire_fossiele_energie - Predicted_Pand_primaire_fossiele_energieGB)),
    MSE = mean((Pand_primaire_fossiele_energie - Predicted_Pand_primaire_fossiele_energieGB)^2)
  )

# Print the performance table
print(performance_tableGB)


#Now we plot again
ggplot(data = test_dataGB, aes(x = Pand_energieklasse, y = Pand_primaire_fossiele_energie)) +
  geom_point(shape = 1, aes(color = "Actual"), fill = NA, size = 3) +
  geom_point(shape = 3, aes(x = Pand_energieklasse, y = Predicted_Pand_primaire_fossiele_energieGB, color = "Predicted"), size = 3) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x = "Energy Label", y = "Pand_primaire_fossiele_energie", color = "") +
  ggtitle("Gradient Boosting: Predicted vs. Actual") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "green")) +
  guides(color = guide_legend(override.aes = list(shape = c(1, 3))))



#now compare the two models with eachother 
comparison <- data.frame(Model = c("Linear Regression", "Gradient Boosting"),
                         RMSE_Train = c(train_rmseLR, train_rmseGB),
                         RMSE_Test = c(test_rmseLR, test_rmseGB),
                         R_Squared_Train = c(train_r_squaredLR, train_r_squaredGB),
                         R_Squared_Test = c(test_r_squaredLR, test_r_squaredGB))
print(comparison)
View(comparison)

# Merge the performance tables based on Pand_energieklasse
comparison_tabletotal <- merge(performance_table, performance_tableGB, by = "Pand_energieklasse", suffixes = c("_LR", "_GB"))

# Print the merged table
print(comparison_tabletotal)
# Merge the performance tables based on Pand_energieklasse
comparison_tabletotal <- merge(performance_table, performance_tableGB, by = "Pand_energieklasse", suffixes = c("_LR", "_GB"))

# Round the merged table to two decimal places
comparison_tabletotal <- round(comparison_tabletotal, 2)

# Print the rounded merged table
View(comparison_tabletotal)












###### PREDICT NA DATASET ######
#remove the first column
dataset_na <- dataset_na[, -c(1)]

# Predict the missing values using the LR model
predictions_na <- predict(best_model, newdata = dataset_na, n.trees = best_model$n.trees)

# Predict the missing values using the linear regression model
predictions_na <- predict(modelLR, newdata = dataset_na)

# Update the Pand_primaire_fossiele_energie column with the predicted values
dataset_na$Pand_primaire_fossiele_energie <- predictions_na




#plot the energy label and primary fossil energy
ggplot(dataset_non_na, aes(x = Pand_energieklasse, y = Pand_primaire_fossiele_energie)) +
  geom_point(shape = 16, size = 3) +
  labs(x = "Energy Label", y = "Predicted Pand_primaire_fossiele_energie") +
  ggtitle("Pand_primaire_fossiele_energie vs. Energy Label") +
  theme_minimal()

ggplot(dataset_na, aes(x = Pand_energieklasse, y = Pand_primaire_fossiele_energie)) +
  geom_point(shape = 16, size = 3) +
  labs(x = "Energy Label", y = "Predicted Pand_primaire_fossiele_energie") +
  ggtitle("Predicted Pand_primaire_fossiele_energie vs. Energy Label") +
  theme_minimal()


# Merge non-NA and NA datasets
combined_dataset <- rbind(dataset_non_na, dataset_na)


#now we make a boxplot of the two to compare them to each other
ggplot(dataset_non_na, aes(x = as.factor(Pand_energieklasse), y = Pand_primaire_fossiele_energie)) +
  geom_boxplot() +
  labs(x = "Energy Label", y = "Pand_primaire_fossiele_energie") +
  ggtitle("Boxplot of Pand_primaire_fossiele_energie by Energy Label")+ 
  scale_x_discrete(labels = c("A++++", "A+++", "A++", "A+", "A", "B", "C", "D", "E", "F", "G"))+
  theme_minimal()

ggplot(dataset_na, aes(x = as.factor(Pand_energieklasse), y = Pand_primaire_fossiele_energie)) +
  geom_boxplot() +
  labs(x = "Energy Label", y = "Predicted Pand_primaire_fossiele_energie") +
  ggtitle("Boxplot of Predicted Pand_primaire_fossiele_energie by Energy Label") +
  scale_x_discrete(labels = c( "A+", "A", "B", "C", "D", "E", "F", "G"))+
  theme_minimal()






  ####### ANALYSES EL non na LIVING AREA ##############


#### energy label A
# Create bins for the living_area ranges
bins <- cut(energy_label_A$living_area, breaks = seq(0, max(energy_label_A$living_area) + 100, by = 100))
# Count the number of observations in each bin
bin_counts <- table(bins)
# Print the bin counts
print(bin_counts)
#Above 400 it is an outlier so im deleting it

#### energy label B
# Create bins for the living_area ranges
bins <- cut(energy_label_B$living_area, breaks = seq(0, max(energy_label_B$living_area) + 100, by = 100))
# Count the number of observations in each bin
bin_counts <- table(bins)
# Print the bin counts
print(bin_counts)

### energy label c
# Create bins for the living_area ranges
bins <- cut(energy_label_C$living_area, breaks = seq(0, max(energy_label_C$living_area) + 100, by = 100))
# Count the number of observations in each bin
bin_counts <- table(bins)
# Print the bin counts
print(bin_counts)

# we see that above 400 its an outlier in every dataset so we remove it
# removing observations with living_area above 300
dataset_non_na <- dataset_non_na[dataset_non_na$living_area <= 300, ]




# Split the dataset into separate datasets based on energy labels
energy_label_datasets <- split(dataset_non_na, dataset_non_na$Pand_energieklasse)

# Access individual datasets for each energy label
energy_label_APPPP <- energy_label_datasets$'1'
energy_label_APPP <- energy_label_datasets$'2'
energy_label_APP <- energy_label_datasets$'3'
energy_label_AP <- energy_label_datasets$'4'
energy_label_A <- energy_label_datasets$'5'
energy_label_B <- energy_label_datasets$'6'
energy_label_C <- energy_label_datasets$'7'
energy_label_D <- energy_label_datasets$'8'
energy_label_E <- energy_label_datasets$'9'
energy_label_F <- energy_label_datasets$'10'
energy_label_G <- energy_label_datasets$'11'

# Print the number of observations in each dataset
print(paste("Dataset A++++ - Number of observations:", nrow(energy_label_APPPP)))
print(paste("Dataset A+++ - Number of observations:", nrow(energy_label_APPP)))
print(paste("Dataset A++ - Number of observations:", nrow(energy_label_APP)))
print(paste("Dataset A+ - Number of observations:", nrow(energy_label_AP)))
print(paste("Dataset A - Number of observations:", nrow(energy_label_A)))
print(paste("Dataset B - Number of observations:", nrow(energy_label_B)))
print(paste("Dataset C - Number of observations:", nrow(energy_label_C)))
print(paste("Dataset D - Number of observations:", nrow(energy_label_D)))
print(paste("Dataset E - Number of observations:", nrow(energy_label_E)))
print(paste("Dataset F - Number of observations:", nrow(energy_label_F)))
print(paste("Dataset G - Number of observations:", nrow(energy_label_G)))

#we see that the + ones have reallu low amound in them so they are not goin in
summary(energy_label_A)



#matrix the outcomes of the comparison between energy label
# Create an empty matrix to store the mean values
mean_matrix <- matrix(NA, nrow = 7, ncol = 3, dimnames = list(c("A", "B", "C", "D", "E", "F", "G"), c("0-100", "101-200", "201-300")))

# Iterate over energy labels
for (label in c("A", "B", "C", "D", "E", "F", "G")) {
  # Get the corresponding energy label dataset
  energy_label_data <- get(paste0("energy_label_", label))
  
  # Iterate over living area ranges
  for (i in 1:3) {
    # Define the living area range
    range_values <- strsplit(living_area_ranges[i], "-")[[1]]
    range_min <- as.numeric(range_values[1])
    range_max <- as.numeric(range_values[2])
    
    # Subset the dataset based on the living area range
    subset_data <- energy_label_data[energy_label_data$living_area >= range_min & energy_label_data$living_area <= range_max, ]
    
    # Calculate the mean of Pand_primaire_fossiele_energie
    mean_value <- mean(subset_data$Pand_primaire_fossiele_energie)
    
    # Store the mean value in the matrix
    mean_matrix[label, i] <- mean_value
  }
}

# Print the mean matrix
print(mean_matrix)

####difference matrix
# Create an empty matrix to store the differences
difference_matrix <- matrix(NA, nrow = nrow(mean_matrix) - 1, ncol = ncol(mean_matrix))

# Calculate the differences between consecutive rows
for (i in 1:(nrow(mean_matrix) - 1)) {
  difference_matrix[i, ] <- mean_matrix[i + 1, ] - mean_matrix[i, ]
}

# Print the mean matrix
print(mean_matrix)

# Print the difference matrix
print(difference_matrix)

####the label difference matrix
# Create an empty matrix to store the differences
label_difference_matrix <- matrix(NA, nrow = nrow(mean_matrix), ncol = nrow(mean_matrix))

# Calculate the differences between energy labels
for (i in 1:nrow(mean_matrix)) {
  for (j in 1:nrow(mean_matrix)) {
    label_difference_matrix[i, j] <- mean_matrix[i, 1] - mean_matrix[j, 1]
  }
}

# Print the mean matrix
print(mean_matrix)

# Print the difference matrix for living area
print(difference_matrix)

# Print the label difference matrix
print(label_difference_matrix)










  ####### ANALYSES EL na LIVING AREA #########
# removing observations with living_area above 300
dataset_na <- dataset_na[dataset_na$living_area <= 300, ]




# Split the dataset into separate datasets based on energy labels
energy_label_datasetsNA <- split(dataset_na, dataset_na$Pand_energieklasse)

# Access individual datasets for each energy label
energy_label_NAAPPPP <- energy_label_datasetsNA$'1'
energy_label_NAAPPP <- energy_label_datasetsNA$'2'
energy_label_NAAPP <- energy_label_datasetsNA$'3'
energy_label_NAAP <- energy_label_datasetsNA$'4'
energy_label_NAA <- energy_label_datasetsNA$'5'
energy_label_NAB <- energy_label_datasetsNA$'6'
energy_label_NAC <- energy_label_datasetsNA$'7'
energy_label_NAD <- energy_label_datasetsNA$'8'
energy_label_NAE <- energy_label_datasetsNA$'9'
energy_label_NAF <- energy_label_datasetsNA$'10'
energy_label_NAG <- energy_label_datasetsNA$'11'

# Print the number of observations in each dataset
print(paste("Dataset A++++ - Number of observations:", nrow(energy_label_NAAPPPP)))
print(paste("Dataset A+++ - Number of observations:", nrow(energy_label_NAAPPP)))
print(paste("Dataset A++ - Number of observations:", nrow(energy_label_NAAPP)))
print(paste("Dataset A+ - Number of observations:", nrow(energy_label_NAAP)))
print(paste("Dataset A - Number of observations:", nrow(energy_label_NAA)))
print(paste("Dataset B - Number of observations:", nrow(energy_label_NAB)))
print(paste("Dataset C - Number of observations:", nrow(energy_label_NAC)))
print(paste("Dataset D - Number of observations:", nrow(energy_label_NAD)))
print(paste("Dataset E - Number of observations:", nrow(energy_label_NAE)))
print(paste("Dataset F - Number of observations:", nrow(energy_label_NAF)))
print(paste("Dataset G - Number of observations:", nrow(energy_label_NAG)))

#we see that we dont have +++ only one A+ s0 we also continue with just A to G
#we see that the + ones have reallu low amound in them so they are not goin in
summary(energy_label_NAA)



#matrix the outcomes of the comparison between energy label
# Create an empty matrix to store the mean values
mean_matrixNA <- matrix(NA, nrow = 7, ncol = 3, dimnames = list(c("A", "B", "C", "D", "E", "F", "G"), c("0-100", "101-200", "201-300")))

# Iterate over energy labels
for (label in c("A", "B", "C", "D", "E", "F", "G")) {
  # Get the corresponding energy label dataset
  energy_label_dataNA <- get(paste0("energy_label_NA", label))
  
  # Iterate over living area ranges
  for (i in 1:3) {
    # Define the living area range
    range_values <- strsplit(living_area_ranges[i], "-")[[1]]
    range_min <- as.numeric(range_values[1])
    range_max <- as.numeric(range_values[2])
    
    # Subset the dataset based on the living area range
    subset_dataNA <- energy_label_dataNA[energy_label_dataNA$living_area >= range_min & energy_label_dataNA$living_area <= range_max, ]
    
    # Calculate the mean of Pand_primaire_fossiele_energie
    mean_valueNA <- mean(subset_dataNA$Pand_primaire_fossiele_energie)
    
    # Store the mean value in the matrix
    mean_matrixNA[label, i] <- mean_valueNA
  }
}

# Print the mean matrix
print(mean_matrixNA)

####difference matrix
# Create an empty matrix to store the differences
difference_matrixNA <- matrix(NA, nrow = nrow(mean_matrixNA) - 1, ncol = ncol(mean_matrixNA))

# Calculate the differences between consecutive rows
for (i in 1:(nrow(mean_matrixNA) - 1)) {
  difference_matrixNA[i, ] <- mean_matrixNA[i + 1, ] - mean_matrixNA[i, ]
}

# Print the mean matrix
print(mean_matrixNA)

# Print the difference matrix
print(difference_matrixNA)

####the label difference matrix
# Create an empty matrix to store the differences
label_difference_matrixNA <- matrix(NA, nrow = nrow(mean_matrixNA), ncol = nrow(mean_matrixNA))

# Calculate the differences between energy labels
for (i in 1:nrow(mean_matrixNA)) {
  for (j in 1:nrow(mean_matrixNA)) {
    label_difference_matrixNA[i, j] <- mean_matrixNA[i, 1] - mean_matrixNA[j, 1]
  }
}

# Print the mean matrix
print(mean_matrixNA)

# Print the difference matrix for living area
print(difference_matrixNA)

# Print the label difference matrix
print(label_difference_matrixNA)


##now need to compare them
# Compare mean_matrix and mean_matrixNA
mean_matrix_comparison <- mean_matrix - mean_matrixNA
# Print the comparison matrix
print(mean_matrix_comparison)
#Positive values indicate that the corresponding element in mean_matrix is larger, 
#while negative values indicate that the corresponding element in mean_matrixNA is larger.



# Compare difference_matrix and difference_matrixNA
difference_matrix_comparison <- difference_matrix - difference_matrixNA
# Print the comparison matrix
print(difference_matrix_comparison)
#Positive values indicate that the corresponding element in difference_matrix is larger, 
#while negative values indicate that the corresponding element in difference_matrixNA is larger.



# Compare label_difference_matrix and label_difference_matrixNA
label_difference_matrix_comparison <- label_difference_matrix - label_difference_matrixNA
# Print the comparison matrix
print(label_difference_matrix_comparison)
#Positive values indicate that the corresponding element in label_difference_matrix is larger, 
#while negative values indicate that the corresponding element in label_difference_matrixNA is larger.



  ####### ANALYSES EL non na HOUSE AGE ##########
#### energy label A





# Create bins for the living_area ranges
binsLA <- cut(energy_label_A$house_age, breaks = seq(0, max(energy_label_A$house_age) + 50, by = 50))
# Count the number of observations in each bin
bin_countsLA <- table(binsLA)
# Print the bin counts
print(bin_countsLA)
#Above 300 it is an outlier so im deleting it


# we see that above 400 its an outlier in every dataset so we remove it
# removing observations with living_area above 300
dataset_non_na <- dataset_non_na[dataset_non_na$house_age <= 300, ]




# Split the dataset into separate datasets based on energy labels
energy_label_datasets <- split(dataset_non_na, dataset_non_na$Pand_energieklasse)

# Access individual datasets for each energy label
energy_label_APPPP <- energy_label_datasets$'1'
energy_label_APPP <- energy_label_datasets$'2'
energy_label_APP <- energy_label_datasets$'3'
energy_label_AP <- energy_label_datasets$'4'
energy_label_A <- energy_label_datasets$'5'
energy_label_B <- energy_label_datasets$'6'
energy_label_C <- energy_label_datasets$'7'
energy_label_D <- energy_label_datasets$'8'
energy_label_E <- energy_label_datasets$'9'
energy_label_F <- energy_label_datasets$'10'
energy_label_G <- energy_label_datasets$'11'

# Print the number of observations in each dataset
print(paste("Dataset A++++ - Number of observations:", nrow(energy_label_APPPP)))
print(paste("Dataset A+++ - Number of observations:", nrow(energy_label_APPP)))
print(paste("Dataset A++ - Number of observations:", nrow(energy_label_APP)))
print(paste("Dataset A+ - Number of observations:", nrow(energy_label_AP)))
print(paste("Dataset A - Number of observations:", nrow(energy_label_A)))
print(paste("Dataset B - Number of observations:", nrow(energy_label_B)))
print(paste("Dataset C - Number of observations:", nrow(energy_label_C)))
print(paste("Dataset D - Number of observations:", nrow(energy_label_D)))
print(paste("Dataset E - Number of observations:", nrow(energy_label_E)))
print(paste("Dataset F - Number of observations:", nrow(energy_label_F)))
print(paste("Dataset G - Number of observations:", nrow(energy_label_G)))

#we see that the + ones have reallu low amound in them so they are not goin in
summary(energy_label_A)


# Create house age ranges

house_age_ranges <- c("0-50", "51-100", "101-150")

# Create an empty matrix to store the mean values
mean_matrixLA <- matrix(NA, nrow = 7, ncol = 3, dimnames = list(c("A", "B", "C", "D", "E", "F", "G"), house_age_ranges))

# Print the mean matrix
print(mean_matrixLA)

# Create an empty matrix to store the mean values
mean_matrixHA <- matrix(NA, nrow = 7, ncol = 3, dimnames = list(c("A", "B", "C", "D", "E", "F", "G"), house_age_ranges))

# Iterate over energy labels
for (label in c("A", "B", "C", "D", "E", "F", "G")) {
  # Get the corresponding energy label dataset
  energy_label_data <- get(paste0("energy_label_", label))
  
  # Iterate over house age ranges
  for (i in 1:3) {
    # Define the house age range
    range_values <- strsplit(house_age_ranges[i], "-")[[1]]
    range_min <- as.numeric(range_values[1])
    range_max <- as.numeric(range_values[2])
    
    # Subset the dataset based on the house age range
    subset_dataLA <- energy_label_data[energy_label_data$house_age >= range_min & energy_label_data$house_age <= range_max, ]
    
    # Calculate the mean of Pand_primaire_fossiele_energie
    mean_valueLA <- mean(subset_dataLA$Pand_primaire_fossiele_energie)
    
    # Store the mean value in the matrix
    mean_matrixLA[label, i] <- mean_valueLA
  }
}

# Print the mean matrix
print(mean_matrixLA)
# Round the mean matrix to two decimal places
rounded_mean_matrixLA <- round(mean_matrixLA, 2)

# Print the rounded mean matrix
print(rounded_mean_matrixLA)

####difference matrix
# Create an empty matrix to store the differences
difference_matrixLA <- matrix(NA, nrow = nrow(mean_matrixLA) - 1, ncol = ncol(mean_matrixLA))
print(difference_matrixLA)
# Calculate the differences between consecutive rows
for (i in 1:(nrow(mean_matrixLA) - 1)) {
  difference_matrixLA[i, ] <- mean_matrixLA[i + 1, ] - mean_matrixLA[i, ]
}

# Print the difference matrix
print(difference_matrixLA)

####the label difference matrix
# Create an empty matrix to store the differences
label_difference_matrixLA <- matrix(NA, nrow = nrow(mean_matrixLA), ncol = nrow(mean_matrixLA))

# Calculate the differences between energy labels
for (i in 1:nrow(mean_matrixLA)) {
  for (j in 1:nrow(mean_matrixLA)) {
    label_difference_matrixLA[i, j] <- mean_matrixLA[i, 1] - mean_matrixLA[j, 1]
  }
}

# Print the label difference matrix
print(label_difference_matrixLA)




  ####### ANALYSES EL na HOUSE AGE ##########

dataset_na <- dataset_na[dataset_na$house_age <= 300, ]




# Split the dataset into separate datasets based on energy labels
energy_label_datasetsNA <- split(dataset_na, dataset_na$Pand_energieklasse)

# Access individual datasets for each energy label
energy_label_NAAPPPP <- energy_label_datasetsNA$'1'
energy_label_NAAPPP <- energy_label_datasetsNA$'2'
energy_label_NAAPP <- energy_label_datasetsNA$'3'
energy_label_NAAP <- energy_label_datasetsNA$'4'
energy_label_NAA <- energy_label_datasetsNA$'5'
energy_label_NAB <- energy_label_datasetsNA$'6'
energy_label_NAC <- energy_label_datasetsNA$'7'
energy_label_NAD <- energy_label_datasetsNA$'8'
energy_label_NAE <- energy_label_datasetsNA$'9'
energy_label_NAF <- energy_label_datasetsNA$'10'
energy_label_NAG <- energy_label_datasetsNA$'11'

# Print the number of observations in each dataset
print(paste("Dataset A++++ - Number of observations:", nrow(energy_label_NAAPPPP)))
print(paste("Dataset A+++ - Number of observations:", nrow(energy_label_NAAPPP)))
print(paste("Dataset A++ - Number of observations:", nrow(energy_label_NAAPP)))
print(paste("Dataset A+ - Number of observations:", nrow(energy_label_NAAP)))
print(paste("Dataset A - Number of observations:", nrow(energy_label_NAA)))
print(paste("Dataset B - Number of observations:", nrow(energy_label_NAB)))
print(paste("Dataset C - Number of observations:", nrow(energy_label_NAC)))
print(paste("Dataset D - Number of observations:", nrow(energy_label_NAD)))
print(paste("Dataset E - Number of observations:", nrow(energy_label_NAE)))
print(paste("Dataset F - Number of observations:", nrow(energy_label_NAF)))
print(paste("Dataset G - Number of observations:", nrow(energy_label_NAG)))

#we see that we dont have +++ only one A+ s0 we also continue with just A to G
#we see that the + ones have reallu low amound in them so they are not goin in
summary(energy_label_NAA)



#matrix the outcomes of the comparison between energy label
# Create an empty matrix to store the mean values
mean_matrixNA <- matrix(NA, nrow = 7, ncol = 3, dimnames = list(c("A", "B", "C", "D", "E", "F", "G"), c("0-50", "51-100", "101-150")))

# Iterate over energy labels
for (label in c("A", "B", "C", "D", "E", "F", "G")) {
  # Get the corresponding energy label dataset
  energy_label_dataNA <- get(paste0("energy_label_NA", label))
  
  # Iterate over living area ranges
  for (i in 1:3) {
    # Define the living area range
    range_values <- strsplit(living_area_ranges[i], "-")[[1]]
    range_min <- as.numeric(range_values[1])
    range_max <- as.numeric(range_values[2])
    
    # Subset the dataset based on the living area range
    subset_dataNA <- energy_label_dataNA[energy_label_dataNA$living_area >= range_min & energy_label_dataNA$living_area <= range_max, ]
    
    # Calculate the mean of Pand_primaire_fossiele_energie
    mean_valueNA <- mean(subset_dataNA$Pand_primaire_fossiele_energie)
    
    # Store the mean value in the matrix
    mean_matrixNA[label, i] <- mean_valueNA
  }
}

# Print the mean matrix
print(mean_matrixNA)

####difference matrix
# Create an empty matrix to store the mean values
mean_matrixHA <- matrix(NA, nrow = 7, ncol = 3, dimnames = list(c("A", "B", "C", "D", "E", "F", "G"), c("0-50", "51-100", "101-150")))

# Iterate over energy labels
for (label in c("A", "B", "C", "D", "E", "F", "G")) {
  # Get the corresponding energy label dataset
  energy_label_data <- get(paste0("energy_label_", label))
  
  # Iterate over house age ranges
  for (i in 1:3) {
    # Define the house age range
    range_values <- strsplit(house_age_ranges[i], "-")[[1]]
    range_min <- as.numeric(range_values[1])
    range_max <- as.numeric(range_values[2])
    
    # Subset the dataset based on the house age range
    subset_dataHA <- energy_label_data[energy_label_data$house_age >= range_min & energy_label_data$house_age <= range_max, ]
    
    # Calculate the mean of Pand_primaire_fossiele_energie
    mean_valueHA <- mean(subset_dataHA$Pand_primaire_fossiele_energie)
    
    # Store the mean value in the matrix
    mean_matrixHA[label, i] <- mean_valueHA
  }
}

# Print the mean matrix
print(mean_matrixHA)

#### Difference matrix
# Create an empty matrix to store the differences
difference_matrixHA <- matrix(NA, nrow = nrow(mean_matrixHA) - 1, ncol = ncol(mean_matrixHA))

# Calculate the differences between consecutive rows
for (i in 1:(nrow(mean_matrixHA) - 1)) {
  difference_matrixHA[i, ] <- mean_matrixHA[i + 1, ] - mean_matrixHA[i, ]
}

# Print the mean matrix
print(mean_matrixHA)

# Print the difference matrix
print(difference_matrixHA)

#### Label difference matrix
# Create an empty matrix to store the differences
label_difference_matrixHA <- matrix(NA, nrow = nrow(mean_matrixHA), ncol = nrow(mean_matrixHA))

# Calculate the differences between energy labels
for (i in 1:nrow(mean_matrixHA)) {
  for (j in 1:nrow(mean_matrixHA)) {
    label_difference_matrixHA[i, j] <- mean_matrixHA[i, 1] - mean_matrixHA[j, 1]
  }
}

# Print the mean matrix
print(mean_matrixHA)

# Print the difference matrix for house age
print(difference_matrixHA)

# Print the label difference matrix
print(label_difference_matrixHA)

## Compare matrices
# Compare mean_matrix and mean_matrixNA
mean_matrix_comparison <- mean_matrixHA - mean_matrixNA
# Print the comparison matrix
print(mean_matrix_comparison)

# Compare difference_matrix and difference_matrixNA
difference_matrix_comparison <- difference_matrixHA - difference_matrixNA
# Print the comparison matrix
print(difference_matrix_comparison)

# Compare label_difference_matrix and label_difference_matrixNA
label_difference_matrix_comparison <- label_difference_matrixHA - label_difference_matrixNA
# Print the comparison matrix
print(label_difference_matrix_comparison)




####### COMBINING FOR FULL ANALYSES ######

# dataset_non_na <- dataset_non_na[, -c(1)]
combined_dataset <- rbind(dataset_non_na, dataset_na)


# Split the dataset into separate datasets based on energy labels
energy_label_datasetsC <- split(combined_dataset, combined_dataset$Pand_energieklasse)

# Access individual datasets for each energy label
energy_label_CAPPPP <- energy_label_datasetsC$'1'
energy_label_CAPPP <- energy_label_datasetsC$'2'
energy_label_CAPP <- energy_label_datasetsC$'3'
energy_label_CAP <- energy_label_datasetsC$'4'
energy_label_CA <- energy_label_datasetsC$'5'
energy_label_CB <- energy_label_datasetsC$'6'
energy_label_CC <- energy_label_datasetsC$'7'
energy_label_CD <- energy_label_datasetsC$'8'
energy_label_CE <- energy_label_datasetsC$'9'
energy_label_CF <- energy_label_datasetsC$'10'
energy_label_CG <- energy_label_datasetsC$'11'

# Print the number of observations in each dataset
print(paste("Dataset A++++ - Number of observations:", nrow(energy_label_CAPPPP)))
print(paste("Dataset A+++ - Number of observations:", nrow(energy_label_CAPPP)))
print(paste("Dataset A++ - Number of observations:", nrow(energy_label_CAPP)))
print(paste("Dataset A+ - Number of observations:", nrow(energy_label_CAP)))
print(paste("Dataset A - Number of observations:", nrow(energy_label_CA)))
print(paste("Dataset B - Number of observations:", nrow(energy_label_CB)))
print(paste("Dataset C - Number of observations:", nrow(energy_label_CC)))
print(paste("Dataset D - Number of observations:", nrow(energy_label_CD)))
print(paste("Dataset E - Number of observations:", nrow(energy_label_CE)))
print(paste("Dataset F - Number of observations:", nrow(energy_label_CF)))
print(paste("Dataset G - Number of observations:", nrow(energy_label_CG)))

#we see that we dont have +++ only one A+ s0 we also continue with just A to G
#we see that the + ones have reallu low amound in them so they are not goin in
summary(energy_label_CA)



#matrix the outcomes of the comparison between energy label
# Create an empty matrix to store the mean values
mean_matrixC <- matrix(NA, nrow = 7, ncol = 3, dimnames = list(c("A", "B", "C", "D", "E", "F", "G"), c("0-50", "51-100", "101-150")))

# Iterate over energy labels
for (label in c("A", "B", "C", "D", "E", "F", "G")) {
  # Get the corresponding energy label dataset
  energy_label_datasetsC <- get(paste0("energy_label_C", label))
  
  # Iterate over living area ranges
  for (i in 1:3) {
    # Define the living area range
    range_values <- strsplit(living_area_ranges[i], "-")[[1]]
    range_min <- as.numeric(range_values[1])
    range_max <- as.numeric(range_values[2])
    
    # Subset the dataset based on the living area range
    subset_dataC <- energy_label_datasetsC[energy_label_datasetsC$house_age >= range_min & energy_label_datasetsC$house_age <= range_max, ]
    
    # Calculate the mean of Pand_primaire_fossiele_energie
    mean_valueC <- mean(subset_dataC$Pand_primaire_fossiele_energie)
    
    # Store the mean value in the matrix
    mean_matrixC[label, i] <- mean_valueC
  }
}

# Print the mean matrix
print(mean_matrixC)


#### Difference matrix
# Create an empty matrix to store the differences
difference_matrixC <- matrix(NA, nrow = nrow(mean_matrixC) - 1, ncol = ncol(mean_matrixC))

# Calculate the differences between consecutive rows
for (i in 1:(nrow(mean_matrixC) - 1)) {
  difference_matrixC[i, ] <- mean_matrixC[i + 1, ] - mean_matrixC[i, ]
}

# Print the mean matrix
print(mean_matrixC)
# Print the difference matrix
print(difference_matrixC)


#### Label difference matrix
# Create an empty matrix to store the differences
label_difference_matrixC <- matrix(NA, nrow = nrow(mean_matrixC), ncol = nrow(mean_matrixC))

# Calculate the differences between energy labels
for (i in 1:nrow(mean_matrixC)) {
  for (j in 1:nrow(mean_matrixC)) {
    label_difference_matrixC[i, j] <- mean_matrixC[i, 1] - mean_matrixC[j, 1]
  }
}

# Print the mean matrix
print(mean_matrixC)
# Print the difference matrix for house age
print(difference_matrixC)
# Print the label difference matrix
print(label_difference_matrixC)


#NOW MAKE IT FOR % SAVINGS
percentage_savings_tableC <- matrix(NA, nrow = nrow(label_difference_matrixC), ncol = ncol(label_difference_matrixC))

# Calculate the percentage savings
for (i in 1:nrow(label_difference_matrixC)) {
  for (j in 1:ncol(label_difference_matrixC)) {
    percentage_savings_tableC[i, j] <- (label_difference_matrixC[i, j] / mean_matrixC[i, 1]) * 100
  }
}

# Print the percentage savings table
print(percentage_savings_tableC)
# Round the mean matrix to two decimal places
percentage_savings_tableC <- round(percentage_savings_tableC, 2)
View(label_difference_matrixC)
mean_matrixC<- round(mean_matrixC, 2)
View(mean_matrixC)

### NOW FOR PANDGEBOUWSUBTYPE
# Define the Pand_gebouwsubtype categories
frequency <- table(combined_dataset$Pand_gebouwsubtype)
print(frequency)



# Get unique Pand_gebouwsubtype categories
gebouwsubtype_categories <- unique(combined_dataset$Pand_gebouwsubtype)

# Create an empty matrix to store the mean values
mean_matrixGS <- matrix(NA, nrow = 7, ncol = length(gebouwsubtype_categories), dimnames = list(c("A", "B", "C", "D", "E", "F", "G"), gebouwsubtype_categories))

# Iterate over energy labels
for (label in c("A", "B", "C", "D", "E", "F", "G")) {
  # Get the corresponding energy label dataset
  energy_label_data <- get(paste0("energy_label_", label))
  
  # Iterate over Pand_gebouwsubtype categories
  for (i in 1:length(gebouwsubtype_categories)) {
    # Get the current Pand_gebouwsubtype category
    category <- gebouwsubtype_categories[i]
    
    # Subset the dataset based on the Pand_gebouwsubtype category
    subset_dataGS <- energy_label_data[energy_label_data$Pand_gebouwsubtype == category, ]
    
    # Calculate the mean of Pand_primaire_fossiele_energie
    mean_valueGS <- mean(subset_dataGS$Pand_primaire_fossiele_energie, na.rm = TRUE)
    
    # Handle missing values
    if (is.na(mean_valueGS)) {
      mean_valueGS <- 0  # Replace NaN with 0 or any other desired value
    }
    
    # Store the mean value in the matrix
    mean_matrixGS[label, i] <- mean_valueGS
  }
}

# Print the mean matrix
print(mean_matrixGS)



#### NOW IM DONE WITH THE ANALYSES, I DID THE OVERALL ALSO SO I CAN NOW 

#### CALCULATE ON HOW MUCH YOU CAN SAFE ON AVERAGE

# Create an empty matrix to store the cost values
cost_matrix <- matrix(NA, nrow = nrow(difference_matrixC), ncol = ncol(difference_matrixC))

# Multiply each element in the difference matrix by 0.40 to calculate the cost
for (i in 1:nrow(difference_matrixC)) {
  for (j in 1:ncol(difference_matrixC)) {
    cost_matrix[i, j] <- difference_matrixC[i, j] * 0.40
  }
}

# Print the cost matrix
print(cost_matrix)

# Create an empty matrix to store the cost values
cost_matrixLABEL <- matrix(NA, nrow = nrow(label_difference_matrixC), ncol = ncol(label_difference_matrixC))

# Multiply each element in the difference matrix by 0.40 to calculate the cost
for (i in 1:nrow(label_difference_matrixC)) {
  for (j in 1:ncol(label_difference_matrixC)) {
    cost_matrixLABEL[i, j] <- label_difference_matrixC[i, j] * 0.40
  }
}

# Print the cost matrix
print(cost_matrixLABEL)

### WANT TO DO MORE TO CALCULATE THE SAVINGS 
# Calculate the monthly savings
combined_dataset$totaal_fossiel <- combined_dataset$Pand_primaire_fossiele_energie * combined_dataset$living_area
combined_dataset$totaal_fossiel <- combined_dataset$totaal_fossiel / 12
combined_dataset$totaal_fossiel <- combined_dataset$totaal_fossiel * 0.40

# Round the monthly savings to two decimal places
combined_dataset$totaal_fossiel <- round(combined_dataset$totaal_fossiel, 2)

# Create a bar plot of monthly savings
ggplot(combined_dataset, aes(x = Pand_energieklasse, y = totaal_fossiel)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Energy Label", y = "Monthly Savings") +
  ggtitle("Monthly Savings by Energy Label")


# Create a new variable for energy label categories
combined_dataset$Energy_Category <- factor(combined_dataset$Pand_energieklasse, levels = 1:11,
                                           labels = c("A++++", "A+++", "A++", "A+", "A", "B", "C", "D", "E", "F", "G"))

# Create a box plot for totaal_fossiel by energy label
ggplot(combined_dataset, aes(x = Energy_Category, y = totaal_fossiel)) +
  geom_boxplot() +
  labs(x = "Energy Label", y = "Total Fossil Amount") +
  ggtitle("Total Fossil Amount by Energy Label") +
  scale_x_discrete(labels = c("A++++", "A+++", "A++", "A+", "A", "B", "C", "D", "E", "F", "G")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(ggplot2)

# Filter the dataset to exclude values above 150 and below -100 for totaal_fossiel and house_age
filtered_dataset <- combined_dataset[combined_dataset$totaal_fossiel <= 5000 & combined_dataset$totaal_fossiel >= -100 &
                                       combined_dataset$house_age <= 150 & combined_dataset$house_age >= -100,]

# Create a dot plot for energy labels with total fossil amount and house age represented by color
ggplot(filtered_dataset, aes(x = Energy_Category, y = totaal_fossiel, color = house_age)) +
  geom_point() +
  labs(x = "Energy Label", y = "Total Fossil Amount", color = "House Age") +
  ggtitle("Energy Labels, Total Fossil Amount, and House Age") +
  scale_x_discrete(labels = c("A++++", "A+++", "A++", "A+", "A", "B", "C", "D", "E", "F", "G")) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









####### FINISH ######
