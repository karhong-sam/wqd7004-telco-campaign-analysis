#install.packages("readxl")
#install.packages("pryr")
#install.packages("coefplot")

#import lib
library(readxl)
library(pryr)
library(Rcpp)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(caret)
library(randomForest)
library(devtools)
library(infotheo)
library(corrplot)
library(reshape2)
library(coefplot)

set.seed(123)

# Function
data_profiling = function(x) {
  
  #visualize
  #numeric > violin plot, char > horizontal bar plot
  
  for (col_name in colnames(x)) {
    
    if (is.numeric(x[[col_name]])) {
      
      p = ggplot(x, aes(x = col_name, y = x[[col_name]], fill = col_name)) +
        geom_violin() +
        labs(x = "Column", y = "Range", title = paste("Plot of", col_name)) +
        theme_minimal()
      print(p)
      
    }
    
    else {
      counts = table(x[[col_name]])
      df_counts = data.frame(category = names(counts), count = as.numeric(counts))
      p = ggplot(df_counts, aes(x = count, y = category)) +
        geom_col(fill = "steelblue") +
        labs(x = "Count", y = "Category", title = col_name) +
        theme_minimal()
      print(p)
    }
  }  
  
}

get_upper_tri = function(cormat){
  
  # Get upper triangle of the correlation matrix
  cormat[lower.tri(cormat)]= NA
  return(cormat)
}

#import data
dataframe = read_excel("7k_dataset.xlsx")

#drop all after data
cols_to_drop = grep("AFTER$", names(dataframe), value = TRUE)
print(cols_to_drop)
dataframe[, cols_to_drop] = NULL

#data info
head(dataframe,10)
cat("Columns info:", names(dataframe), "\n")
cat("Number of columns:", ncol(dataframe), "\n")
cat("Number of rows:", nrow(dataframe), "\n")
object_size(dataframe)
str(dataframe)
summary(dataframe)

##############################################################################
# Data preprocess #
##############################################################################
#here help to check data types, distribution, weird number
data_profiling(dataframe)

#not useful (cant use to differentiate offer taker), drop
dataframe = subset(dataframe, select = -c(NATIONALITY, STATUS_BEFORE, OFFER_TAKE_UP_DT))

#data type conversion
dataframe = replace(dataframe, dataframe=='?', NA)
dataframe = replace(dataframe, dataframe=='-9999', NA)

dataframe$DATA_CHRG_BEFORE = round(as.numeric(dataframe$DATA_CHRG_BEFORE), 2)
dataframe$RLD_AMT_BEFORE = round(as.numeric(dataframe$RLD_AMT_BEFORE), 2)
dataframe$CPA_RVN_BEFORE = round(as.numeric(dataframe$CPA_RVN_BEFORE), 2)
dataframe$ARPU_BEFORE = round(as.numeric(dataframe$ARPU_BEFORE), 2)

#double check again after clean
data_profiling(dataframe)

#check na occurrence
null_counts = colSums(is.na(dataframe))
print(null_counts)
par(las =1, mar = c(5, 10, 4, 2) + 0.1)
barplot(null_counts, main = "Null Value Counts by Column", xlab = "Null Value Counts", horiz = TRUE)


#less than 10%, replace all
dataframe$ARPU_BEFORE = replace(dataframe$ARPU_BEFORE, is.na(dataframe$ARPU_BEFORE), mean(dataframe$ARPU_BEFORE, na.rm = TRUE))
dataframe$CPA_RVN_BEFORE = replace(dataframe$CPA_RVN_BEFORE, is.na(dataframe$CPA_RVN_BEFORE), mean(dataframe$CPA_RVN_BEFORE, na.rm = TRUE))
dataframe$RLD_AMT_BEFORE = replace(dataframe$RLD_AMT_BEFORE, is.na(dataframe$RLD_AMT_BEFORE), mean(dataframe$RLD_AMT_BEFORE, na.rm = TRUE))
dataframe$DATA_CHRG_BEFORE = replace(dataframe$DATA_CHRG_BEFORE, is.na(dataframe$DATA_CHRG_BEFORE), mean(dataframe$DATA_CHRG_BEFORE, na.rm = TRUE))
dataframe$AGE = replace(dataframe$AGE, is.na(dataframe$AGE), mean(dataframe$AGE, na.rm = TRUE))

dataframe$GENDER = replace(dataframe$GENDER, is.na(dataframe$GENDER), "Unspecified")

#check na occurrence again
null_counts = colSums(is.na(dataframe))
print(null_counts)

#alter state columns
print(unique(dataframe$STATE))
dataframe$STATE[dataframe$STATE == "JOHORE"] = "JOHOR"
dataframe$STATE[dataframe$STATE == "KLANG VALLEY"] = "WILAYAH PERSEKUTUAN"
dataframe$STATE[dataframe$STATE == "MALACCA"] = "MELAKA"
dataframe$STATE[dataframe$STATE == "N SEMBILAN"] = "NEGERI SEMBILAN"
dataframe$STATE[dataframe$STATE == "PULAU PINANG"] = "PENANG"
dataframe$STATE[dataframe$STATE == "SEREMBAN/MELAKA"] = "MELAKA"
print(unique(dataframe$STATE))

#one hot encode
str(dataframe)
cols_to_encode = c("GENDER", "STATE", "DATA_PURC_BEFORE", "RLD_IND_BEFORE")
one_hot = dummyVars(" ~ .", data = dataframe[, cols_to_encode])
encoded_df = data.frame(predict(one_hot, newdata = dataframe))
print(encoded_df)

# feature selection
# correlation check for non categorical and plot heatmap
non_encoded_cols = setdiff(colnames(dataframe), cols_to_encode)
test_df = dataframe[, non_encoded_cols]
test_df = subset(test_df, select = -OFFER_TAKER)

cormat = round(cor(test_df),2)
upper_tri = get_upper_tri(cormat)
melted_cormat = melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  coord_fixed()

##############################################################################
# Classification #
##############################################################################
# drop high correlation column and combine one hot encode column 
merged_df = cbind(dataframe[, non_encoded_cols], encoded_df)
final_df = subset(merged_df, select = c(-CPA_RVN_BEFORE, -ARPU_BEFORE))
print(colnames(final_df))

#assign target
final_df$target = ifelse(final_df$OFFER_TAKER == "Y", 1, 0)
final_df = subset(final_df, select = -OFFER_TAKER)
print(final_df)

# Split the data into training and testing sets
train_idx = createDataPartition(final_df$target, p = 0.8, list = FALSE)
train_data = final_df[train_idx,]
test_data = final_df[-train_idx,]

#Train the random forest classifier
classifier_RF = randomForest(x = train_data[, -ncol(train_data)],
                             y = as.factor(train_data$target),
                             ntree = 500,
                             importance = TRUE)

#Make predictions on the test data
preds = predict(classifier_RF, newdata = test_data[, -ncol(test_data)])

#Calculate the confusion matrix/ accuracy of the predictions
conf_mat = table(preds, test_data$target)
accuracy = sum(diag(conf_mat)) / sum(conf_mat)

#print output
print(classifier_RF)
print(conf_mat)
print(paste("Accuracy:", round(accuracy, 3)))

#feature important
imp = importance(classifier_RF)
print(imp)
varImpPlot(classifier_RF)


##############################################################################
# Regression #
##############################################################################
# drop category
non_cat_df = dataframe[, non_encoded_cols]
final_df = subset(non_cat_df, select = c(-CPA_RVN_BEFORE, -ARPU_BEFORE, -OFFER_TAKER))
colnames(final_df)

# Split the data into training and testing sets
train_idx = createDataPartition(final_df$DATA_CHRG_BEFORE, p = 0.8, list = FALSE)
train_data = final_df[train_idx,]
test_data = final_df[-train_idx,]

# scaling
train_data_scaled = train_data %>%
  select(-DATA_CHRG_BEFORE) %>%
  scale() %>%
  as.data.frame() %>%
  cbind(train_data$DATA_CHRG_BEFORE)

colnames(train_data_scaled)[6] = "target"

# regression
lm_model = lm(target ~ ., data = train_data_scaled)

#predict 
test_data_scaled = test_data %>%
  select(-DATA_CHRG_BEFORE) %>%
  scale() %>%
  as.data.frame() %>%
  cbind(test_data$DATA_CHRG_BEFORE)
predictions = predict(lm_model, newdata = test_data_scaled)

colnames(test_data_scaled)[6] = "target"

#result
RMSE = sqrt(mean((predictions - test_data_scaled$target)^2))
R2 = cor(predictions, test_data_scaled$target)^2
cat("RMSE:", RMSE, "\n")
cat("R-squared:", R2, "\n")

# coefficient
coefplot(lm_model)



