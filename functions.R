# This R script store the functions

library(ggplot2)
library(gridExtra)

# Function to perform data profiling on a data frame
# Input: x - data frame
# Output: Prints plots for numeric columns and categorical columns
data_profiling = function(x) {
  
  # Iterate over column names
  for (col_name in colnames(x)) {
    
    # Check if column is numeric
    if (is.numeric(x[[col_name]])) {
      
      # Generate violin plot for numeric columns
      p = ggplot(x, aes(x = col_name, y = x[[col_name]], fill = col_name)) +
        geom_violin() +
        labs(x = "Column", y = "Range", title = paste("Violin Plot of", col_name)) +
        theme_minimal()
      print(p)
      
    }
    
    # Column is not numeric (assumed categorical)
    else {
      # Calculate counts for each category
      counts = table(x[[col_name]])
      
      # Create a data frame for plotting
      df_counts = data.frame(category = names(counts), count = as.numeric(counts))
      
      # Generate bar plot for categorical columns
      p = ggplot(df_counts, aes(x = count, y = category)) +
        geom_col(fill = "steelblue") +
        labs(x = "Count", y = "Category", title = paste("Bar Plot of", col_name)) +
        theme_minimal()
      print(p)
    }
  }  
}

# Function to extract the upper triangle of a correlation matrix
# Input: cormat - correlation matrix
# Output: upper triangle of the correlation matrix with lower triangle set to NA
get_upper_tri = function(cormat){
  
  # Set lower triangle of the correlation matrix to NA
  cormat[lower.tri(cormat)] <- NA
  
  # Return the modified correlation matrix
  return(cormat)
}