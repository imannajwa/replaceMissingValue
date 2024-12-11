detect_missing_values <- function(data) {
  missing_counts <- numeric(ncol(data))
  names(missing_counts) <- colnames(data)
  
  for(i in 1:ncol(data)) {
  missing_counts[i] <- sum(is.na(data[[i]]))
  }
  
  # Filter out colums with no missing value
  missing_counts <- missing_counts[missing_counts > 0]
  return(missing_counts)
}


# dataset
missing_value <- data.frame(
  A = c(1,2,3,4,5),
  B = c(2,NA,6,NA,10),
  C = c(34, 67,23,45,NA),
  D = c(NA,32, NA, 45,56), 
  E = c(NA,NA,89,100,NA)
  )

detect_missing_values(missing_value)


# Find the mean of data
find_medians <- function(data) {
  medians <- sapply(data, function(x) if(is.numeric(x)) median(x, na.rm = T) else NA)
  return(medians)
}

find_medians(missing_value)


# replace the missing values with median
replace_missing_with_median <- function(data) {
  medians <- find_medians(data)
  for(i in 1:ncol(data)) {
    if(is.numeric(data[[i]])) {
      data[[i]][is.na(data[[i]])] <- medians[i]
    }
  }
  return(data)
}



new_dataset <- replace_missing_with_median(missing_value)
new_dataset


