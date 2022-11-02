# Задание 1

data1 <- read.csv('lab1_e1.csv', sep = ',')
data1

class(data1)
sapply(data1, typeof)

fix_data <- function(data){
  for (name in colnames(data)) {
    values_without_gap <- gsub(" ", "", data[[name]])
    numeric_column_values <- suppressWarnings(as.numeric(values_without_gap))
    if (!(NA %in% numeric_column_values))
      data[[name]] <- numeric_column_values
  }
  return(data)
}

fixed_data <- fix_data(data1)
fixed_data
sapply(fixed_data, typeof)

# Задание 2

data2 <- get(load('lab1_e2.Rdata'))
typeof(data2)

get_id <- function(data){
  all_days <- do.call("rbind", data)
  
  # Найдем id порядочных пациентов
  frequency <- setNames(aggregate(temp ~ id, all_days, length), c("id", "temp_freq"))
  good_patients <- frequency[frequency$temp_freq == 7, ][1]
  
  # Вычислим среднюю температуру всех пациентов
  mean_temp <- setNames(aggregate(temp ~ id, all_days, mean), c("id", "mean_temp"))
  
  # Выберем только порядочных
  res <- mean_temp[mean_temp$id %in% unlist(good_patients), ]
  names(res)[1] <- "good_id"
  
  return(res)
}

res <- get_id(data2)
res