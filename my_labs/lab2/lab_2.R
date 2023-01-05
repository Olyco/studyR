# Задание 12
data1 <- read.table(file = "data1.txt")
head(data1)
colnames(data1) <- data1[1, ]
data1 <- data1[-1, ] 
head(data1)

data2 <- read.csv('data2.csv', sep = ',')
data2 <- as.data.frame(t(data2))
head(data2)
colnames(data2) <- data2[1, ]
data2 <- data2[-1, ] 
head(data2)

fix_data2 <- function(data){
  for (name in colnames(data)) {
    data[[name]] <- gsub(" ", "", data[[name]])
  }
  return(data)
}

data2 <- fix_data2(data2)

class(data1)
class(data2)

merged_df <- cbind(data1,data2)
withoutNA <- na.omit(merged_df) 
rownames(withoutNA) <- 1:nrow(withoutNA)
withoutNA[ , -1] <- as.data.frame(lapply(withoutNA[ , -1], as.numeric))

# Упорядочим столбцы
withoutNA <- withoutNA[ ,c(1,7,2,3,4,5,6,8)]

# Считаем среднее, медиану
mean_ = sapply(withoutNA[2:5], function(x) mean(x))
print(mean_)

median_ = sapply(withoutNA[6:8], function(x) median(x))
print(median_)


# Задание 27
data <- read.csv('Payment_and_value_of_Care-Hospital_1.csv', sep = ',')

child_hosp <- data[0:0]
for (i in 1:nrow(data)) {
  if (grepl("CHILD", data$Facility.Name[i])){
  #if (str_detect(data$Facility.Name[i], "CHILD")) {
    if(!(data$Facility.ID[i] %in% child_hosp$Facility.ID)){
      child_hosp <- rbind(child_hosp, data[i, ])
    }
  }
}

head(child_hosp)

unique_city_hosp_count <- as.data.frame(table(child_hosp$City))

#Создаем пустой датафрейм нужной структуры
result_df <- data.frame(State = character(),
                        County.Name = character(),
                        City = character(),
                        Num.of.Hosp = character())

for (i in 1:nrow(child_hosp)) {
  if (!(child_hosp[["City"]][i] %in% result_df[["City"]])){
    new_str <- data.frame(child_hosp[["State"]][i],
                          child_hosp[["County.Name"]][i],
                          child_hosp[["City"]][i],
                          unique_city_hosp_count[ unique_city_hosp_count$Var1 == child_hosp[["City"]][i], 2]) 
    names(new_str) <- c("State", "County.Name", "City", "Num.of.Hosp")
    result_df <- rbind(result_df, new_str)
  }
}

result_df[1:15, ]
