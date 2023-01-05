# Задание 12
data1 <- read.table(file = "data1.txt")
data2 <- read.csv('data2.csv', sep = ',')

head(data1)
colnames(data1) <- data1[1, ]
data1 <- data1[-1, ] 
head(data1)

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

merged_df <- cbind(data1,data2)
withoutNA <- na.omit(merged_df) 
rownames(withoutNA) <- 1:nrow(withoutNA)
withoutNA[ , -1] <- as.data.frame(lapply(withoutNA[ , -1], as.numeric))

# Упорядочим столбцы
withoutNA <- select(withoutNA, Sample, GenmBMatur, everything())


# Рисунок с подграфиками
# Гистограмма - 1, 2 количественные переменные
dat_long1 <- pivot_longer(withoutNA, GenmBMatur:Height, names_to = "Feature", values_to = "Values")

ggplot(dat_long1, aes(x = Values)) +
  geom_histogram(fill = "grey20", col = "white", binwidth = 5) +
  facet_wrap("Feature") +
  labs(title = "Sample histogram", x = "Feature values", y = "Count of values")

# Плотность - 1, 2 количественные переменных
ggplot(dat_long1, aes(x = Values)) + geom_density(alpha = 0.2, fill = "yellow") +
  facet_wrap("Feature") +
  labs(title = "Sample density", x = "Feature values", y = "Density")

# Гистограмма - 3, 4 количественные переменные
dat_long2 <- pivot_longer(withoutNA, Protein:Oil, names_to = "Feature", values_to = "Values")

ggplot(dat_long2, aes(x = Values)) +
  geom_histogram(fill = "grey20", col = "white", binwidth=1) +
  facet_wrap("Feature") +
  labs(title = "Sample histogram", x = "Feature values", y = "Count of values")

# Плотность - 3, 4 количественные переменных
ggplot(dat_long2, aes(x = Values)) + geom_density(alpha = 0.2, fill = "yellow") +
  facet_wrap("Feature") +
  labs(title = "Sample density", x = "Feature values", y = "Density")


# Для качественных переменных график с разными цветами
withoutNA$GrowthType <- as.factor(withoutNA$GrowthType)
ggplot(withoutNA, aes(x = MaturType, fill = GrowthType)) +
  geom_histogram(col = "white", binwidth = 1) + 
  labs(title = "Distribution GrowthType for each MaturType", x = "MaturType values", y = "Count of MaturType values")


## Гистограмма с плотностью
ggplot(withoutNA, aes(x = Oil)) +
  geom_histogram(aes(y = ..density..), alpha = 0.9, fill = "grey20", col = "white", binwidth = 0.5) +
  geom_density(alpha = 0.2, fill = "yellow") +
  labs(title = "Oil sample histogram, density", x = "Oil values", y = "Density values")


# Боксплот для количественной переменной
ggplot(withoutNA, aes(y = Oil)) + geom_boxplot(fill='#de3163', color="black") +
  labs(title = "Oil sample boxplot", y = "Oil values")


# Задание 3
data <- get(load('trades.Rdata'))
data <- bind_rows(data)[ , -3]
data <- data[str_detect(data$indic_et, "Imports in|Exports in"), ]
dat_wide <- pivot_wider(data, names_from = indic_et, values_from = values)
colnames(dat_wide)

group_sum <- aggregate(dat_wide[[4]], list(dat_wide$time), sum)
names(group_sum) <- c("Time", "Sum.Exp")

# [min_row_num, min_col_num]
min_exp_ind <- which(group_sum == min(group_sum[["Sum.Exp"]]), arr.ind = TRUE)
same_aes <- group_sum[-min_exp_ind, ]

ggplot(group_sum, aes(x = Time, y = Sum.Exp)) + geom_line() + geom_point() +
  geom_text(data = same_aes, aes(x = Time, y = Sum.Exp, label = Sum.Exp),
            vjust = 0, hjust=1, nudge_x = 100, nudge_y = 33000, size = 3.5) +
  geom_point(data = group_sum, aes(x = group_sum[min_exp_ind[1,1], min_exp_ind[1,2] - 1],
                                   y = group_sum[min_exp_ind[1,1], min_exp_ind[1,2]]),
             size = 8, shape = "*", color = "red") +
  geom_text(data = group_sum[min_exp_ind[1,1], ], aes(x = Time, y = Sum.Exp, label = Sum.Exp),
            color = "red", size = 3.5, vjust = 0, hjust = 1, nudge_x = 100, nudge_y = -33000, check_overlap = TRUE) +
  labs(title = "Export changes by years", x = "Year", y = "Total exports for different product groups")

