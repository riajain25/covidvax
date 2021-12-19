# read in the cleaned data
data = read_csv("Desktop/stat471/stat-471-fall-2021/finalproject/data/clean/vaxdata.csv")

#Check and adjust data types
sapply(data, class)
data <- data %>% mutate (SVI_CTGY= as.factor(SVI_CTGY))


# split into train and test (set seed here if applicable)
train_samples = sample(1:nrow(data), .8*nrow(data))
data_train = data %>% filter(row_number() %in% train_samples)
data_test = data %>% filter(!(row_number() %in% train_samples))

data_train <- data_train %>% mutate (SVI_CTGY= as.factor(SVI_CTGY))
data_test <- data_test %>% mutate (SVI_CTGY= as.factor(SVI_CTGY))

#Remove state and county names
data_train <- data_train %>% select(-c(Recip_State, Recip_County, FIPS))
data_test <- data_test %>% select(-c(Recip_State, Recip_County, FIPS))

#Check and adjust data types
sapply(data_test, class)
sapply(data_train, class)

# save the train and test data
write_csv(x = data_train, file = "Desktop/stat471/stat-471-fall-2021/finalproject/data/clean/data_train.csv")
write_csv(x = data_test, file = "Desktop/stat471/stat-471-fall-2021/finalproject/data/clean/data_test.csv")
