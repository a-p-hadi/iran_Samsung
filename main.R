
# 1
x<-read.csv("F:\\Learning\\Data Science (19)\\24 R (3)\\Project\\iran_Samsung\\Samdata.csv",header=T)
x

# 2
names(x)
head(x)
str(x)
summary(x)

# 3
# names(x)[16] = "Action"
names(x)[names(x)=="Repair_Action_Desc"]<-"Action"

# 4
df <- data.frame(x)
df[df==""] <- NA
head(df)
# s <- sum(is.na(df$Product_Date))
# t <- nrow(df)
# naPercent <- (s/t)*100
# naPercent
sum(is.na(df$Product_Date)) # 2412
colMeans(is.na(df))*100   # 76.9 %
df <- df[, -which(names(df) == "Product_Date")] # Delete Column
dim(df)

# 5
duration_time <- df$TAT02 - df$TAT01
duration_time
mean_start <- mean(df$TAT01)
mean_duration <- mean(duration_time)
mean_start       # 1.3 day
mean_duration    # 5.4 day

# 6
s <- sum(duplicated(df$Serial_No))
t <- nrow(df)
return_index <- (s/(t-s))*100
return_index     # 3.4 %

# 7
y<-table(df$Cost_Type)
barplot(y[order(y)],col = c("red","violetred3","antiquewhite2"),main = "Freq of Cost Type")

# 8





install.packages("data.table", dependencies = TRUE)
library(data.table)

dt <- as.data.table(df)
summary(dt)
categorical_cols <- names(dt)[sapply(dt, function(x) is.factor(x) | is.character(x))]
categorical_cols

for (col in categorical_cols) {
  # Calculate mean target encoding
  encoding_map <- dt[, .(mean_target = mean(dt$Total_Invoice_Amount)), by = col]  # Replace target_column with your actual target column name
  setkeyv(encoding_map, col)  # Set key for fast join
  dt[encoding_map, paste0(col, "_encoded") := i.mean_target, on = col]
}
summary(dt)
# Remove the original categorical columns from the encoded dataframe
dt[, (categorical_cols) := NULL]









install.packages("categoryEncoders")
library(categoryEncoders)

# Build a data set
data_set <- data.frame(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"),
                       grades = c(1, 1, 2, 3, 4))

# Perform target encoding
encoder <- TargetEncoder(c("student"), "grades")
encoded_data <- transform(encoder, data_set)

# View the encoded data set
print(encoded_data)







data_set <- data.table(student = c("Marie", "Marie", "Pierre", "Louis", "Louis"),
                       grades = c(1, 1, 2, 3, 4))

target_encoding <- build_target_encoding(data_set, cols_to_encode = "student",
                                         target_col = "grades", functions = c("mean", "sum"))
















summary(dt)






cor_matrix <- cor(df)
cor_matrix

heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Correlation Heatmap")
