
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

# correct date columns
install.packages("lubridate")
library(lubridate)
problematic_dates <- df[!complete.cases(mdy(df$Receipt_Date)), "Receipt_Date"]
print(problematic_dates)
problematic_dates <- df[!complete.cases(mdy(df$Appoint_Date)), "Appoint_Date"]
print(problematic_dates)
problematic_dates <- df[!complete.cases(mdy(df$Complete_Date)), "Complete_Date"]
print(problematic_dates)

df$Receipt_Date <- as.Date(df$Receipt_Date, format = "%m/%d/%Y")
df$Appoint_Date <- as.Date(df$Appoint_Date, format = "%m/%d/%Y")
df$Complete_Date <- as.Date(df$Complete_Date, format = "%m/%d/%Y")
Receipt_start_date <- min(df$Receipt_Date)
Appoint_start_date <- min(df$Appoint_Date)
Complete_start_date <- min(df$Complete_Date)
df$Receipt_Day <- as.numeric(difftime(df$Receipt_Date, Receipt_start_date, units = "days"))
df$Appoint_Day <- as.numeric(difftime(df$Appoint_Date, Appoint_start_date, units = "days"))
df$Complete_Day <- as.numeric(difftime(df$Complete_Date, Complete_start_date, units = "days"))
summary(df)


# Target Encoding
install.packages("data.table", dependencies = TRUE)
update.packages(ask = FALSE, dependencies = TRUE)
library(data.table)

dt <- as.data.table(df)
summary(dt)
columns_to_remove <- c('No', 'Serial_No', 'Receipt_Date', 'Appoint_Date', 'Complete_Date')
dt <- dt[, (columns_to_remove) := NULL]
categorical_cols <- names(dt)[sapply(dt, function(x) is.factor(x) | is.character(x))]
categorical_cols


install.packages("dataPreparation")
library(dataPreparation)

target_encoding <- build_target_encoding(dt, cols_to_encode = categorical_cols,
                                         target_col = "Total_Invoice_Amount", functions = c("mean"))
dt_encoded <- target_encode(dt, target_encoding = target_encoding, drop = TRUE)
names(dt_encoded)
names(dt_encoded)[names(dt_encoded)=="Total_Invoice_Amount_mean_by_Cost_Type"]<-"Cost_Type"
names(dt_encoded)[names(dt_encoded)=="Total_Invoice_Amount_mean_by_Product_Group"]<-"Product_Group"
names(dt_encoded)[names(dt_encoded)=="Total_Invoice_Amount_mean_by_City"]<-"City"
names(dt_encoded)[names(dt_encoded)=="Total_Invoice_Amount_mean_by_Defect_Des"]<-"Defect_Des"
names(dt_encoded)[names(dt_encoded)=="Total_Invoice_Amount_mean_by_Symptom_Desc"]<-"Symptom_Desc"
names(dt_encoded)[names(dt_encoded)=="Total_Invoice_Amount_mean_by_Action"]<-"Action"
names(dt_encoded)[names(dt_encoded)=="Total_Invoice_Amount_mean_by_Labor_Charge_Desc"]<-"Labor_Charge_Desc"
names(dt_encoded)[names(dt_encoded)=="Total_Invoice_Amount_mean_by_Engineer"]<-"Engineer"

# Correlation 
cor_matrix <- cor(dt_encoded)
cor_matrix>0.7

heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Correlation Heatmap")


# VIF Calculation
install.packages("stringi")
library("stringi")
install.packages("car", dependencies = TRUE)
library(car)

df_new <- as.data.frame(dt_encoded)
model <- lm(df_new$Total_Invoice_Amount ~ ., data = df_new)
vif_values <- car::vif(model)
print(vif_values)

unique(df$Cost_Type)
unique(df_new$Cost_Type)






# 9
summary(model)
summary(model)$r.squared

# remove columns with p-value > 0.5 or vif > 10
names(df_new)
df_scale <- scale(df_new)
df_scale <- as.data.frame(df_scale)
summary(df_scale)
model_final <- lm(df_scale$Total_Invoice_Amount ~ Service_type + TAT01 + TAT02 + Job_Satus + 
                    Labor_Charge_Amount + Parts_Amount + Discount_Amount + 
                    Receipt_Day + Appoint_Day + Complete_Day + Cost_Type + Product_Group + City + 
                    Defect_Des + Symptom_Desc + Action + Labor_Charge_Desc + Engineer, data = df_scale)
vif_values <- car::vif(model_final)
print(vif_values)

summary(model_final)
summary(model_final)$r.squared # r2


# 10
install.packages("arules")
library("arules")




