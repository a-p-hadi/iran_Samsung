
# 1
x<-read.csv("F:\\Learning\\Data Science (19)\\24 R (3)\\Project\\iran_Samsung\\Samdata.csv",header=T)
x





# ------------------------------------------------------------------------------
# 2
names(x)
head(x)
str(x)
summary(x)





# ------------------------------------------------------------------------------
# 3
# names(x)[16] = "Action"
names(x)[names(x)=="Repair_Action_Desc"]<-"Action"







# ------------------------------------------------------------------------------
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






# ------------------------------------------------------------------------------
# 5
duration_time <- df$TAT02 - df$TAT01
duration_time
mean_start <- mean(df$TAT01)
mean_duration <- mean(duration_time)
mean_start       # 1.3 day
mean_duration    # 5.4 day





# ------------------------------------------------------------------------------
# 6
s <- sum(duplicated(df$Serial_No))
t <- nrow(df)
return_index <- (s/(t-s))*100
return_index     # 3.4 %





# ------------------------------------------------------------------------------
# 7
y<-table(df$Cost_Type)
barplot(y[order(y)],col = c("red","violetred3","antiquewhite2"),main = "Freq of Cost Type")








# ------------------------------------------------------------------------------
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
install.packages('corrplot')
library(corrplot)
cor_matrix <- cor(dt_encoded)
corrplot(cor_matrix, method = 'number')
cor_matrix>0.7

heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Correlation Heatmap")


# VIF Calculation
install.packages("stringi")
library("stringi")
install.packages("car", dependencies = TRUE)
library(car)

df_encoded <- as.data.frame(dt_encoded)
model <- lm(df_encoded$Total_Invoice_Amount ~ ., data = df_encoded)
vif_values <- car::vif(model)
print(vif_values)

# encode check
unique(df$Cost_Type)
unique(df_new$Cost_Type)





# ------------------------------------------------------------------------------
# 9
# Correlation (Cost_Type, Total_Invoice_Amount) = 0.17775805 
summary(model)
summary(model)$r.squared

# remove columns with p-value > 0.5 or vif > 10
names(df_encoded)
df_encoded$FinalInvoice <- (df_encoded$Total_Invoice_Amount + df_encoded$Discount_Amount)
df_scale <- scale(df_encoded)
df_scale <- as.data.frame(df_scale)
summary(df_scale)
model_final <- lm(df_scale$FinalInvoice ~ Labor_Charge_Desc:Service_type + TAT01 + TAT02 +
                    Labor_Charge_Amount + Parts_Amount + 
                    Cost_Type + Product_Group + Labor_Charge_Desc:City + 
                    Engineer:Symptom_Desc + Action + Defect_Des + Engineer, data = df_scale)
vif_values <- car::vif(model_final)
print(vif_values)

summary(model_final)
summary(model_final)$r.squared # r2




# ------------------------------------------------------------------------------
# 10
install.packages("arules", dependencies = TRUE)
library("arules")

summary(df)
df_arules <- subset(df, select = -c(No, Serial_No, Receipt_Date , Appoint_Date , Complete_Date , Receipt_Day, Appoint_Day, Complete_Day, TAT01, TAT02,
                                    Job_Satus, Parts_Amount , Discount_Amount , Total_Invoice_Amount , Labor_Charge_Amount))
summary(df_arules)

df_arules$Cost_Type <- as.factor(df_arules$Cost_Type)
df_arules$Defect_Des <- as.factor(df_arules$Defect_Des)
df_arules$Service_type <- as.factor(df_arules$Service_type)
df_arules$City <- as.factor(df_arules$City)
df_arules$Symptom_Desc <- as.factor(df_arules$Symptom_Desc)
df_arules$Action <- as.factor(df_arules$Action)
df_arules$Labor_Charge_Desc <- as.factor(df_arules$Labor_Charge_Desc)
df_arules$Engineer <- as.factor(df_arules$Engineer)
df_arules$Product_Group <- as.factor(df_arules$Product_Group)
df_arules <-  as(df_arules, "transactions")

arules_model <- apriori(df_arules, parameter = list(supp = 0.7, conf = 0.8))
arules_model
inspect(arules_model[1:15])
itemFrequencyPlot(df_arules, topN = 15,
                  main = "Items Distribution", 
                  type = "absolute", ylab = "Frequency")

install.packages("arulesViz")
library("arulesViz")
inspectDT(arules_model)
inspect(subset(arules_model, lift > 1))
inspect(subset(arules_model, support > 0.8))

# دستگاههاي داراي گارانتي با علت خرابي فرسودگي پذيرش شده اند كه جاي بررسي دارد.
# در شهر تهران اغلب خدمات در محل مشتري ارائه شده است
# در شهر تهران اغلب نياز به خدمات پس از فروش به علت فرسودگي بوده است
# خدمات پس از فروش در شهر تهران كه به علت فرسودگي درخواست شده است اغلب در محل مشتري انجام گرفته است






# ------------------------------------------------------------------------------
# 11
install.packages("factoextra")
install.packages("fpc")
library("cluster")
library("ggplot2")
library("factoextra")
library(fpc)

df_seg <- subset(df_encoded, select = -c(No, Serial_No, Receipt_Date , Appoint_Date , Complete_Date , Receipt_Day, 
                                 Appoint_Day, Complete_Day, TAT01, TAT02))
df_seg_scale <- as.data.frame(scale(df_seg))
summary(df_seg_scale)
str(df_seg_scale)


# k values
set.seed(234)
silhouette_score <- function(k){
  km <- kmeans(df_seg_scale, centers = k, nstart=10)
  ss <- silhouette(km$cluster, dist(df_seg_scale))
  mean(ss[, 3])
}
k <- 2:20
avg_sil <- sapply(k, silhouette_score)
optimal_k <- k[which.max(avg_sil)]
print(paste("Optimal number of clusters (k) =", optimal_k))
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores')
abline(v = optimal_k, col = "red", lty = 2)

fviz_nbclust(df_seg_scale, kmeans, method='silhouette', k.max = 20)

# Segments
set.seed(234)
seg_km <- kmeans(df_seg_scale, centers = optimal_k)
df_seg_scale$segment <- seg_km$cluster
seg_km$tot.withinss
seg_km$withinss
seg_km$betweenss
table(seg_km$cluster)

# Visulization
library(ggplot2)
plot(df_seg$Symptom_Desc, df_seg$Action, col = seg_km$cluster)
fviz_cluster(seg_km, geom = "point", data = df_seg_scale) + 
  ggtitle("Number of Clusters K = 2")
ggplot(data = df_seg_scale, aes(Symptom_Desc, Cost_Type, color = factor(segment))) +
  geom_point()
boxplot(df_seg_scale$Product_Group ~ df_seg_scale$segment, ylab = "Cost_Type", xlab = "Cluster")




