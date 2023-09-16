x<-read.csv("Samdata.csv",header=T)
x

names(x)
head(x)
str(x)
summary(x)

# names(x)[16] = "Action"
names(x)[names(x)=="Repair_Action_Desc"]<-"Action"

df <- data.frame(x)
df
head(df)
# s <- sum(is.na(df$Product_Date))
# t <- nrow(df)
# naPercent <- (s/t)*100
# naPercent

colMeans(is.na(df))*100
df <- df[, -which(names(df) == "Product_Date")]
dim(df)


mean_TAT01 <- mean(df$TAT01)
mean_TAT02 <- mean(df$TAT02)
mean_TAT01
mean_TAT02

s <- sum(duplicated(df$Serial_No))
t <- nrow(df)
return_index <- (s/(t-s))*100
return_index

y<-table(df$Cost_Type)
barplot(y[order(y)],col = c("red","violetred3","antiquewhite2"),main = "Freq of Cost Type")

cor_matrix <- cor(df)
cor_matrix

heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Correlation Heatmap")
