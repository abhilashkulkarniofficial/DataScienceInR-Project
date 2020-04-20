#We need the following library
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
require(xgboost)
require(methods)
require(data.table)
require(magrittr)
library("wordcloud") #for word cloud
library("RColorBrewer") #for word cloud
library(corrplot)

setwd("/Users/abhilashsk/Downloads")

#Import the dataset
train <- fread('all3/train.tsv', header = T, stringsAsFactors = T)
test <- fread('all3/test.tsv', header=TRUE, stringsAsFactors = T)

dim(train)

print(object.size(train), units = 'Mb')

summary(train)
#Target Variable analysis
range(train$price)

ggplot(data = train, aes(x = log(price+1))) + 
  geom_histogram(fill = 'orangered2') +
  labs(title = 'Histogram of log item price + 1')

#Item Condition
table(train$item_condition_id)

train[, .N, by = item_condition_id] %>%
  ggplot(aes(x = as.factor(item_condition_id), y = N/1000)) +
  geom_bar(stat = 'identity', fill = 'cyan2') + 
  labs(x = 'Item condition', y = 'Number of items (000s)', title = 'Number of items by condition category')

train[, .(.N, median_price = median(price)), by = item_condition_id][order(item_condition_id)]

ggplot(data = train, aes(x = as.factor(item_condition_id), y = log(price + 1))) + 
  geom_boxplot(fill = 'cyan2', color = 'darkgrey')

#Shipping
table(train$shipping)

train %>%
  ggplot(aes(x = log(price+1), fill = factor(shipping))) + 
  geom_density(adjust = 2, alpha = 0.6) + 
  labs(x = 'Log price', y = '', title = 'Distribution of price by shipping')

#Brand
train[, .(median_price = median(price)), by = brand_name] %>%
  head(25) %>%
  ggplot(aes(x = reorder(brand_name, median_price), y = median_price)) + 
  geom_point(color = 'cyan2') + 
  scale_y_continuous(labels = scales::dollar) + 
  coord_flip() +
  labs(x = '', y = 'Median price', title = 'Top 25 most expensive brands') 

#Item Categories
length(unique(train$category_name))

sort(table(train$category_name), decreasing = TRUE)[1:10]

train[, .(median = median(price)), by = category_name][order(median, decreasing = TRUE)][1:30] %>%
  ggplot(aes(x = reorder(category_name, median), y = median)) + 
  geom_point(color = 'orangered2') + 
  coord_flip() + 
  labs(x = '', y = 'Median price', title = 'Median price by item category (Top 30)') + 
  scale_y_continuous(labels = scales::dollar)


colnames(train) <- c("Id", "name", "item_condition_id", "category_name", "brand_name", 
                     "price", "shipping", "item_description")
colnames(test) <- c("Id", "name", "item_condition_id", "category_name", "brand_name", 
                    "shipping", "item_description")

#We will remove the cases with price =0
train <- train[train$price!=0,]
train$log_price <- log(train$price, base=exp(1))
#Thx for the comment, taking log on the response can greatly improve the model
#you can also print the histogram of price, you can see that price is highly skewed
#taking log on price can slightly improve this scenario

trainY <- train[,c(6,9)] #These two column are the response variable, will add it back later
trainX <- train[,-c(6,9)]
step <- rbind(trainX, test, fill=TRUE)

#Checking if there is any missing data
colSums(sapply(step, is.na))

step$item_description <- toupper(step$item_description)
step$item_description <- as.factor(step$item_description)
step$category_name <- toupper(step$category_name)
step$category_name <- as.factor(step$category_name)
step$name <- toupper(step$name)
step$name <- as.factor(step$name)
step$brand_name <- toupper(step$brand_name)
step$brand_name <- as.factor(step$brand_name)

step2 <- step[,-7]

train$item_description <- toupper(train$item_description)
train_desc <- table(train[,8])
train_desc <- data.frame(train_desc)
colnames(train_desc) <- c("desc", "freq")
train_desc <- train_desc[order(train_desc$freq, decreasing=TRUE),]


train_desc_cloud <- train_desc[c(2:200),] 
#As the wordcloud has limitation on no. of character, you would see some error below

set.seed(1234)
wordcloud(words = train_desc_cloud$desc, freq = train_desc_cloud$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#we created a new data set "part" to help create the below 36 new variables
part <- step[,c(4,5, 7)]
part <- data.frame(part)
colnames(part) <- c("cate", "brand", "desc")
part$cate <- as.factor(part$cate)
part$brand <- as.factor(part$brand)
part$desc <- as.factor(part$desc)

train$brand_name <- toupper(train$brand_name)
train_brand <- table(train[,5])
train_brand <- data.frame(train_brand)
colnames(train_brand) <- c("brand", "freq")
train_brand <- train_brand[order(train_brand$freq, decreasing=TRUE),]


train_brand_cloud <- train_brand[c(2:200),] 
#As the wordcloud has limitation on no. of character, you would see some error below

wordcloud(words = train_brand_cloud$brand, freq = train_brand_cloud$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#There are 10 level in the first level of Category variable, 
#we woul exclude "Others" and create 9 new variables.
part$beauty <- ifelse(grepl("BEAUTY", part$cate)|grepl("BEAUTY", part$desc),1,0)
part$electronic <- ifelse(grepl("ELECTRONIC", part$cate)|grepl("ELECTRONIC", part$desc),1,0)
part$handmade <- ifelse(grepl("HANDMADE", part$cate)|grepl("HANDMADE", part$desc)|
                          grepl("DIY", part$cate)|grepl("DIY", part$desc),1,0)
part$home <- ifelse(grepl("HOME", part$cate)|grepl("HOME", part$desc)|
                      grepl("HOUSE", part$cate)|grepl("HOUSE", part$desc),1,0)
part$kid <- ifelse(grepl("KID", part$cate)|grepl("KID", part$desc)|
                     grepl("BOY", part$cate)|grepl("BOY", part$desc)|
                     grepl("GIRL", part$cate)|grepl("GIRL", part$desc)|
                     grepl("CHILD", part$cate)|grepl("CHILD", part$desc),1,0) 
part$women <- ifelse((grepl("WOMEN", part$cate)|grepl("WOMEN", part$desc)),1,0)
part$men <- ifelse(((!grepl("WOMEN", part$cate)) & (grepl("MEN", part$cate)))|
                     ((!grepl("WOMEN", part$desc)) & (grepl("MEN", part$desc)))
                   ,1,0)
part$athletic <- ifelse(grepl("ATHLETIC", part$cate)|grepl("ATHLETIC", part$desc)|
                          grepl("SPORT", part$cate)|grepl("SPORT", part$desc),1,0)
part$wine <- ifelse(grepl("WINE", part$cate)|grepl("WINE", part$desc)|
                      grepl("VINTAGE", part$cate)|grepl("VINTAGE", part$desc)|
                      grepl("ALCOCHOL", part$cate)|grepl("ALCOHOL", part$desc),1,0)

part$beauty <- as.numeric(part$beauty)
part$electronic <- as.numeric(part$electronic)
part$handmade <- as.numeric(part$handmade)
part$home <- as.numeric(part$home)
part$kid <- as.numeric(part$kid)
part$women <- as.numeric(part$women)
part$men <- as.numeric(part$men)
part$athletic <- as.numeric(part$athletic)
part$wine <- as.numeric(part$wine)

#There are more than 100 level in the second level of Category variable, 
#we have chosen the most frequent 12 of them and create 12 new variables.
part$athletic_apparel <- ifelse(grepl("ATHLETIC APPAREL", part$cate)|grepl("ATHLETIC APPAREL", part$desc)
                                ,1,0)
part$makeup <- ifelse(grepl("MAKEUP", part$cate)|grepl("MAKEUP", part$desc),1,0)
part$blouse <- ifelse(grepl("BLOUSE", part$cate)|grepl("BLOUSE", part$desc),1,0)
part$shoes <- ifelse(grepl("SHOE", part$cate)|grepl("SHOE", part$desc),1,0)
part$toy <- ifelse(grepl("TOY", part$cate)|grepl("TOY", part$desc),1,0)
part$jewelry <- ifelse(grepl("JEWELRY", part$cate)|grepl("JEWELRY", part$desc),1,0)
part$phone <- ifelse(((!grepl("ACCESSORIES", part$cate)) & (grepl("PHONE", part$cate)))|
                       ((!grepl("ACCESSORIES", part$desc)) & (grepl("PHONE", part$desc)))
                     ,1,0)
part$bag <- ifelse(grepl("BAG", part$cate)|grepl("BAG", part$desc),1,0)
part$dress <- ifelse(grepl("DRESS", part$cate)|grepl("DRESS", part$desc),1,0)
part$pant <- ifelse(grepl("PANT", part$cate)|grepl("PANT", part$desc)|
                      grepl("JEAN", part$cate)|grepl("JEAN", part$desc),1,0)
part$accessories <- ifelse(((!grepl("PHONE", part$cate)) & (grepl("ACCESSORIES", part$cate)))|
                             ((!grepl("PHONE", part$desc)) & (grepl("ACCESSORIES", part$desc)))
                           ,1,0)
part$luxury <- ifelse(grepl("WATCH", part$cate)|grepl("WATCH", part$desc)|
                        grepl("JEWELRY", part$cate)|grepl("JEWELRY", part$desc)|
                        grepl("RING", part$cate)|grepl("RING", part$desc)|
                        grepl("BRACELETS", part$cate)|grepl("BRACELETS", part$desc),1,0)

part$athletic_apparel <- as.numeric(part$athletic_apparel)
part$makeup <- as.numeric(part$makeup)
part$blouse <- as.numeric(part$blouse)
part$shoes <- as.numeric(part$shoes)
part$toy <- as.numeric(part$toy)
part$jewelry <- as.numeric(part$jewelry)
part$phone <- as.numeric(part$phone)
part$bag <- as.numeric(part$bag)
part$dress <- as.numeric(part$dress)
part$pant <- as.numeric(part$pant)
part$accessories <- as.numeric(part$accessories)
part$luxury <- as.numeric(part$luxury)

#There are more than 600 level in the third level of Category variable, 
#we have chosen the most frequent 7 of them and create 7 new variables.
part$tshirt <- ifelse(grepl("T-SHIRT", part$cate)|grepl("T-SHIRT", part$desc)|
                        grepl("TSHIRT", part$cate)|grepl("TSHIRT", part$desc),1,0)
part$face <- ifelse(grepl("FACE", part$cate)|grepl("FACE", part$desc),1,0)
part$game <- ifelse(grepl("GAME", part$cate)|grepl("GAME", part$desc),1,0)
part$lip <- ifelse(grepl("LIP", part$cate)|grepl("LIP", part$desc),1,0)
part$eye <- ifelse(grepl("EYE", part$cate)|grepl("EYE", part$desc),1,0)
part$care <- ifelse(grepl("CARE", part$cate)|grepl("CARE", part$desc),1,0)
part$top <- ifelse(grepl("TOP", part$cate)|grepl("TOP", part$desc),1,0)

part$tshirt <- as.numeric(part$tshirt)
part$face <- as.numeric(part$face)
part$game <- as.numeric(part$game)
part$lip <- as.numeric(part$lip)
part$eye <- as.numeric(part$eye)
part$care <- as.numeric(part$care)
part$top <- as.numeric(part$top)

#There are more than 600 level in brand variable
#we have chosen the most frequent 5 of them and create 5 variable

part$pink <- ifelse(grepl("PINK", part$brand),1,0)
part$secret <- ifelse(grepl("VICTORIA'S SECRET", part$brand)|grepl("VICTORIA'S SECRET", part$desc),1,0)
part$nike <- ifelse(grepl("NIKE", part$brand)|grepl("NIKE", part$desc),1,0)
part$apple <- ifelse(grepl("APPLE", part$brand)|grepl("APPLE", part$desc)|
                       grepl("MAC", part$brand)|grepl("MAC", part$desc)|
                       grepl("IPHONE", part$desc)|grepl("I PHONE", part$desc)|
                       grepl("IPAD", part$desc)|grepl("I PAD", part$desc),1,0)
part$lularoe <- ifelse(grepl("LULAROE", part$brand)|grepl("LULAROE", part$desc),1,0)


part$pink <- as.numeric(part$pink)
part$secret<- as.numeric(part$secret)
part$nike <- as.numeric(part$nike)
part$apple <- as.numeric(part$apple)
part$lularoe <- as.numeric(part$lularoe)

#From the word cloud of "item_description", 
#we have done the same way to choose the most frequent 8 of them and create 8 new variables.
#we cannot create too much variable as it really slows the model down
part$size <- ifelse((grepl("SMALL", part$desc)|grepl("MEDIUM", part$desc)|
                       grepl("LARGE", part$desc)|grepl("XL", part$desc)|
                       grepl("XS", part$desc)|grepl("SIZE", part$desc)|
                       grepl("INCH", part$desc)),1,0)

part$color <- ifelse((grepl("RED", part$desc)|grepl("YELLOW", part$desc)|
                        grepl("BLUE", part$desc)|grepl("GREEN", part$desc)|
                        grepl("WHITE", part$desc)|grepl("BLACK", part$desc)|
                        grepl("DULL", part$desc)|grepl("GREY", part$desc)|
                        grepl("BROWN", part$desc)|grepl("PURPLE", part$desc)|
                        grepl("COLOR", part$desc)|grepl("COLOUR", part$desc)),1,0)  

part$leather <- ifelse(grepl("LEATHER", part$desc),1,0)

part$new <- 0 #without age information
part$new[grepl("NEW", part$desc)|grepl("NEVER", part$desc)|
           grepl("BNWT", part$desc)|grepl("NWT", part$desc)] <- 2 #New
part$new[grepl("ONCE", part$desc)|grepl("TWICE", part$desc)|
           (grepl("WORN", part$desc)&(!grepl("NEVER WORN", part$desc)))] <- 1 #Nearly New

part$desc <- as.character(part$desc)
part$len_des <- 1 #With description shorter or equal to 50 character
part$len_des[nchar(part$desc)>60] <- 2 #With description more than 50 character
part$len_des[grepl("NO DESCRIPTION YET", part$desc)] <- 0 #no description
part$desc <- as.factor(part$desc)

part$cond <- 0
part$cond[grepl("GOOD CONDITION", part$desc)|grepl("GREAT CONDITION", part$desc)] <- 1
part$cond[grepl("EXCELLENT CONDITION", part$desc)|grepl("PERFECT CONDITION", part$desc)] <-2


part$tags <- ifelse(grepl("WITH TAG", part$desc)|grepl("BNWT", part$desc)|grepl("NWT", part$desc),1,0)
part$no_tags <- ifelse(grepl("WITHOUT TAG", part$desc)|grepl("NO TAG", part$desc),1,0)

part$size <- as.numeric(part$size)
part$color <- as.numeric(part$color)
part$leather <- as.numeric(part$leather)
part$new <- as.numeric(part$new)
part$len_des <- as.numeric(part$len_des)
part$cond <- as.numeric(part$cond)
part$tags <- as.numeric(part$tags)
part$no_tags <- as.numeric(part$no_tags)

#XGBoost only accepts numeric variables, so we convert all the character to numeric value here
char_n_idx <- c(2,4,5)
#--------- Convert them to number---------#
#n_name = length(char_name)
step2 = data.frame(step2)
n_idx = length(char_n_idx)
counter = 1
for (i in seq(n_idx)){
  idx = char_n_idx[i]
  n_lv = length(unique(step2[,idx])) #number of levels 
  levels(step2[,idx]) = counter:(counter+n_lv-1) #change them to numerics
  counter = (counter+n_lv-1) 
  #avoid using different name : each non-numeric data in each col -->diff. no.
  step2[,idx] <- as.numeric(step2[,idx])
}

part2 <- part[,-c(1,2,3)]
step3 <- cbind(step2, part2)
summary(step3)

corrMat <- cor(part2)
corrplot(corr=corrMat, type="upper", method= "color")

#step3 <- step2

train_len <- dim(trainX)[1]
step_len <- dim(step3)[1]

train_x <- step3[1:train_len,]
test_x <- step3[(train_len+1):step_len,]

train_final <- as.matrix(train_x, sparse = TRUE) #this is the code to change train dataframe to matrix
test_final <- as.matrix(test_x, sparse = TRUE)

boost <- xgboost(data = train_final, label = train$log_price, max.depth = 16, eta = 0.1, print_every_n = 15, nthread = 4, nround = 450, objective = "reg:linear")
log_answer <- predict(boost, test_final)
imp <- xgb.importance (model = boost)
xgb.plot.importance (importance_matrix = imp[1:20])

sum(log_answer<0)
sum(is.na(log_answer))
#ensure the output is not na/ 0

log_answer <- data.frame(log_answer)
log_answer$price[log_answer$log_answer<0|is.na(log_answer$log_answer)] <- 0
log_answer$price <- exp(log_answer$log_answer)

log_answer_2 <- cbind(test$Id, log_answer$price)
colnames(log_answer_2) <- c("test_id", "price")
log_answer_2 <- data.frame(log_answer_2)
log_answer_2$test_id <- format(log_answer_2$test_id, scientific=FALSE)
head(log_answer_2)