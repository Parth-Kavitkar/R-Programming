fruits <- c("Apple", "Banana", "Orange", "Mango", "Grapes", "Pineapple", "Strawberry", "Watermelon", "Cherry", "kiwi")
fruits_n <- c(3,7,9,1,6,9,8,3,8,6)
flowers <- c("Rose", "Tulip", "Lily", "Sunflower", "Daisy", "Marigold", "Orchid", "Iris", "Lotus", "Hibiscus")
flowers_n <- c(5,9,2,8,1,7,2,7,9,4)

fruits
fruits_n
flowers
flowers_n

data2<-data.frame(fruits,fruits_n,flowers,flowers_n)
data2

write.csv(data2,"my_data2.csv",row.names = FALSE)

getwd()