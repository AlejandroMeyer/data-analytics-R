#The file has been loaded
load("Group_3.RData")

#2) Changing the name of my variable
View(dat) #as table

data <- dat
head(data)

#Count how many times each educational level code appears
table(data[["isced11_20"]])
