#set working environment
setwd("/Documents/HCMUS/STAT452/Project")
#read data
data<-read.csv("human_resource.csv")
#store data
save(data, file = 'data.rda')
load('data.rda')
attach(data)
#count the number of rows and columns
nrow(data)
ncol(data)

#name the column
names(data)<-c("satisfaction", "evaluation", "hours", "left", "promotion", "salary")

#left
data$left<-factor(data$left)
levels(data$left)<-c("No", "Yes")
#promotion
data$promotion<-factor(data$promotion)
levels(data$promotion)<-c("No", "Yes")

summary(data)


#create table
psal<-table(data$salary)

#draw pie chart
pie(psal, main = "Pie chart for salary" ,radius = 1.0, clockwise = TRUE, col = rainbow(3))


#create table left
pleft<-table(data$left)

#draw pie chart for left
pie(pleft, main = "Pie chart for left", radius = 1.0, clockwise = TRUE, col = c("springgreen", "yellow2"))


#create table salaray and left
saleft<-table(left, salary)

#draw the graph
barplot(saleft, main = "Relation between salary and left", col = c("slategray1", "wheat"))
legend("topleft", c("No", "Yes"), fill = c("slategray1", "wheat"))


#create table
promo<-table(data$promotion)
#draw pie chart
pie(promo, radius = 1.0, main = "Pie chart for promotion",clockwise = TRUE, col = c("beige", "aquamarine"))


#create table of left and promotion
promoleft<-table(left, promotion)

#draw bar graphs
barplot(promoleft, main = "Relation between promotion and left", beside = TRUE, col = c("cadetblue1", "darkseagreen1"))
legend("topright", c("Remained", "Left"), fill = c("cadetblue1", "darkseagreen1"))


#crate boxplot
boxplot(hours, main = "Boxplot for hours" ,col = c("gray96"))


#devide column
par(mfrow = c(1,3))
#draw boxplot
boxplot(subset(hours, salary == 'high'), main = 'high', col = c("lightskyblue1"))
boxplot(subset(hours, salary == 'low'), main = 'low', col = c("hotpink"))
boxplot(subset(hours, salary == 'medium'), main = 'medium', col = c("lightgoldenrod1"))

#devide column
par(mfrow = c(1,2))
#Draw boxplots
boxplot(subset(data$hours, data$promotion =='No'), main = "No", col = c("ivory"))
boxplot(subset(data$hours, data$promotion == "Yes"), main = "Yes", col = c("slategray1"))


#Devide column
par(mfrow = c(1,2))

#Draw
boxplot(subset(data$hours, data$left == "No"), main = "No", col = c("salmon"))
boxplot(subset(data$hours, data$left == "Yes"), main = "Yes", col = c("palegreen"))


#create boxplot
boxplot(evaluation, main = "Boxplot for evaluation", col = c("bisque"))

#devide column
par(mfrow = c(1,2))
#draw
boxplot(evaluation[data$promotion == "Yes"], main = "Yes", col = c('deepskyblue'))
boxplot(evaluation[data$promotion == "No"], main = "No", col = c('lightgoldenrodyellow'))

#devide column
par(mfrow = c(1,2))

#draw boxplots
boxplot(evaluation[data$left == 'Yes'], main = 'Left', col = c('orchid'))
boxplot(evaluation[data$left == 'No'], main = 'Remained', col = c('yellowgreen'))

#draw plot

plot(hours, evaluation, main = 'Evaluation and Hours')

#draw boxplot

boxplot(satisfaction, main = 'Satisfaction', col = c('coral'))

#devide into 3 columns
par(mfrow = c(1,3))

#draw boxplots
boxplot(satisfaction[salary == 'high'], main = 'High', col = c('darkolivegreen1'))
boxplot(satisfaction[salary == 'medium'], main = 'Medium', col = c('firebrick1'))
boxplot(satisfaction[salary == 'low'], main = 'Low', col = c('deepskyblue1'))

#deivde into 2 columns
par(mfrow = c(1,2))

#draw boxplots
boxplot(satisfaction[data$left == 'Yes'], main = 'Yes', col = c('moccasin'))
boxplot(satisfaction[data$left == 'No'], main = 'No', col = c('indianred1'))


#draw matrix of scatterplot
pairs(~satisfaction + evaluation + hours)


#use t test
test<-t.test(hours ~ promotion)
#print the result
test

#test the hypothesis
test <- t.test(hours[data$left == 'Yes'], y = hours[data$left == 'No'], alternative = "less")
#show the result
test

#use t test
test<-t.test(evaluation ~ promotion)

#print the result
test

#test the hypothesis
test <- t.test(evaluation[data$left == 'No'], y = evaluation[data$left == 'Yes'], alternative = "greater")
#show the result
test

#test the hypothesis
test <- t.test(satisfaction[data$left == 'Yes'], y = satisfaction[data$left == 'No'], alternative = "less")
#show the result
test

#build the model
model1 <- lm(evaluation ~ hours)
#show the result
model1

#plot the graph
plot(evaluation ~ hours)
#add the line
abline(model1, col = 'blue', lwd = 3)

summary(model1)

confint(model1)

#build the model
model2 <- lm(satisfaction ~ evaluation)
#show the result
model2

#plot the graph
plot(satisfaction ~ evaluation)
#add the line
abline(model2, col = 'red', lwd = 3)

summary(model2)

confint(model2)

#build model
model3 <- lm(satisfaction ~ evaluation + hours)
#show model
model3


summary(model3)

#build
fit <- lm(satisfaction ~ 0 + evaluation + hours)
fit
#test the reduced equation
anova(model3, fit)
#analysis
summary(fit)

