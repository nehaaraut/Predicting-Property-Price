# Project 1 - Property Price using linear regression

data <- read.csv("C:/Users/Nehaa/Desktop/Imarticus/DataSet/Property_Price_Train.csv")

dim(data)
str(data)
summary(data)

# Data preprocessing
is.na(data)
is.na(data$Id)
#install.packages("naniar")
library(naniar)
vis_miss(data)   #too condensed

colSums(is.na(data))

#imputation of a continuous variable

which(is.na(data$Lot_Extent))
sum(is.na(data$Lot_Extent))

library(psych)
describe(data$Lot_Extent)
hist(data$Lot_Extent)
table(data$Lot_Extent)
data$Lot_Extent[is.na(data$Lot_Extent)] <- median(data$Lot_Extent, na.rm = TRUE)
which(is.na(data$Lot_Extent))
sum(is.na(data$Lot_Extent))
describe(data$Lot_Extent)
hist(data$Lot_Extent)

#imputation of a categorical variable


str(data$Brick_Veneer_Type)
summary(data$Brick_Veneer_Type)

which(is.na(data$Brick_Veneer_Type)) 
sum(is.na(data$Brick_Veneer_Type))
t1 = table(data$Brick_Veneer_Type)
t1
sum(t1)
barplot(t1)
mode(data$Brick_Veneer_Type)

data$Brick_Veneer_Type[is.na(data$Brick_Veneer_Type)] <- "None"
which(is.na(data$Brick_Veneer_Type)) 
sum(is.na(data$Brick_Veneer_Type))

t2 = table(data$Brick_Veneer_Type)
t2
sum(t2)
barplot(t2)

par(mfrow=c(1,2))   #plotting 2 plots in 1
barplot(t1)
barplot(t2)
dev.off()

# correlation
cor(data$Sale_Price, data$Lot_Extent) 
cor.test(data$Sale_Price,data$Lot_Extent)
plot(data$Lot_Extent, data$Sale_Price, 
     col = "red", lwd = 4)
line <-lm(data$Sale_Price~ data$Lot_Extent)
abline(line, lwd = 2, col = "blue")

library("car")
scatterplot(data$Sale_Price~ data$Lot_Extent, 
            data = data, col = "red")

# Anova
str(data$Brick_Veneer_Type)
m1 = aov(data$Sale_Price ~ data$Brick_Veneer_Type)
summary(m1) 

# Road_Type
str(data$Road_Type)
summary(data$Road_Type)
which(is.na(data$Road_Type))

#Brick_Veneer_Area
str(data$Brick_Veneer_Area)
summary(data$Brick_Veneer_Area)
describe(data$Brick_Veneer_Area)
table(data$Brick_Veneer_Area)
sum(is.na(data$Brick_Veneer_Area))

hist(data$Brick_Veneer_Area)
data$Brick_Veneer_Area[is.na(data$Brick_Veneer_Area)] <- median(data$Brick_Veneer_Area, na.rm = TRUE)
sum(is.na(data$Brick_Veneer_Area))

cor(data$Sale_Price, data$Brick_Veneer_Area)
cor.test(data$Sale_Price, data$Brick_Veneer_Area)
scatterplot(data$Sale_Price~data$Brick_Veneer_Area,
            data = data, col = "Pink")
table(data$Brick_Veneer_Area)


#Basement Height
str(data$Basement_Height)
summary(data$Basement_Height)
sum(is.na(data$Basement_Height))
table(data$Basement_Height)
data$Basement_Height[is.na(data$Basement_Height)] = "TA"

m1 = aov(data$Sale_Price ~ data$Basement_Height)
summary(m1) 


# Building_Class
str(data$Building_Class)
summary(data$Building_Class)
describe(data$Building_Class)
sum(is.na(data$Building_Class))
table(data$Building_Class)
barplot(table(data$Building_Class))
summary(aov(data$Sale_Price ~ data$Building_Class))

#Zoning_Class
str(data$Zoning_Class)
summary(data$Zoning_Class)
sum(is.na(data$Zoning_Class))
t1 = table(data$Zoning_Class)
t1
barplot(t1)
m1 = aov(data$Sale_Price ~ data$Zoning_Class)
summary(m1)


#Lot_Size
str(data$Lot_Size)
summary(data$Lot_Size)
sum(is.na(data$Lot_Size))
describe(data$Lot_Size)
hist(data$Lot_Size)
table(data$Lot_Size)
cor(data$Sale_Price, data$Lot_Size)
cor.test(data$Sale_Price, data$Lot_Size)
scatterplot(data$Sale_Price~data$Lot_Size,
            data=data)
table(data$Lot_Size)
summary(lm(data$Sale_Price~data$Lot_Size))

#Lane_Type
str(data$Lane_Type)
summary(data$Lane_Type)

#Property_Shape
str(data$Property_Shape)
summary(data$Property_Shape)
sum(is.na(data$Property_Shape))
t1= table(data$Property_Shape)
t1
barplot(t1)
m1 = aov(data$Sale_Price ~ data$Property_Shape)
summary(m1)


#Land_Outline
str(data$Land_Outline)
summary(data$Land_Outline)
sum(is.na(data$Land_Outline))
m1 = aov(data$Sale_Price ~ data$Land_Outline)
summary(m1)


#Utility_Type
str(data$Utility_Type)
summary(data$Utility_Type)
sum(is.na(data$Utility_Type))


#Lot_configuration
str(data$Lot_Configuration)
summary(data$Lot_Configuration)
sum(is.na(data$Lot_Configuration))
m1 = aov(data$Sale_Price ~ data$Lot_Configuration)
summary(m1)


#Property_Slope
str(data$Property_Slope)
summary(data$Property_Slope)
sum(is.na(data$Property_Slope))
m1 = aov(data$Sale_Price ~ data$Property_Slope)
summary(m1)

#Neighborhood
str(data$Neighborhood)
summary(data$Neighborhood)
sum(is.na(data$Neighborhood))
m1 = aov(data$Sale_Price ~ data$Neighborhood)
summary(m1)

#Condition1
str(data$Condition1)
summary(data$Condition1)
sum(is.na(data$Condition1))
m1 = aov(data$Sale_Price ~ data$Condition1)
summary(m1)

#Condition2
str(data$Condition2)
summary(data$Condition2)
sum(is.na(data$Condition2))
m1 = aov(data$Sale_Price ~ data$Condition2)
summary(m1)


# House_Type
str(data$House_Type)
summary(data$House_Type)
sum(is.na(data$House_Type))
m1 = aov(data$Sale_Price ~ data$House_Type)
summary(m1)

#House_Design
str(data$House_Design)
summary(data$House_Design)
sum(is.na(data$House_Design))
m1 = aov(data$Sale_Price ~ data$House_Design)
summary(m1)

#Overall_Material
str(data$Overall_Material)
table(data$Overall_Material)
summary(data$Overall_Material)
sum(is.na(data$Overall_Material))
m1 = aov(data$Sale_Price ~ data$Overall_Material)
summary(m1)


#House_Condition
str(data$House_Condition)
table(data$House_Condition)
summary(data$House_Condition)
sum(is.na(data$House_Condition))
m1 = aov(data$Sale_Price ~ data$House_Condition)
summary(m1)

#Construction_Year
str(data$Construction_Year)
summary(data$Construction_Year)
describe(data$Construction_Year)
sum(is.na(data$Construction_Year))
hist(data$Construction_Year)
table(data$Construction_Year)
#m1 = aov(data$Sale_Price ~ data$Construction_Year)
#summary(m1)
cor(data$Sale_Price, data$Construction_Year)
cor.test(data$Sale_Price, data$Construction_Year)
scatterplot(data$Sale_Price ~ data$Construction_Year,
            data = data)


#Remodel_Year
str(data$Remodel_Year)
summary(data$Remodel_Year)
table(data$Remodel_Year)
describe(data$Remodel_Year)
#sum(is.na(data$Remodel_Year))
#m1 = aov(data$Sale_Price ~ data$Remodel_Year)
summary(m1)
cor(data$Sale_Price, data$Remodel_Year)
cor.test(data$Sale_Price, data$Remodel_Year)

#Roof_Design
str(data$Roof_Design)
summary(data$Roof_Design)
sum(is.na(data$Roof_Design))
m1 = aov(data$Sale_Price ~ data$Roof_Design)
summary(m1)

#Roof_Quality
str(data$Roof_Quality)
summary(data$Roof_Quality)
sum(is.na(data$Roof_Quality))
m1 = aov(data$Sale_Price ~ data$Roof_Quality)
summary(m1)


#Exterior1st
str(data$Exterior1st)
summary(data$Exterior1st)
sum(is.na(data$Exterior1st))
m1 = aov(data$Sale_Price ~ data$Exterior1st)
summary(m1)

#Exterior2nd
str(data$Exterior2nd)
summary(data$Exterior2nd)
sum(is.na(data$Exterior2nd))
m1 = aov(data$Sale_Price ~ data$Exterior2nd)
summary(m1)


#Exterior_Material
str(data$Exterior_Material)
summary(data$Exterior_Material)
sum(is.na(data$Exterior_Material))
m1 = aov(data$Sale_Price ~ data$Exterior_Material)
summary(m1)

#Exterior_Condition
str(data$Exterior_Condition)
summary(data$Exterior_Condition)
sum(is.na(data$Exterior_Condition))
m1 = aov(data$Sale_Price ~ data$Exterior_Condition)
summary(m1)

#Foundation_Type
str(data$Foundation_Type)
summary(data$Foundation_Type)
sum(is.na(data$Foundation_Type))
m1 = aov(data$Sale_Price ~ data$Foundation_Type)
summary(m1)


#Basement_Condition
str(data$Basement_Condition)
summary(data$Basement_Condition)
sum(is.na(data$Basement_Condition))
data$Basement_Condition[is.na(data$Basement_Condition)] <- "TA"
sum(is.na(data$Basement_Condition))

m1 = aov(data$Sale_Price ~ data$Basement_Condition)
summary(m1)

#Exposure_Level
str(data$Exposure_Level)
summary(data$Exposure_Level)
sum(is.na(data$Exposure_Level))
data$Exposure_Level[is.na(data$Exposure_Level)] <- "No"
sum(is.na(data$Exposure_Level))
m1 = aov(data$Sale_Price ~ data$Exposure_Level)
summary(m1)


#BsmtFinType1
str(data$BsmtFinType1)
summary(data$BsmtFinType1)
sum(is.na(data$BsmtFinType1))
data$BsmtFinType1[is.na(data$BsmtFinType1)] <- "Unf"
sum(is.na(data$BsmtFinType1))
m1 = aov(data$Sale_Price ~ data$BsmtFinType1)
summary(m1)


#BsmtFinSF1
str(data$BsmtFinSF1)
summary(data$BsmtFinSF1)
describe(data$BsmtFinSF1)
sum(is.na(data$BsmtFinSF1))
cor(data$Sale_Price, data$BsmtFinSF1)
cor.test(data$Sale_Price, data$BsmtFinSF1)
scatterplot(data$Sale_Price ~ data$BsmtFinSF1,
            data = data)


#BsmtFinType2
str(data$BsmtFinType2)
summary(data$BsmtFinType2)
sum(is.na(data$BsmtFinType2))
data$BsmtFinType2[is.na(data$BsmtFinType2)] <- "Unf"
sum(is.na(data$BsmtFinType2))
m1 = aov(data$Sale_Price ~ data$BsmtFinType2)
summary(m1)


#BsmtFinSF2
str(data$BsmtFinSF2)
summary(data$BsmtFinSF2)
describe(data$BsmtFinSF2)
sum(is.na(data$BsmtFinSF2))
table(data$BsmtFinSF2)
cor(data$Sale_Price, data$BsmtFinSF2)

#BsmtUnfSF
str(data$BsmtUnfSF)
summary(data$BsmtUnfSF)
describe(data$BsmtUnfSF)
sum(is.na(data$BsmtUnfSF))
table(data$BsmtUnfSF)
cor(data$Sale_Price, data$BsmtUnfSF)
cor.test(data$Sale_Price, data$BsmtUnfSF)
scatterplot(data$Sale_Price ~ data$BsmtUnfSF,
            data = data)

#Total_Basement_Area
str(data$Total_Basement_Area)
summary(data$Total_Basement_Area)
table(data$Total_Basement_Area)
describe(data$Total_Basement_Area)
sum(is.na(data$Total_Basement_Area))
cor(data$Sale_Price, data$Total_Basement_Area)
cor.test(data$qq  Sale_Price, data$Total_Basement_Area)
scatterplot(data$Sale_Price ~ data$Total_Basement_Area,
            data = data)

#Heating_Type
str(data$Heating_Type)
summary(data$Heating_Type)
sum(is.na(data$Heating_Type))
m1 = aov(data$Sale_Price ~ data$Heating_Type)
summary(m1)

#Heating_Quality
str(data$Heating_Quality)
summary(data$Heating_Quality)
sum(is.na(data$Heating_Quality))
m1 = aov(data$Sale_Price ~ data$Heating_Quality)
summary(m1)

#Air_Conditioning
str(data$Air_Conditioning)
table(data$Air_Conditioning)
sum(is.na(data$Air_Conditioning))
#independent t-test
t.test(data$Sale_Price~data$Air_Conditioning, var.equal = TRUE)

#Electrical_System
str(data$Electrical_System)
summary(data$Electrical_System)
sum(is.na(data$Electrical_System))
data$Electrical_System[is.na(data$Electrical_System)] <- "SBrkr"
m1 = aov(data$Sale_Price ~ data$Electrical_System)
summary(m1)

#First_Floor_Area
str(data$First_Floor_Area)
summary(data$First_Floor_Area)
describe(data$First_Floor_Area)
sum(is.na(data$First_Floor_Area))
cor(data$Sale_Price,data$First_Floor_Area)
cor.test(data$Sale_Price,data$First_Floor_Area)
scatterplot(data$Sale_Price ~ data$First_Floor_Area,
            data = data)
#Second_Floor_Area
str(data$Second_Floor_Area)
summary(data$Second_Floor_Area)
describe(data$Second_Floor_Area)
sum(is.na(data$Second_Floor_Area))
cor(data$Sale_Price,data$Second_Floor_Area)
cor.test(data$Sale_Price,data$Second_Floor_Area)
scatterplot(data$Sale_Price ~ data$Second_Floor_Area,
            data = data)

#LowQualFinSF
str(data$LowQualFinSF)
summary(data$LowQualFinSF)
describe(data$LowQualFinSF)
sum(is.na(data$Second_Floor_Area))
table(data$LowQualFinSF)

#Grade_Living_Area
str(data$Grade_Living_Area)
summary(data$Grade_Living_Area)
table(data$Grade_Living_Area)
sum(is.na(data$Grade_Living_Area))
cor(data$Sale_Price,data$Grade_Living_Area)
cor.test(data$Sale_Price,data$Grade_Living_Area)
scatterplot(data$Sale_Price ~ data$Grade_Living_Area,
            data = data)

#Underground_Full_Bathroom
str(data$Underground_Full_Bathroom)
summary(data$Underground_Full_Bathroom)
table(data$Underground_Full_Bathroom)
sum(is.na(data$Underground_Full_Bathroom))
m1 = aov(data$Sale_Price ~ data$Underground_Full_Bathroom)
summary(m1)


#Underground_Half_Bathroom
str(data$Underground_Half_Bathroom)
summary(data$Underground_Half_Bathroom)
table(data$Underground_Half_Bathroom)
sum(is.na(data$Underground_Half_Bathroom))
m1 = aov(data$Sale_Price ~ data$Underground_Half_Bathroom)
summary(m1)


#Full_Bathroom_Above_Grade
str(data$Full_Bathroom_Above_Grade)
summary(data$Full_Bathroom_Above_Grade)
table(data$Full_Bathroom_Above_Grade)
sum(is.na(data$Full_Bathroom_Above_Grade))
m1 = aov(data$Sale_Price ~ data$Full_Bathroom_Above_Grade)
summary(m1)

#Half_Bathroom_Above_Grade
str(data$Half_Bathroom_Above_Grade)
summary(data$Half_Bathroom_Above_Grade)
table(data$Half_Bathroom_Above_Grade)
sum(is.na(data$Full_Bathroom_Above_Grade))
m1 = aov(data$Sale_Price ~ data$Half_Bathroom_Above_Grade)
summary(m1)

#Bedroom
str(data$Bedroom)
summary(data$Bedroom)
table(data$Bedroom)
sum(is.na(data$Bedroom))
m1 = aov(data$Sale_Price ~ data$Bedroom)
summary(m1)


#Kitchen above grade
str(data$Kitchen_Above_Grade)
summary(data$Kitchen_Above_Grade)
table(data$Kitchen_Above_Grade)
sum(is.na(data$Kitchen_Above_Grade))
m1 = aov(data$Sale_Price ~ data$Kitchen_Above_Grade)
summary(m1)


#Kitchen_Quality
str(data$Kitchen_Quality)
summary(data$Kitchen_Quality)
table(data$Kitchen_Quality)
sum(is.na(data$Kitchen_Quality))
m1 = aov(data$Sale_Price ~ data$Kitchen_Quality)
summary(m1)

#Rooms_Above_Grade
str(data$Rooms_Above_Grade)
summary(data$Rooms_Above_Grade)
table(data$Rooms_Above_Grade)
sum(is.na(data$Rooms_Above_Grade))
#cor(data$Sale_Price , data$Rooms_Above_Grade)
m1 = aov(data$Sale_Price ~ data$Rooms_Above_Grade)
summary(m1)

scatterplot(data$Sale_Price, data$Rooms_Above_Grade,
            data = data)
#m1 = aov(data$Sale_Price ~ data$Rooms_Above_Grade)
#m1

#Functional_Rate
str(data$Functional_Rate)
summary(data$Functional_Rate)
table(data$Functional_Rate)
sum(is.na(data$Functional_Rate))
m1 = aov(data$Sale_Price ~ data$Functional_Rate)
summary(m1)
#Fireplaces
str(data$Fireplaces)
summary(data$Fireplaces)
table(data$Fireplaces)
sum(is.na(data$Fireplaces))
m1 = aov(data$Sale_Price ~ data$Fireplaces)
summary(m1)


#Fireplace_Quality
str(data$Fireplace_Quality)
summary(data$Fireplace_Quality)





#Garage
str(data$Garage)
summary(data$Garage)
table(data$Garage)
sum(is.na(data$Garage))
data$Garage[is.na(data$Garage)] <- "Attchd"
sum(is.na(data$Garage))
m1 = aov(data$Sale_Price ~ data$Garage)
summary(m1)

#Garage_Built_Year
str(data$Garage_Built_Year)
summary(data$Garage_Built_Year)
table(data$Garage_Built_Year)
describe(data$Garage_Built_Year)
data$Garage_Built_Year[is.na(data$Garage_Built_Year)] <- median (data$Garage_Built_Year, na.rm = TRUE)
#summary(aov(data$Sale_Price ~ data$Garage_Built_Year))
cor(data$Sale_Price , data$Garage_Built_Year)
cor.test(data$Sale_Price , data$Garage_Built_Year)

#Garage_Finish_Year
str(data$Garage_Finish_Year)
summary(data$Garage_Finish_Year)
table(data$Garage_Finish_Year)
summary(aov(data$Sale_Price ~ data$Garage_Finish_Year))
data$Garage_Finish_Year[is.na(data$Garage_Finish_Year)] <- "Unf" 
cor(data$Sale_Price , data$Garage_Built_Year)
cor.test(data$Sale_Price , data$Garage_Built_Year)

#Garage_Size
str(data$Garage_Size)
summary(data$Garage_Size)
table(data$Garage_Size)
sum(is.na(data$Garage_Size))
summary(aov(data$Sale_Price ~ data$Garage_Size))

#Garage_Area
str(data$Garage_Area)
summary(data$Garage_Area)
table(data$Garage_Area)
sum(is.na(data$Garage_Area))
cor(data$Sale_Price, data$Garage_Area)
cor.test(data$Sale_Price, data$Garage_Area)
scatterplot(data$Sale_Price, data$Garage_Area,
            data = data)
summary(lm(data$Sale_Price~ data$Garage_Area))

#Garage_Qualit
str(data$Garage_Qualit)
summary(data$Garage_Qualit)
table(data$Garage_Qualit)
data$Garage_Quality[is.na(data$Garage_Quality)] <- "TA"
sum(is.na(data$Garage_Qualit))
summary(aov(data$Sale_Price ~ data$Garage_Qualit))


#Garage_Condition
str(data$Garage_Condition)
summary(data$Garage_Condition)
table(data$Garage_Condition)
sum(is.na(data$Garage_Condition))
data$Garage_Condition[is.na(data$Garage_Condition)] <- "TA"
summary(aov(data$Sale_Price ~ data$Garage_Condition))


#Pavedd_Drive
str(data$Pavedd_Drive)
summary(data$Pavedd_Drive)
table(data$Pavedd_Drive)
sum(is.na(data$Pavedd_Drive))
summary(aov(data$Sale_Price ~ data$Pavedd_Drive))

#W_Deck_Area
str(data$W_Deck_Area)
summary(data$W_Deck_Area)
table(data$W_Deck_Area)
sum(is.na(data$W_Deck_Area))
cor(data$Sale_Price, data$W_Deck_Area)
cor.test(data$Sale_Price, data$W_Deck_Area)
scatterplot(data$Sale_Price, data$W_Deck_Area,
            data = data)
summary(lm(data$Sale_Price~ data$W_Deck_Area))

#Open_Lobby_Area
str(data$Open_Lobby_Area)
summary(data$Open_Lobby_Area)
table(data$Open_Lobby_Area)
sum(is.na(data$Open_Lobby_Area))
cor(data$Sale_Price, data$Open_Lobby_Area)
cor.test(data$Sale_Price, data$Open_Lobby_Area)
scatterplot(data$Sale_Price, data$Open_Lobby_Area,
            data = data)
summary(lm(data$Sale_Price~ data$Open_Lobby_Area))
#Enclosed_Lobby_Area
str(data$Enclosed_Lobby_Area)
summary(data$Enclosed_Lobby_Area)
table(data$Enclosed_Lobby_Area)
sum(is.na(data$Enclosed_Lobby_Area))
cor(data$Sale_Price, data$Enclosed_Lobby_Area)
cor.test(data$Sale_Price, data$Enclosed_Lobby_Area)
scatterplot(data$Sale_Price, data$Enclosed_Lobby_Area,
            data = data)
summary(lm(data$Sale_Price~ data$Enclosed_Lobby_Area))
#Three_Season_Lobby_Area
str(data$Three_Season_Lobby_Area)
summary(data$Three_Season_Lobby_Area)
table(data$Three_Season_Lobby_Area)
sum(is.na(data$Three_Season_Lobby_Area))
cor(data$Sale_Price, data$Three_Season_Lobby_Area)
scatterplot(data$Sale_Price, data$Three_Season_Lobby_Area,
            data = data)
summary(lm(data$Sale_Price~ data$Three_Season_Lobby_Area))
#Screen_Lobby_Area
str(data$Screen_Lobby_Area)
summary(data$Screen_Lobby_Area)
table(data$Screen_Lobby_Area)
sum(is.na(data$Screen_Lobby_Area))
cor(data$Sale_Price, data$Screen_Lobby_Area)
cor.test(data$Sale_Price, data$Screen_Lobby_Area)
scatterplot(data$Sale_Price, data$Screen_Lobby_Area,
            data = data)
summary(lm(data$Sale_Price~ data$Screen_Lobby_Area))
#Pool_Area
str(data$Pool_Area)
summary(data$Pool_Area)
table(data$Pool_Area)
sum(is.na(data$Pool_Area))
cor(data$Sale_Price, data$Pool_Area)
scatterplot(data$Sale_Price, data$Pool_Area,
            data = data)
summary(lm(data$Sale_Price~ data$Pool_Area))

#Pool_Quality
str(data$Pool_Quality)
summary(data$Pool_Quality)
table(data$Pool_Quality)
sum(is.na(data$Pool_Quality))
summary(aov(data$Sale_Price ~ data$Pool_Quality))



#Fence_Quality
str(data$Fence_Quality)
summary(data$Fence_Quality)
table(data$Fence_Quality)
sum(is.na(data$Fence_Quality))
summary(aov(data$Sale_Price ~ data$Fence_Quality))

#Miscellaneous_Feature
str(data$Miscellaneous_Feature)
summary(data$Miscellaneous_Feature)
table(data$Miscellaneous_Feature)
sum(is.na(data$Miscellaneous_Feature))
summary(aov(data$Sale_Price ~ data$Miscellaneous_Feature))

#Miscellaneous_Value
str(data$Miscellaneous_Value)
summary(data$Miscellaneous_Value)
table(data$Miscellaneous_Value)
sum(is.na(data$Miscellaneous_Value))
cor(data$Sale_Price, data$Miscellaneous_Value)
scatterplot(data$Sale_Price, data$Miscellaneous_Value,
            data = data)
summary(lm(data$Sale_Price~ data$Miscellaneous_Value))

#Month_Sold
str(data$Month_Sold)
summary(data$Month_Sold)
table(data$Month_Sold)
sum(is.na(data$Month_Sold))
summary(aov(data$Sale_Price ~ data$Month_Sold))
cor(data$Sale_Price , data$Month_Sold)

#Year_Sold
str(data$Year_Sold)
summary(data$Year_Sold)
table(data$Year_Sold)
sum(is.na(data$Year_Sold))
summary(aov(data$Sale_Price ~ data$Year_Sold))
cor(data$Sale_Price , data$Year_Sold)
#Sale_Type
str(data$Sale_Type)
summary(data$Sale_Type)
table(data$Sale_Type)
sum(is.na(data$Sale_Type))
summary(aov(data$Sale_Price ~ data$Sale_Type))
#Sale_Condition
str(data$Sale_Condition)
summary(data$Sale_Condition)
table(data$Sale_Condition)
sum(is.na(data$Sale_Condition))
summary(aov(data$Sale_Price ~ data$Sale_Condition))

#LM1
summary(lm(data$Sale_Price ~ data$Building_Class+data$Zoning_Class+data$Lot_Extent+data$Brick_Veneer_Area+data$Brick_Veneer_Type+
          data$Basement_Height+ data$Lot_Size+data$Property_Shape+data$Land_Outline+data$Lot_Configuration+data$Neighborhood+data$Condition1+data$House_Type+data$House_Design+data$Overall_Material+data$House_Condition+data$Construction_Year+data$Remodel_Year+data$Roof_Design+
            data$Exterior1st+data$Exterior2nd+data$Exterior_Material+data$Exterior_Condition+data$Foundation_Type+data$Basement_Condition+data$Exposure_Level+data$BsmtFinSF1+data$BsmtFinType1+data$BsmtFinType2+data$BsmtUnfSF+data$Total_Basement_Area+data$Heating_Type+data$Heating_Quality+data$Air_Conditioning+data$Electrical_System+data$First_Floor_Area+data$Second_Floor_Area+data$Grade_Living_Area+data$Underground_Full_Bathroom+data$Full_Bathroom_Above_Grade+ data$Half_Bathroom_Above_Grade+ data$Bedroom_Above_Grade+ data$Kitchen_Above_Grade+data$Kitchen_Quality+data$Rooms_Above_Grade+data$Functional_Rate+data$Fireplaces+data$Garage+ data$Garage_Built_Year+data$Garage_Finish_Year+data$Garage_Size+data$Garage_Condition+data$Garage_Quality+data$Pavedd_Drive+data$Sale_Type+data$Sale_Condition ))
#LM2
summary(lm(data$Sale_Price ~ data$Zoning_Class+data$Lot_Extent+data$Basement_Height+data$Lot_Size
                      +data$Land_Outline+data$Overall_Material+data$House_Condition+data$Underground_Full_Bathroom+data$Full_Bathroom_Above_Grade
                      +data$Kitchen_Above_Grade+data$Kitchen_Quality+data$Rooms_Above_Grade+data$Fireplaces+data$Garage_Size
                      +data$Garage_Quality+data$Garage_Condition))
#LM3
summary(lm(data$Sale_Price ~ data$Basement_Height+data$Lot_Size
           +data$Land_Outline+data$Overall_Material+data$House_Condition+data$Underground_Full_Bathroom+data$Full_Bathroom_Above_Grade
           +data$Kitchen_Above_Grade+data$Kitchen_Quality+data$Rooms_Above_Grade+data$Fireplaces+data$Garage_Size
           +data$Garage_Quality))

#LM4
summary(lm(data$Sale_Price ~ data$Basement_Height+data$Lot_Size
           +data$Land_Outline+data$Overall_Material+data$House_Condition+data$Underground_Full_Bathroom+data$Full_Bathroom_Above_Grade
           +data$Kitchen_Above_Grade+data$Kitchen_Quality+data$Rooms_Above_Grade+data$Fireplaces+data$Garage_Size
           ))
       
v <-lm(data$Sale_Price ~ data$Basement_Height+data$Lot_Size
           +data$Land_Outline+data$Overall_Material+data$House_Condition+data$Underground_Full_Bathroom+data$Full_Bathroom_Above_Grade
           +data$Kitchen_Above_Grade+data$Kitchen_Quality+data$Rooms_Above_Grade+data$Fireplaces+data$Garage_Size)

library(car)
vif(v)



