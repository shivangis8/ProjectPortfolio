##### title: "Analysis on employee attrition of a company"
# date: "2022-10-05"

# Load the dataset into R
library(readr)
df <- read.csv("HREmployeeAttrition.csv")

# Data cleaning
df <- subset(df, select = -c(EmployeeCount, EmployeeNumber, JobLevel, 
                             Over18, StandardHours))
head(df)

# Creating age group intervals
df$Attrition <- as.factor(df$Attrition)
df$age.grps <- cut(df$Age, c(17, 24, 34, 44, 54, 60))

# Graph 1: Age groups vs employee attrition
library(ggplot2)
library(RColorBrewer)

pl1 <- ggplot(df, aes(fill=Attrition, y=PercentSalaryHike, x=age.grps)) + 
  geom_bar(position="fill", stat="identity") + labs(x="Employee age groups", 
                                                    y="Total employees")

pl1 + scale_fill_brewer(palette = "Paired")

# Classifying company departments as unique
unique(df$Department)

# Graph 2: Department wise employee attrition
pl2 <- ggplot(df, aes(fill=Attrition, x=Department, y=PercentSalaryHike)) + 
  geom_bar(position = "fill", stat = "identity") + labs(x="Departments",
                                                        y="Total employees")

pl2 + scale_fill_brewer(palette = "RdPu")

# Create new data frame with data of only the employees that quit
df1 <- df[df$Attrition == "Yes",]
head(df1)

# Graph 3: Histogram
ggplot(df1, aes(x=YearsAtCompany)) + 
  geom_histogram(binwidth = 5, color = "black", lwd = 0.75, linetype = 1, 
                 fill = "thistle", position="identity") + 
  labs(x="Years at the company", y="Employees that quit")

# Graph 4: Income vs attrition
ggplot(df, aes(x = Age, y = MonthlyIncome, color = Attrition)) +
  geom_point(alpha=0.5, position = position_jitter()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + labs(y = "Monthly Income")

# Graph 5: Income vs education level
df$Education <- as.factor(df$Education)

pl3 <- ggplot(df, aes(x = Education, y = MonthlyIncome, color = Attrition, 
                      fill = Education)) + geom_point(alpha = 0.4,
                                                      position = "jitter") +
  geom_boxplot(alpha = 0, color = "black") + labs(y="Monthly Income")

pl3 + scale_fill_discrete(name="Education",
                          labels=c("1 Below College","2 College","3 Bachelors", 
                                   "4 Masters", "5 Doctoral"))
