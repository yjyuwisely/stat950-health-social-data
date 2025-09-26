## 1. Introduction (5 Marks)
Data <- read.csv("ais_modified.csv", header = T)
head(Data, 10)

# Check the structure of the dataset  
str(Data)  

# See a summary of the dataset  
summary(Data)  

# Check for missing values  
colSums(is.na(Data))  

# Number of athletes (rows in the dataset)  
nrow(Data)  

# Number of unique sports  
length(unique(Data$sport))  

# List of sports  
unique(Data$sport)  


# Variable descriptions:
# rcc: red blood cell count
# wcc: white blood cell count
# hc: hematocrit (%)
# hg: haemoglobin concentration (g/dL)
# ferr: plasma ferritin (ng/mL)
# ssf: sum of skin folds
# pcBfat: percent body fat
# lbm: lean body mass (kg)
# ht: height (cm)
# wt: weight (kg)

Data_clean <- subset(Data, select = -c(Dictionary, X, X.1))
str(Data_clean)
head(Data_clean)


## 2. Sport by Sex (10 Marks) 

# Number of athletes and sports  
n_athletes <- nrow(Data_clean)  
n_sports <- length(unique(Data_clean$sport))  

# List of unique sports  
unique_sports <- unique(Data_clean$sport)  
print(paste("Number of athletes:", n_athletes))  
print(paste("Number of sports:", n_sports))  
print(unique_sports)  

# Get counts by sex  
table(Data_clean$sex)  

# Create a contingency table of sports by sex
sport_sex_table <- table(Data_clean$sport, Data_clean$sex)

# Add row and column totals
sport_sex_table_with_totals <- addmargins(sport_sex_table)

# View the table with totals
print(sport_sex_table_with_totals)


# Create a barplot
# change all sport names in the bar chart using that exact mapping
library(ggplot2)  
library(dplyr)
sport_sex_df <- as.data.frame(table(Data_clean$sport, Data_clean$sex))  
names(sport_sex_df) <- c("Sport", "Sex", "Count")  

sport_sex_df <- sport_sex_df %>%
  mutate(
    Sport = recode(Sport,
                   "B_Ball" = "Basketball",
                   "Gym" = "Gymnastics",
                   "Netball" = "Netball",
                   "Row" = "Rowing",
                   "Swim" = "Swimming",
                   "Tennis" = "Tennis",
                   "T_Sprnt" = "Track (m) 100–400",
                   "T_400m" = "Track (m) >400",
                   "Field" = "Field",
                   "W_Polo" = "Waterpolo"
    ),
    Sex = recode(Sex,
                 "f" = "Female",
                 "m" = "Male"
    )
  )

ggplot(sport_sex_df, aes(x = Sport, y = Count, fill = Sex)) +  
  geom_bar(stat = "identity", position = "dodge") +  
  #scale_fill_manual(values = c("Female" = "lightpink", "Male" = "#00BFC4")) +  
  theme_minimal() +  
  labs(
    # title = "Count of Athletes by Sport and Sex",
    x = "Sport",
    y = "Number of Athletes"
  ) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 3. Haemoglobin Concentration by Sex (15 Marks)
# Create a summary statistics table for haemoglobin by sex  
hg_stats <- aggregate(hg ~ sex, data = Data_clean,   
                      FUN = function(x) c(  
                        Mean = mean(x),  
                        SD = sd(x),  
                        Min = min(x),  
                        Q1 = quantile(x, 0.25),  
                        Median = median(x),  
                        Q3 = quantile(x, 0.75),  
                        Max = max(x)  
                      ))  
print(hg_stats)  

# Set consistent breaks for both histograms
breaks_seq <- seq(11, 20, by = 0.5)
# Creating side-by-side histograms  
par(mfrow = c(1, 2))

# Female histogram
hist(Data_clean$hg[Data_clean$sex == "f"],
     main = "Female Athletes",
     xlab = "Haemoglobin (g/dL)",
     ylab = "Frequency",
     col = "lightpink", #"#F8766D",
     xlim = c(10, 20),
     ylim = c(0, 50),
     breaks = breaks_seq
     )

# Male histogram
hist(Data_clean$hg[Data_clean$sex == "m"],
     main = "Male Athletes",
     xlab = "Haemoglobin (g/dL)",
     ylab = "Frequency",
     col = "lightblue", #"#00BFC4",
     xlim = c(10, 20),
     ylim = c(0, 50),
     breaks = breaks_seq
     )

# Reset layout
par(mfrow = c(1, 1))


# Creating boxplots  
ggplot(Data_clean, aes(x = sex, y = hg, fill = sex)) +  
  geom_boxplot() +
  scale_x_discrete(labels = c("f" = "Female", "m" = "Male")) +
  scale_fill_manual(values = c("f" = "lightpink", "m" = "lightblue")) +
  labs(#title = "Boxplot of Haemoglobin Concentration by Sex",  
       x = "Sex", y = "Haemoglobin (g/dL)") +  
  theme_minimal() + theme(legend.position = "none")  # Hides legend


## 4. Body Mass Index (15 Marks)

# Calculate BMI from height and weight  
# BMI = weight (kg) / height (m)^2  
Data_clean$BMI <- Data_clean$wt / ((Data_clean$ht/100)^2)  

# Summary statistics for BMI  
bmi_summary <- summary(Data_clean$BMI)  
print(bmi_summary)  
sd_bmi <- sd(Data_clean$BMI)  
print(paste("Standard Deviation of BMI:", sd_bmi))  

# Boxplot of BMI  
boxplot(BMI ~ sex, data = Data_clean,  
        main = "Boxplot of BMI by Sex",  
        xlab = "Sex", ylab = "BMI",  
        col = c("pink", "lightblue"))  

# Boxplot of BMI using ggplot2
ggplot(Data_clean, aes(x = sex, y = BMI, fill = sex)) +  
  geom_boxplot() +
  scale_x_discrete(labels = c("f" = "Female", "m" = "Male")) +
  scale_fill_manual(values = c("f" = "lightpink", "m" = "lightblue")) +
  labs(
    # title = "Boxplot of BMI by Sex",
    x = "Sex",
    y = "Body Mass Index (BMI)"
  ) +  
  theme_minimal() +
  theme(legend.position = "none")  # Hides legend


# Scatter plot of BMI against lean body mass  
plot(Data_clean$lbm, Data_clean$BMI,  
     main = "Scatter Plot of BMI vs Lean Body Mass",  
     xlab = "Lean Body Mass (kg)", ylab = "BMI",  
     col = ifelse(Data_clean$sex == "f", "pink", "blue"),  
     pch = 19)  
legend("topright", legend = c("Female", "Male"),  
       col = c("pink", "blue"), pch = 19)  

# Scatter plot of BMI against lean body mass using ggplot2
ggplot(Data_clean, aes(x = lbm, y = BMI, colour = sex)) +  
  geom_point(size = 2, alpha = 0.8) +
  scale_colour_manual(values = c("f" = "lightpink", "m" = "blue"), 
                      labels = c("Female", "Male"),
                      name = "Sex") +
  labs(
    x = "Lean Body Mass (kg)", 
    y = "Body Mass Index (BMI)",
    #title = "Scatter Plot of BMI vs Lean Body Mass"
  ) +
  theme_minimal()

# Identify potentially obese individuals (BMI > 30)  
obese_count <- sum(Data_clean$BMI > 30, na.rm = TRUE)  
print(paste("Number of athletes with BMI > 30:", obese_count))  