bank <- read.csv("C:/Users/ASUS/OneDrive/Desktop/bank-full.csv", sep = ",", stringsAsFactors = FALSE)

str(bank)

bank[bank == "unknown"] <- NA
sum(is.na(bank))

get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
cat_cols <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "y")
for (col in cat_cols) {
  bank[[col]][is.na(bank[[col]])] <- get_mode(bank[[col]])
}





# install.packages("modeest")  # if you haven't already
library(modeest)

cat_cols <- c("job","marital","education","default","housing","loan",
              "contact","month","poutcome","y")

for (col in cat_cols) {
  v <- bank[[col]]
  m <- modeest::mfv(na.omit(v))   # returns one or several modes
  
  m <- m[1]
  
  # if it's a factor, make sure the level exists before assignment
  if (is.factor(v) && !m %in% levels(v)) {
    levels(v) <- c(levels(v), as.character(m))
  }
  
  v[is.na(v)] <- m
  bank[[col]] <- v
}


sum(is.na(bank))

num_duplicates <- sum(duplicated(bank))
print(paste("Number of duplicate rows:", num_duplicates))


bank <- bank[!duplicated(bank), ]


count_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  sum(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR), na.rm = TRUE)
}


cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.01, na.rm = TRUE)
  Q99 <- quantile(x, 0.99, na.rm = TRUE)
  x[x < Q1] <- Q1
  x[x > Q99] <- Q99
  return(x)
}

cap_duration <- function(x) {
  Q1 <- quantile(x[x > 0], 0.01, na.rm = TRUE)
  Q99 <- quantile(x[x > 0], 0.99, na.rm = TRUE)
  x[x > 0 & x < Q1] <- Q1
  x[x > Q99] <- Q99
  return(x)
}


num_cols <- c("age", "balance", "day", "duration", "campaign", "pdays", "previous")
bank$balance <- cap_outliers(bank$balance)
bank$duration <- cap_duration(bank$duration)


print("Missing values after imputation:")
print(colSums(is.na(bank)))


for (col in cat_cols) {
  cat("\nFrequency and Percentage for", col, ":\n")
  freq <- table(bank[[col]])
  percent <- prop.table(freq) * 100
  summary_df <- data.frame(Frequency = freq, Percentage = round(percent, 2))
  print(summary_df)
}

library(dplyr)

bank$age <- as.numeric(bank$age)
bank$balance <- as.numeric(bank$balance)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)


colnames(bank) <- trimws(gsub("\"", "", colnames(bank)))


bank$age <- as.numeric(bank$age)
bank$balance <- as.numeric(bank$balance)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)


bank$age_group <- cut(
  bank$age,
  breaks = c(18, 30, 40, 50, 60, 70, 100),  # intervals
  right = FALSE,
  include.lowest = TRUE
)


numeric_summary <- bank %>%
  group_by(age_group) %>%
  summarize(
    Count = n(),
    Mean_Age = mean(age, na.rm = TRUE),
    SD_Age = sd(age, na.rm = TRUE),
    Mean_Balance = mean(balance, na.rm = TRUE),
    SD_Balance = sd(balance, na.rm = TRUE),
    Mean_Duration = mean(duration, na.rm = TRUE),
    SD_Duration = sd(duration, na.rm = TRUE),
    Mean_Campaign = mean(campaign, na.rm = TRUE),
    SD_Campaign = sd(campaign, na.rm = TRUE),
    Mean_Pdays = mean(pdays, na.rm = TRUE),
    SD_Pdays = sd(pdays, na.rm = TRUE),
    Mean_Previous = mean(previous, na.rm = TRUE),
    SD_Previous = sd(previous, na.rm = TRUE)
  )

print(numeric_summary)


categorical_summary <- bank %>%
  group_by(age_group) %>%
  summarize(
    Common_Job = get_mode(job),
    Common_Marital = get_mode(marital),
    Common_Education = get_mode(education),
    Common_Default = get_mode(default),
    Common_Contact = get_mode(contact),
    Common_Month = get_mode(month),
    Common_Poutcome = get_mode(poutcome),
    Housing_Yes_Percent = round(mean(housing == "yes") * 100, 2),
    Loan_Yes_Percent = round(mean(loan == "yes") * 100, 2),
    Subscribed_Yes_Percent = round(mean(y == "yes") * 100, 2)
  )

print(categorical_summary)

write.csv(numeric_summary, "numeric_summary1.csv", row.names = FALSE)
write.csv(categorical_summary, "categorical_summary1.csv", row.names = FALSE)

getwd()

library(ggplot2)

t.test(balance ~ y, data = bank)
ggplot(bank,aes(x=balance,fill=y))+
  geom_density(alpha=0.5)+
  labs(title= "Density Distribution of Client Balances by Subscription Outcome",
       x="Balance",
       y = "Density")+
  theme_minimal()

t.test(duration~y, data = bank)

ggplot(bank, aes(x = duration, fill = y)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Distribution of Call Duration by Subscription Outcome",
       x = "Call Duration (seconds)",
       y = "Density") +
  theme_minimal()

t.test(duration ~ y, data = bank, var.equal = TRUE)

model<-aov(balance~marital,data=bank)
summary(model)
ggplot(bank, aes(x = marital, y = balance)) +
  geom_boxplot(fill = "skyblue", color = "pink") +
  labs(title = "Distribution of Balance Across Marital Status Groups",
       x = "Marital Status",
       y = "Balance") +
  theme_minimal()


subscription_table <- table(bank$job, bank$y)
chisq.test(subscription_table)


ggplot(bank, aes(x = job, fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Subscription Rate by Job Type",
       x = "Job Type",
       y = "Proportion",
       fill = "Subscribed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(dplyr)

bank %>%
  group_by(job) %>%
  summarise(
    mean_duration = mean(duration, na.rm = TRUE),
    median_duration = median(duration, na.rm = TRUE),
    sd_duration = sd(duration, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )
library(ggplot2)

ggplot(bank, aes(x = job, y = duration, fill = job)) +
  geom_boxplot() +
  labs(title = "Boxplot of Call Duration by Job",
       x = "Job Type", y = "Call Duration (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(corrplot)
corr_matrix <- cor(bank[sapply(bank, is.numeric)], use = "complete.obs")
corrplot(corr_matrix,
         method = "circle",   
         type = "lower")


corr_matrix <- cor(bank[sapply(bank, is.numeric)], use = "complete.obs")
corrplot(corr_matrix,
         method = "circle",   
         type = "upper")

ggplot(bank)+
  aes(x = age, y = balance) +
  geom_point(color = "blue") +
  theme_minimal()

ggplot(bank, aes(x = job, y = balance)) +
  stat_summary(fun = "mean", geom = "bar", fill = "steelblue") +
  theme_minimal() 





library(ggplot2)


library(ggplot2)



# Create data frame


# Plot pie chart with auto-calculated percentages
ggplot(data, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(count / sum(count) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Subscription Outcomes") +
  scale_fill_manual(values = c("Subscribed" = "#833f70", "Not Subscribed" = "#833")) +
  theme_void()  # Removes axes, ticks, and background

ggplot(data, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(count / sum(count) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Subscription Outcomes") +
  theme_void()

# Assuming your dataset is named 'bank_data'
# and it contains columns 'age' and 'balance'

# Scatter plot of Age vs. Balance
plot(bank$age, bank$balance,
     main = "Scatter Plot of Age vs. Balance",
     xlab = "Age (years)",
     ylab = "Balance (EUR)",
     pch = 19, col = rgb(0.2, 0.4, 0.6, 0.5))




# Assuming your dataset is named 'bank_data' and has 'age' and 'balance' columns

# Filter realistic values
filtered_data <- subset(bank, age >= 18 & age <= 100 & balance >= 0 & balance <= 100000)

# Scatter plot
plot(filtered_data$age, filtered_data$balance,
     main = "Scatter Plot of Age vs. Balance",
     xlab = "Age (years)",
     ylab = "Balance (EUR)",
     col = "blue",
     pch = 19,
     cex = 0.6)

# Add grid
grid()



ages <- c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75)
balances <- c(500, 1500, 3000, 4500, 6000, 7500, 10000, 15000, 25000, 40000, 60000, 80000)

# Scatter plot
plot(ages, balances,
     main = "Scatter Plot of Age vs. Balance",
     xlab = "Age (years)",
     ylab = "Balance (EUR)",
     col = "blue",
     pch = 19,
     cex = 0.6,
     ylim = c(0, 80000))  # Match y-axis scale

# Add grid
grid()



library(ggplot2)


ggplot(bank, aes(x = balance, fill = marital)) +
  geom_density(alpha = 0.5) +
  xlim(0, 10000) +
  labs(
    title = "Density Distribution of Client Balance by Marital Status",
    x = "Balance",
    y = "Density"
  ) +
  theme_minimal()

library(ggplot2)  
ggplot(bank, aes(x=marital,y=balance,fill = marital))+ geom_boxplot() + 
  geom_jitter(shape=15, color="pink")+ 
  theme_minimal() +
  labs(
    title = "Client Balance by Marital Status",
    x = "Marital Status",
    y = "Balance"
  )



library(ggplot2)

ggplot(bank, aes(x = marital, y =balance , fill = marital)) +
  geom_boxplot() +
  geom_jitter(aes(color = marital), shape = 15) +
  theme_classic() +
  labs(
    title = "Client Balance by Marital Status",
    x = "Balance",
    y = "Marital Status"
  )




library(ggplot2)

ggplot(bank, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#833f70", color = "white") +
  labs(title = "Age Distribution of Clients", x = "Age", y = "Count") +
  theme_minimal()


# Assuming your data has: category (Yes/No) and count
ggplot(bank, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(count / sum(count) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(values = c("#833f70", "#d9b6cc")) +  # main + lighter shade
  labs(title = "Subscription Outcome") +
  theme_void()


ggplot(bank, aes(x = "", fill = y)) +   # y = subscription column (Yes/No)
  geom_bar(width = 1, color = "white") +
  coord_polar("y") +
  geom_text(
    aes(label = paste0(round(..count../sum(..count..) * 100, 1), "%")),
    stat = "count", position = position_stack(vjust = 0.5),
    color = "white"
  ) +
  scale_fill_manual(values = c("yes" = "#d9b6cc", "no" = "#833f70")) +
  labs(title = "Subscription Outcome", fill = "Subscription") +
  theme_void()



bank$age_group <- cut(bank$age,
                      breaks = c(18,30,40,50,60,70,100),
                      labels = c("18-30","30-40","40-50","50-60","60-70","70+"),
                      right = FALSE)

# Plot subscription rate by age group
ggplot(bank, aes(x = age_group, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("yes" = "#833f70", "no" = "#d9b6cc")) +
  labs(title = "Subscription Rate by Age Group",
       x = "Age Group", y = "Percentage") +
  theme_minimal()


library(ggcorrplot)

# Select numeric columns
num_data <- bank[, c("age", "balance", "duration", "campaign", "pdays", "previous")]

# Correlation matrix
corr_matrix <- cor(num_data, use = "complete.obs")

# Plot correlation heatmap
ggcorrplot(corr_matrix, 
           method = "circle", 
           lab = TRUE, 
           lab_size = 3,
           colors = c("#d9b6cc", "white", "#833f70"), # custom palette
           title = "Correlation Between Numeric Features")


library(corrplot)

num_data <- bank[, c("age", "balance", "duration", "campaign", "pdays", "previous")]
corr_matrix <- cor(num_data, use = "complete.obs")

corrplot(corr_matrix, method = "circle",
         col = colorRampPalette(c("#d9b6cc", "white", "#833f70"))(200),
         tl.cex = 0.8, title = "Correlation Between Numeric Features")



library(corrplot)

# Select numeric columns
num_data <- bank[, c("age", "balance", "duration", "campaign", "pdays", "previous")]

# Compute correlation matrix
corr_matrix <- cor(num_data, use = "complete.obs")

# Plot correlation matrix with circles and numbers inside
corrplot(corr_matrix,
         method = "circle",           # circles
         type = "upper",              # upper triangle only
         col = colorRampPalette(c("#d9b6cc", "white", "#833f70"))(200),
         tl.cex = 0.8,                # label size
         title = "Correlation Between Numeric Features"
)

library(ggplot2)
library(dplyr)
bank <- data.frame(
  Subscription = c("No Subscription", "Subscription"),
  Count = c(39922, 5289) # values from your dataset
)

# Calculate percentages
bank <- bank %>%
  mutate(Percent = Count / sum(Count) * 100,
         ypos = cumsum(Percent) - 0.5 * Percent)

library(ggplot2)

ggplot(bank, aes(x = 2, y = Percent, fill = Subscription)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  theme_void() +
  scale_fill_manual(values = c("#c29ac5", "#3e2440")) + # adjust colors
  ggtitle("Term Deposit Subscription Distribution") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.position = "bottom") +
  # make it a donut
  annotate("rect", xmin = 1.4, xmax = 1.6, ymin = -100, ymax = 100,
           fill = "white", color = "white")





library(ggplot2)
library(dplyr)

# Example data
bank <- data.frame(
  Subscription = c("No Subscription", "Subscription"),
  Count = c(39922, 5289) # replace with your own counts if different
)

# Calculate percentages and label positions
bank <- bank %>%
  mutate(Percent = Count / sum(Count) * 100,
         Label = paste0(round(Percent, 1), "%"),
         ypos = cumsum(Percent) - 0.5 * Percent)

# Donut chart
ggplot(bank, aes(x = 2, y = Percent, fill = Subscription)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  theme_void() +
  scale_fill_manual(values = c("#c29ac5", "#3e2440")) + # light/dark purple
  ggtitle("Term Deposit Subscription Distribution") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom") +
  geom_text(aes(y = ypos, label = Label), color = "white", size = 5) +
  # This makes it a donut (remove the inner circle)
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -100, ymax = 100,
           fill = "white", color = "white")








library(ggplot2)

# Sample data
bank <- data.frame(
  category = c("Subscribed", "Not Subscribed"),
  value = c(88.3, 11.7)
)

# Donut chart
ggplot(bank, aes(x = 2, y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  theme_void() +
  theme(legend.position = "right") +
  annotate("text", x = 2, y = 0, label = "") +
  guides(fill = guide_legend(title = "Subscription Status"))




# Load required libraries
library(ggplot2)
library(dplyr)

# Create the data
bank <- data.frame(
  category = c("No Subscription", "Subscription"),
  value = c(88.3, 11.7)
)

# Calculate percentages and positions
bank <- bank %>%
  arrange(desc(category)) %>%
  mutate(
    fraction = value / sum(value),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymax + ymin) / 2
  )

# Define colors: light purple and dark purple
colors <- c("No Subscription" = "#800080", "Subscription" = "#D8BFD8")

# Create the donut chart
ggplot(bank) +
  geom_rect(aes(
    ymin = ymin, ymax = ymax,
    xmin = 3, xmax = 4,
    fill = category
  )) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  scale_fill_manual(values = colors) +
  ggtitle("Term Deposit Subscription Distribution")