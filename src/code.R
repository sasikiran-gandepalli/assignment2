library(dplyr)
diabetes <- read.csv("C:\\Users\\Sasig\\Desktop\\pds2\\rawdata//diabetes.csv")


diabetes_clean <- diabetes


sum(is.na(diabetes_clean))


diabetes_clean$Glucose <- ifelse(diabetes_clean$Glucose == 0, median(diabetes_clean$Glucose), diabetes_clean$Glucose)
diabetes_clean$BloodPressure <- ifelse(diabetes_clean$BloodPressure == 0, median(diabetes_clean$BloodPressure), diabetes_clean$BloodPressure)
diabetes_clean$SkinThickness <- ifelse(diabetes_clean$SkinThickness == 0, median(diabetes_clean$SkinThickness), diabetes_clean$SkinThickness)
diabetes_clean$Insulin <- ifelse(diabetes_clean$Insulin == 0, median(diabetes_clean$Insulin), diabetes_clean$Insulin)
diabetes_clean$BMI <- ifelse(diabetes_clean$BMI == 0, median(diabetes_clean$BMI), diabetes_clean$BMI)

library(dplyr)
diabetes_clean %>% 
  select(-Outcome) %>% 
  boxplot()


diabetes_clean <- diabetes_clean %>%
  filter(Glucose <= 200 & BloodPressure <= 120 & BMI <= 60)


summary(diabetes_clean)


write.csv(diabetes_clean, "C:\\Users\\Sasig\\Desktop\\pds2\\cleandata\\diabetes_cleaned.csv", row.names = FALSE)

diabetes_clean = read.csv("C:\\Users\\Sasig\\Desktop\\pds2\\cleandata\\diabetes_cleaned.csv")




set.seed(1234)


sample_data <- diabetes_clean[sample(nrow(diabetes_clean), 25), ]


mean_glucose <- mean(sample_data$Glucose)
max_glucose <- max(sample_data$Glucose)


result_df <- data.frame(mean_glucose, max_glucose)


write.table(result_df, file = "C:\\Users\\Sasig\\Desktop\\pds2\\results\\Mean_Max_Glucosevalues", row.names = FALSE)


pop_mean_glucose <- mean(diabetes_clean$Glucose)
pop_max_glucose <- max(diabetes_clean$Glucose)


result_df <- data.frame(pop_mean_glucose, pop_max_glucose)

write.table(result_df, file = "C:\\Users\\Sasig\\Desktop\\pds2\\results\\Mean_Max_Population", row.names = FALSE)


mean_data <- data.frame(Type = c("Population", "Sample"),
                        Glucose = c(pop_mean_glucose, mean_glucose))
library(ggplot2)
ggplot(mean_data, aes(x = Type, y = Glucose, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Mean Glucose Values") +
  xlab("") +
  ylab("Mean Glucose") +
  theme_bw()


ggsave("C:\\Users\\Sasig\\Desktop\\pds2\\results\\Mean_Comparison.png")


max_data <- data.frame(Type = c("Population", "Sample"),
                       Glucose = c(pop_max_glucose, max_glucose))

ggplot(max_data, aes(x = Type, y = Glucose, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Highest Glucose Values") +
  xlab("") +
  ylab("Highest Glucose") +
  theme_bw()

ggsave("C:\\Users\\Sasig\\Desktop\\pds2\\results\\Max_Comparison.png")





sample_bmi_percentile <- quantile(sample_data$BMI, 0.98)
pop_bmi_percentile <- quantile(diabetes_clean$BMI, 0.98)

write.table(c(sample_bmi_percentile, pop_bmi_percentile), file = "C:\\Users\\Sasig\\Desktop\\pds2\\results//bmi_percentiles.txt")

percentile_data <- data.frame(Type = c("Population", "Sample"),
                              BMI = c(pop_bmi_percentile, sample_bmi_percentile))
ggplot(percentile_data, aes(x = Type, y = BMI, color = Type)) +
  geom_point(size = 4) +
  ggtitle("Comparison of 98th Percentile of BMI Values") +
  xlab("") +
  ylab("98th Percentile of BMI") +
  theme_bw()

ggsave("C:\\Users\\Sasig\\Desktop\\pds2\\results\\compare_98thpercentile.png")




num_samples <- 500
sample_size <- 150


bootstrap_data <- matrix(nrow = num_samples, ncol = sample_size)


for (i in 1:num_samples) {
  bootstrap_sample <- sample(diabetes_clean$BloodPressure, sample_size, replace = TRUE)
  bootstrap_data[i, ] <- bootstrap_sample
}


bootstrap_means <- apply(bootstrap_data, 1, mean)
bootstrap_mean_avg <- mean(bootstrap_means)
bootstrap_mean_sd <- sd(bootstrap_means)
bootstrap_percentile <- quantile(bootstrap_data, 0.98)


result_df <- data.frame(bootstrap_mean_avg, bootstrap_mean_sd, bootstrap_percentile)


write.table(result_df, file = "C:\\Users\\Sasig\\Desktop\\pds2\\results\\bootstrap_mean_avg_sd_percentile_values", row.names = FALSE)


pop_mean <- mean(diabetes_clean$BloodPressure)
pop_sd <- sd(diabetes_clean$BloodPressure)
pop_percentile <- quantile(diabetes_clean$BloodPressure, 0.98)


result_df <- data.frame(pop_mean, pop_sd, pop_percentile)

write.table(result_df, file = "C:\\Users\\Sasig\\Desktop\\pds2\\results\\pop_mean_sd_percentile_values", row.names = FALSE)



mean_data <- data.frame(Type = c("Population", "Bootstrap"),
                        Mean = c(pop_mean, bootstrap_mean_avg))
ggplot(mean_data, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Mean BloodPressure Values") +
  xlab("") +
  ylab("Mean BloodPressure") +
  theme_bw()


ggsave("C:\\Users\\Sasig\\Desktop\\pds2\\results\\mean_bloodpressure_values_comparison.png")


sd_data <- data.frame(Type = c("Population", "Bootstrap"),
                      SD = c(pop_sd, bootstrap_mean_sd),
                      Color = c("blue", "red"))

ggplot(sd_data, aes(x = "", y = SD, fill = Type)) +
  geom_bar(stat = "identity", width = 1, alpha = 0.8) +
  ggtitle("Comparison of Standard Deviation of BloodPressure Values") +
  ylab("Standard Deviation of BloodPressure") +
  theme_bw() +
  coord_polar(theta = "y") +
  scale_fill_manual(values = sd_data$Color)

ggsave("C:\\Users\\Sasig\\Desktop\\pds2\\results\\sd_bloodpressure_comparison.png")



percentile_data <- data.frame(Type = c("Population", "Bootstrap"),
                              BloodPressure = c(pop_percentile, bootstrap_percentile))
ggplot(percentile_data, aes(x = BloodPressure, fill = Type)) +
  geom_histogram(position = "dodge", alpha = 0.8, bins = 10) +
  ggtitle("Comparison of 98th Percentile of BloodPressure Values") +
  xlab("98th Percentile of BloodPressure") +
  ylab("Count") +
  theme_bw()

ggsave("C:\\Users\\Sasig\\Desktop\\pds2\\results\\_bloodpressure_comparison.png")
