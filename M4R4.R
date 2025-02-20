#importing library for functions
library(dplyr) 
library(ggplot2)
library(broom)

#import dataset combordities 
PSID <- read.csv("D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\small sample PSID (4 variables).csv")
PSID
PSID_head <- head(PSID,5)
write.csv(PSID_head, "D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\sumtable1.txt")

#converting wage to $ from cents
PSID$wage2 <- PSID$wage2/100
PSID$wage2
write.csv(PSID_head, "D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\sumtable1.txt")

#creating two vectors for gender
male <- subset(PSID, subset = (gender=="male"))$wage2
male
female <- subset(PSID, subset = (gender=="female"))$wage2
female


#checking summary for both vectors
sum1 <- summary(male)
sum_1 <- broom::tidy(sum1)
sum_1
write.csv(sum_1, "D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\sumtable1.txt")
sum2 <- summary(female)
sum_2 <- broom::tidy(sum2)
sum_2
write.csv(sum_2, "D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\sumtable1.txt")

#t-test
tt1 <- t.test(male, female, var.equal = FALSE, alternative = "two.sided",alpha = 0.01)
tt1
tidy_t_test1 <- broom::tidy(tt1)
tidy_t_test1 
write.csv(tidy_t_test1, "D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\sumtable1.txt")

#bar graph for above data tt1
ggplot(PSID, aes(x = gender, y = wage2, fill = gender)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(x = "Gender", y = "Hourly Wage (Dollars)", fill = "Gender") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

#Normal distribution curve for tt1
# Create a data frame with the t-distribution values
df_tdist <- data.frame(x = seq(-5, 5, length.out = 1000), y = dt(seq(-5, 5, length.out = 1000), df = tt1$parameter))
# Create a data frame with the t-distribution values
df_tdist <- data.frame(x = seq(-5, 5, length.out = 1000), y = dt(seq(-5, 5, length.out = 1000), df = tt1$parameter))
# And creating a plot using df_tdist values
ggplot(df_tdist, aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = tt1$estimate, color = "red", size = 1.2, linetype = "dashed") +
  geom_vline(xintercept = c(tt1$conf.int[1], tt1$conf.int[2]), color = "blue", size = 1.2, linetype = "dashed") +
  xlim(c(min(df_tdist$x), max(df_tdist$x))) +
  ggtitle("T-Distribution and Test Statistic") +
  xlab("t-value") +
  ylab("Density")



#Part2
Wage_1980 <- subset(PSID, subset = (year == 1980), select = c("id", "wage2"))
Wage_1980
Wage_1981 <- subset(PSID, subset = (year == 1981), select = c("id", "wage2"))
Wage_1981


#merge two years vector 
Wage_diff <- merge(Wage_1980, Wage_1981, by = "id", suffixes = c("_1980", "_1981"))
Wage_diff
write.csv(Wage_diff, "D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\sumtable1.txt")


#calculate difference between both wages
Wage_diff$diff <- Wage_diff$wage2_1981 - Wage_diff$wage2_1980
Wage_diff$diff
write.csv(Wage_diff$diff, "D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\sumtable1.txt")


#t-test
tt2 <- t.test(Wage_diff$wage2_1980, Wage_diff$wage2_1981, paired = TRUE, alpha = 0.01)
tt2
tidy_t_test2 <- broom::tidy(tt2)
tidy_t_test2 
write.csv(tidy_t_test2, "D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\sumtable1.txt")


# Get the mean,SD and quantiles for a standard normal distribution of the differences
diff_mean <- mean(Wage_diff$diff)
diff_mean
diff_sd <- sd(Wage_diff$diff)
x <- seq(diff_mean - 4 * diff_sd, diff_mean + 4 * diff_sd, length = 100)
y <- dnorm(x, mean = diff_mean, sd = diff_sd)

# Plotting the density curve
plot(x, y, type = "l", xlab = "Wage Difference", ylab = "Density", main = "Normal Distribution for Wage Differences")
abline(v = tt2$statistic, col = "red", lty = 2)  # Add a vertical line for the t-statistic
# Added a shaded area for the critical region
if(tt2$p.value <= 0.01/2) {
  x_left <- qnorm(0.01/2, mean = diff_mean, sd = diff_sd)
  x_right <- -x_left
  polygon(c(x_left, seq(x_left, x_right, length = 100), x_right), c(0, dnorm(seq(x_left, x_right, length = 100), mean = diff_mean, sd = diff_sd), 0), col = "gray90", border = NA)
}
