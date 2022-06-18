# FirstPart ----

all_exam_data <- read.csv("exam_data.csv", header = T)

first_exam_data <- c(all_exam_data$Exam1)
second_exam_data <- c(all_exam_data$Exam2)

first_exam_mean <- mean(first_exam_data)
first_exam_min <- min(first_exam_data)
first_exam_max <- max(first_exam_data)
first_exam_variace <- var(first_exam_data)
first_exam_median <- median(first_exam_data)
#summary(first_exam_data)
second_exam_mean <- mean(second_exam_data)
second_exam_min <- min(second_exam_data)
second_exam_max <- max(second_exam_data)
second_exam_variace <- var(second_exam_data)
second_exam_median <- median(second_exam_data)
#summary(second_exam_data)

difference_vec <- second_exam_data - first_exam_data
boxplot(difference_vec)

quantile(difference_vec)





#SecondPart ----

outcome_data <- read.csv("outcome.csv", header = T, na.strings = c("", "<NA>"))

coloumn_names_list <- c(colnames(outcome_data))
print(coloumn_names_list)
first_rows <- head(outcome_data, 10)
print(first_rows)

repeated_input_num_workclass <- table(outcome_data$workclass)
print(repeated_input_num_workclass)

repeated_country_and_income_1 <- table(outcome_data$income, outcome_data$native.country)
barplot(repeated_country_and_income_1,
        main = "\"Grouped bar plot table 1\"",
        xlab = "\"Native-country\"",
        ylab = "\"Income\"",
        col = c(rainbow(30)),
        legend.text = colnames(repeated_country_and_income_1),
        beside = T,
        args.legend = list(title = "Countries", x = "center", inset = c(-0.01, 0.02), cex = 0.37))

repeated_country_and_income_2 <- table( outcome_data$native.country, outcome_data$income)
barplot(repeated_country_and_income_2,
        main = "\"Grouped bar plot table 2\"",
        xlab = "\"Income\"",
        col = c(rainbow(30)),
        legend.text = rownames(repeated_country_and_income_2),
        beside = F,
        args.legend = list(title = "Countries", x = "topright", inset = c(-0.01, 0.02), cex = 0.5))

income_vs_education <- table(outcome_data$education, outcome_data$income)
mosaicplot(income_vs_education, main = "\"Income vs Education\"", color = rainbow(2), cex = 0.4)
age_frequency <- table(outcome_data$age)
plot(age_frequency, col = rainbow(10))