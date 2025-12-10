snippet pack
#Add essentia packages
library(readxl)
library(openxlsx)
library(writexl)
library(corrplot)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lmtest)
library(stargazer)
library(EnvStats)
library(RColorBrewer)
library(reshape2)
library(FinCal)
library(FinancialMath)
library(pander)
library(car)
library(lubridate)




snippet rename
data <- data %>%
  rename(new = old)



snippet replace
df <- df %>%
  mutate(price = str_replace_all(price, ",", "."))



snippet na_expand
df <- df %>%
  arrange(year) %>%  # Ensure data is in correct time order
  mutate(across(c(POLVOILUSDQ, PSUNOUSDM, PROILUSDQ), ~ na.approx(., na.rm = FALSE)))


snippet filter
wages <- wages_all %>%
  filter(`Reference area` == 'Romania') %>%
  select(`Reference area`,`OBS_VALUE`)



snippet year
#from monthly date to yearly
Sunflower_oil_pricesYearly <- Sunflower_oil_prices %>%
  mutate(observation_date = as.Date(observation_date, format = "%Y-%m-%d"),
         year = year(observation_date)) %>%
  group_by(year) %>%
  summarise(avg_value = mean(PSUNOUSDM, na.rm = TRUE))


snippet delete_row_condit
df <- df %>% filter(format(datetime_column, "%Y") != "2023")

snippet delete_row
#single
df <- df[-2, ]

#several
df <- df[-c(2, 4), ]

snippet date
# Sample data frame
df <- data.frame(datetime_column = as.POSIXct(c("2005-10-01 00:00:00", "2023-05-15 12:30:45")))

# OR Filter by year 2023
df_filtered <- df %>% filter(format(datetime_column, "%Y") == "2023")
print(df_filtered)

snippet filter
filter <- data %>% filter(Value == 720)

snippet head
colnames(data) <- data[1, ]  # Assign first row as column names
data <- data[-1, ]  

snippet residuals_plot
plot(residuals(model))

snippet excel
#Import excel file
data <- read_excel(path = 'path.xlsx')


snippet save_excel
# Установка директории для сохранения файла
file_path <- "/Users/grigoriiegorov/Library/Mobile Documents/com~apple~CloudDocs/HSE/Quantitative methods in Economy/R/sheet_nd_summary.xlsx"
# Сохранение файла в указанную директорию
write_xlsx(sheet_nd_summary, file_path)

snippet mysummary
#Full statistical summury of all of the table's columns
summarize(across(where(is.numeric), list(
  mean = mean,
  median = median,
  sd = sd,
  var = var,
  se = ~ sd(.) / sqrt(length(.))
)))


snippet linear_model
#Create a linear model
<- lm(y ~ x1+x2, data = )

snippet linear_model_summary
#Summarize stats of linear model
summary(data_lm)
anova(data_lm)
plot(data_lm)

snippet gg
#Add ggplot with some optional tools
ggplot(data = datax, aes(x=Date, y=Amount))+
  geom_histogram(stat = "identity", colour="black", fill="white")+
  theme_minimal()+
  labs(x="Годы", y="Количество санкций")+
  # Изменение положения подписи данных на Х
  theme(axis.text.x = element_text(size=7, angle = 45, hjust = 0.95, vjust=0.95))+
  # Что показывать на Х
  scale_x_discrete(breaks=interval, labels = interval)


snippet gg_circle
#Create a circle plot
ggplot(data = datac, aes(x="", y= Share, fill= Companies))+
  geom_bar(stat = "identity", width = 10, show.legend = FALSE)+
  coord_polar("y", start = 0)+
  labs(x = "", fill = NULL)+
  #Change colours
  scale_fill_brewer(palette = "Greys")+
  theme_void()+
  #labels settings 
  geom_text( aes (label = paste0(Share)), position = position_stack(vjust= 0.5 )) +
  labs(x = NULL, y = NULL, fill = NULL) 


snippet mmm
# Calculate mode, mediane, and mean
mean_value <- mean(data$value)
median_value <- median(data$value)
mode_value <- as.numeric(names(sort(table(data$value), decreasing=TRUE))[1])


snippet mmm_plot
#Add mmm stat to the plot exists
geom_vline(aes(xintercept = mean_value), color = 'red', linetype = 'dashed', size = 1)+
  geom_vline(aes(xintercept = median_value), color = 'green', linetype = 'dashed', size = 1)+
  geom_vline(aes(xintercept = mode_value), color = 'red', linetype = 'dashed', size = 1)+
  annotate("text", x = mean_value, y = 1, label = sprintf("Mean: %.2f", mean_value), color = "red", vjust = -10) +
  annotate("text", x = median_value, y = 1, label = sprintf("Median: %.2f", median_value), color = "green", vjust = -30)+
  annotate("text", x = mode_value, y = 1, label = sprintf("Mode: %.2f", mode_value), color = "green", vjust = -30)


snippet scewness_coef
skewness(data$AMD, na.rm = FALSE, method = "fisher", l.moment.method = "unbiased", 
         plot.pos.cons = c(a = 0.35, b = 0))


snippet interval
# Create intervals. ig each 10th
interval <- datax$Date[seq(1, nrow(datax), by=10)]



snippet plots
par(mfrow = c(2, 2))
snippet plot
par(mfrow = c(1, 1))


snippet lines
#long data
data_melt <- melt(data, id.vars = 'date')

#plot
ggplot(data = data_melt, aes(x=date, y=value, color = variable))+
  geom_line(stat = "identity")+
  theme_minimal()


snippet long_data
#long data
data_melt <- melt(data, id.vars = 'date')


snippet histogram
hist(data$`Standard deviation of equity`,
     col = 'orange',
     border = 'black',
     main = 'Histogtam',
     ylab = 'Frequensy',
     xlab = 'Deviation')
lines(density(data$`Standard deviation of equity`), 
      col = "red", 
      lwd =2)

snippet boxplot
boxplot(data$`Standard deviation of equity`,
        col = 'orange',
        border = 'orange4',
        main = 'The Boxplot of Deviation')




snippet filter
data_f <- data %>%
  filter(str_detect(Должность, regex("key", ignore_case = TRUE))) %>%
  
  
  
  
  snippet get_share
library(moexer)
sber_candles <- get_candles("RAGR", interval = "weekly", from = '2020-01-01')


snippet conditional_multiplicatiopn
df <- df %>%
  mutate(Result = ifelse(grepl("text", A), B * C, NA))


snippet index 
# Add a row index to use as the x-axis
data$ <- 1:nrow(data)


snippet rm_rows_conditionally

row_name1 <- "ПЛАНОВЫЕ ПРОСТОИ"
row_number1 <- which(df[, 1] == row_name1)

# Удалить все данные после найденной строки
if (length(row_number1) > 0) {
  df <- df[(row_number1):nrow(df), ]
}

# Найти строку с именем "Ежемесячная инвентаризация"
row_name <- "неучтенное время, мин"
row_number <- which(df[, 1] == row_name)

# Удалить все данные после найденной строки
if (length(row_number) > 0) {
  df <- df[1:row_number, ]
}



snippet na_zero
data <- data %>%
  replace_na(list(colname = 0))


snippet select
x <- data %>% select('2', '7')


snippet multyplication
# Умножение столбца x на каждый столбец в dataframe
result <- as.data.frame(lapply(dataframe, function(col) col * x$x))


snippet na_delete
data_long <- data_long %>%
  drop_na(Value)

snippet irr
first-year_value <-
  time <- c()
Total_Cfs	<- c() 

IRR <- IRR(first-year_value, Total_Cfs, time, TRUE)
IRR


snippet rate_calc
rate_calc <- function(nper, pmt, pv, fv = 0, type = 0) {
  # Objective function to solve for rate
  f <- function(rate) {
    pv + pmt * ((1 + rate * type) * (1 - (1 + rate)^(-nper)) / rate) + fv * (1 + rate)^(-nper)
  }
  # Use uniroot to find the root (rate) in a reasonable range
  result <- uniroot(f, c(0, 1), tol = 1e-8)
  return(result$root)
}



snippet npv
first-year_value <- 
  Total_CFs <- c()
time <- c()
i <- 
  NPV(first-year_value, Total_CFs, time, i, TRUE)


snippet var
# Given data
x <- c(2, 3, 4, 5)
p <- c(0.1, 0.4, 0.3, 0.2)

# Ensure probabilities sum to 1
if (abs(sum(p) - 1) > .Machine$double.eps^0.5) {
  stop("Probabilities do not sum to 1.")
}

# Calculate expected value
expected_value <- sum(x * p)
print(paste("Expected Value (Mean):", expected_value))

# Calculate expected value of X squared
expected_value_sq <- sum((x^2) * p)
print(paste("Expected Value of X squared:", expected_value_sq))

# Calculate variance
variance <- expected_value_sq - (expected_value)^2
print(paste("Variance:", variance))






snippet tg
plot_tg <- function(color = "red", fill = TRUE, title = "Grigorii + Taisiia =") {
  # Generate a sequence of t values
  t <- seq(0, 2 * pi, length.out = 1000)
  
  # Parametric equations for the heart shape
  x <- 16 * sin(t)^3
  y <- 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)
  
  # Plot the heart
  plot(x, y, type = "l", col = color, lwd = 2, 
       xlab = "", ylab = "", main = title, 
       xlim = c(-20, 20), ylim = c(-20, 20), asp = 1, axes = FALSE)
  
  # Fill the heart shape if desired
  if (fill) {
    polygon(x, y, col = color, border = NA)
  }
  
  # Add grid and axes if needed
  box()
}

# Use the function to plot
plot_tg()






snippet unique
#Unique values in a particular column

unique_values <- unique(data$machine)
unique_values


snippet unique_numb
#The number of unique values in a particular column

num_unique_values <- length(unique(data$machine))
num_unique_values





snippet binary_check
df <- mortality_df
# Check for binary variables (factors or numeric with two unique values)
binary_vars <- names(df)[sapply(df, function(col) {
  (is.factor(col) && length(levels(col)) == 2) || (is.numeric(col) && length(unique(col)) == 2)
})]

# Display binary variables
binary_vars

# Combine binary variable names into a single comma-separated string
binary_vars_string <- paste(binary_vars, collapse = ", ")

# Print the result
cat(binary_vars_string)



snippet comma
# Combine binary variable names into a single comma-separated string
binary_vars_string <- paste(binary_vars, collapse = ", ")

# Print the result
cat(binary_vars_string)



snippet count
# Count positions containing keywords, grouped by business and department
result <- data %>%
  ilter(str_detect(`Должность`, str_c(keywords, collapse = "|"))) %>%
  group_by(`Бизнес направление для УШР`, `Управленческая структура`) %>%
  summarise(count = n(), .groups = "drop")



snippet merge_files_excel
library(readxl)
library(openxlsx)

# Folder containing your Excel files
folder <- "/Users/grigoriiegorov/Desktop/my_excels/"

# List all .xlsx files in folder
files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE)

# Create new workbook
wb <- createWorkbook()

# Loop through files and add each to a new sheet
for (f in files) {
  sheet_name <- tools::file_path_sans_ext(basename(f))  # filename as sheet name
  
  addWorksheet(wb, sheet_name)
  
  # Read first sheet of the file
  df <- read_excel(f)
  
  writeData(wb, sheet_name, df)
}

# Save final workbook
saveWorkbook(wb, "/Users/grigoriiegorov/Desktop/merged.xlsx", overwrite = TRUE)

