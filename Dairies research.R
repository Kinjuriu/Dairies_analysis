# Merge three excel files into a data frame

library("tidyverse")
library("readxl")
library("dplyr")
library("regclass")

data1 <- read_excel("xlsx files to merge/Biweekly1 Final.xlsx")
data2 <- read_excel("xlsx files to merge/Biweekly2 Final.xlsx")
data3 <- read_excel("xlsx files to merge/Biweekly3__Final_2016_18_11_10_12.0.xlsx")

data_merged <-merge(data1,data2,all.x = TRUE, all.y = TRUE)

data_merged2<-merge(data_merged,data3,all.x = TRUE, all.y = TRUE)

data_frame_merged <- merge(data_merged, data_merged2, by="Respondent_ID")

# Subset the data frame

income_earned <- select(filter(data_frame_merged), c("A_4_1.x", "A_4_2.x", "A_4_3.x", "A_4_4.x", "A_4_5.x"))

# Rename column names
names(income_earned)[names(income_earned) == "A_4_1.x"] <- "Working on other people’s farms"
names(income_earned)[names(income_earned) == "A_4_2.x"] <- "Casual labour"
names(income_earned)[names(income_earned) == "A_4_3.x"] <- "Employment at small business"
names(income_earned)[names(income_earned) == "A_4_4.x"] <- "formal employment"
names(income_earned)[names(income_earned) == "A_4_5.x"] <- "Other employment ( specify)"

colnames(income_earned)

# Create a frequency table

data.table(long_income <- melt(data = setDT(income_earned),
                               variable.name = "income_source",
                               value.name = "frequency"))

#convert to long data

filtered_long_income <- filter(long_income, long_income$frequency==1)

freq_table <- table(filtered_long_income$income_source)

# Create a pie chart for freq_table

freq_table <- table(filtered_long_income$income_source)

my_pie_chart <- c(207, 506, 170, 150, 56)
labels <- c("	
Working on other people’s farms", "Casual labour", "Employment at small business", "formal employment", "Other employment ( specify)")
piepercent<- round(100*my_pie_chart/sum(my_pie_chart), 1)

pie(my_pie_chart, labels = piepercent, main = "Income Sources",col = rainbow(length(my_pie_chart)))
legend("topright", c("Working on other people’s farms","Casual labour","Employment at small business","formal employment", "Other employment ( specify)"), cex = 0.6,
       fill = rainbow(length(my_pie_chart)))

# Make a histogram

ggplot(filtered_long_income)+ 
  geom_bar(aes(x=income_source))+ggtitle("Count of respondents having an income earning activity")

# Finding mean amd median for savings tools

savings_tools <- select(filter(data_frame_merged), c("Keeping money at home_Q_12.x", "On the body/in clothes/in wallet_Q_12.x", "Lend to others_Q_12.x", "Buy something to sell later_Q_12.x", "Savings group_Q_12.x", "MDI (microfinance deposit taking institution)_Q_12.x", "Micro finance institution_Q_12.x", "Bank account_Q_12.x", "Buy stock (e.g. to stock a business reserves)_Q_12.x", "Buy cattle or similar animals_Q_12.x", "mobile money_Q_12.x", "other, please specify_Q_12.x", "None_Q_12.x", "Does not want to answer_Q_12.x"))
class(savings_tools) 

# Find the mean of the first column
mean(savings_tools$`Keeping money at home_Q_12.x`)

# Find the median of the first column
median(savings_tools$`Keeping money at home_Q_12.x`)

#We have calculated the mean for multiple columns
mean_que12 <- savings_tools %>% 
  summarise_if(is.numeric, mean)

#Calculate the media for the multiple columns
median_que12 <- savings_tools %>% 
  summarise_if(is.numeric, median)

# Find the level of stress associated with the ppi score

segmented_data <- read_xlsx("data-raw/segmentation variables.xlsx")
class(segmented_data)

associate(data_frame_merged$Q_91.x~segmented_data$ppicut, permutations = 500,  plot = TRUE, classic = FALSE, 
          cex.leg=0.7, n.levels=N5,prompt=TRUE)


# Average time surveyor uses for interviews

df_with_date <- data_frame_merged
#convert strings to date
df_with_date$VEnd.x <- as.POSIXct(df_with_date$VEnd.x, format = '%d/%m/%y %H:%M')
df_with_date$VStart.x <- as.POSIXct(df_with_date$VStart.x, format = '%d/%m/%y %H:%M')


data_time_difference  <- difftime(df_with_date$VEnd.x,df_with_date$VStart.x, units = "mins") 

#df_with_date$time_difference <- data_time_difference
dff <- data.frame(c(df_with_date$Srvyr.x),c(data_time_difference))

table(dff$c.df_with_date.Srvyr.x.)

# Sampling methods

# Simple random sampling method selects random samples from a process or population where every unit has the same probability of getting selected. 

# This is the most direct method of probability sampling.
# The researcher randomly selects a subset of participants from a population.
random_sample <- sample(data_frame_merged$Respondent_ID, 1000)

random_sample
# Each member of the population has an equal chance of being select. Data is then collected from as large a percentage as possible of this random subset.
# Get the simple random sample mean
random_sample_mean  <- round(mean(random_sample), digits=4)
print(paste("Simple random sample mean: ", random_sample_mean))

# Dealing with NULL values

# This would be used to filter out missing values in a column therefore retaining the records which do not have NA in the Q_4_S.x column and Q_4_S3.x
df_not_na <- data_frame_merged %>%
  filter(!is.na(Q_4_S.x) & !is.na(Q_4_S3.x))

# Return observations where there are no NA values in the Q_4_S.x column, use the complete cases function

no_na_values <- data_frame_merged %>%
  filter(complete.cases(Q_4_S.x))

# Remove all columns with NA values

remove_na_columns <- data_frame_merged %>% select_if(colSums(!is.na(.)) == nrow(data_frame_merged))



