library(dplyr)
library(magrittr)
library(tidyr)
library(arsenal)
library(SnowballC)
library(wordcloud)
library(tm)
library(ggplot2)
library(scales)
library(gridExtra)
library(RColorBrewer)
library(wordcloud)
library(readr)
library(stringr)
library(breakDown)
library(treemapify)
library(wesanderson)
library(lubridate)



h1b_stats <- read.csv("H-1B_2019.csv", stringsAsFactors = F)
Main.df <- h1b_stats[,c("CASE_STATUS", 
                        "CASE_SUBMITTED",
                        "VISA_CLASS", "DECISION_DATE"
                        , "EMPLOYER_STATE", "EMPLOYER_NAME","WAGE_RATE_OF_PAY_FROM",
                        "JOB_TITLE","SOC_NAME",
                        "PREVAILING_WAGE", "PW_UNIT_OF_PAY",
                        "WAGE_RATE_OF_PAY_TO", 
                        "H1B_DEPENDENT", "WAGE_UNIT_OF_PAY",
                         "WORKSITE_STATE")]

rm(h1b_stats)

# Main.df for all 
Main.df <- Main.df %>% filter(VISA_CLASS == "H-1B")
Main.df <- Main.df %>% mutate_all(na_if,"")


# Removing weekly, hourly monthly as Year is highest in Unit of Pay 
df <- Main.df %>% filter(WAGE_UNIT_OF_PAY == "Year")    
df <- df %>% filter(PW_UNIT_OF_PAY == "Year")

# Replacing wage rate if to column's NA values with WAGE RATE OF PAY FROM
df$WAGE_RATE_OF_PAY_FROM[is.na(df$WAGE_RATE_OF_PAY_FROM)] <- 
df$WAGE_RATE_OF_PAY_TO


# Removing dollar sign
df <- df %>% drop_na(WAGE_RATE_OF_PAY_FROM)
df$WAGE_RATE_OF_PAY_FROM <-  as.numeric(gsub
                                             ("[^0-9.]","", df$WAGE_RATE_OF_PAY_FROM))
df$WAGE_RATE_OF_PAY_TO <-  as.numeric(gsub
                                           ("[^0-9.]","", df$WAGE_RATE_OF_PAY_TO))
df$PREVAILING_WAGE <-  as.numeric(gsub
                                       ("[^0-9.]","", df$PREVAILING_WAGE))
# Creating two df from above to seprate NA of wage rate of pay to 
df <- df %>% arrange(WAGE_RATE_OF_PAY_TO)
df_1 <- df[1:228227,]
df_2 <- df[228228:431428, ]

# new wage variable

df_1$Wage <- df_1$WAGE_RATE_OF_PAY_TO

sum(is.na(df_1$Wage))
summary(df$Wage)
# new wage variable for df where wage rate of pay to had NA 
df_2$Wage <- df_2$WAGE_RATE_OF_PAY_FROM

# Binding above df

df <- rbind(df_2, df_1)



df$diff <- df$Wage - df$PREVAILING_WAGE
df <- df %>% arrange(diff)
df <- df[-c(1,2),] 

# cleaning extra data frame
rm(df_1)
rm(df_2)

summary(df$diff)


# creating seprate data frame for plot 1 
df_plot <- df %>% filter(CASE_STATUS == "CERTIFIED" )
df_plot <- df_plot[, c("Wage", "SOC_NAME")]
df_plot <- df_plot %>% group_by(SOC_NAME) %>% summarise(med = median(Wage)) 
df_plot <- df_plot %>% arrange(desc(med))
df_plot <- df_plot[1:10,]
# top 10 Job categories according to the wage 


df_plot$SOC_NAME <-  str_wrap(df_plot$SOC_NAME, width = 3)
##### OBJECTIVE 1 PLOT ######

ggplot(df_plot, aes(x = reorder(SOC_NAME, med), y = med))+
                   geom_bar(stat = "identity") +
                   xlab("Job Title")+ 
                   ylab("Wage")+
                   ggtitle("Highest Wage Paying Jobs") +
                   scale_y_continuous(labels = dollar) + 
                   theme(text = element_text(size=10,
                                             face = "bold"), 
                         plot.title = element_text(hjust = 0.5), 
                         axis.title.y = element_blank(), 
                         axis.title.x = element_text(vjust = 1, hjust = 0.5))  


rm(df_plot)
# Generating treemap of top 30 employers sponsering H1B


mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 12) 
             , brewer.pal(name = "PuBu", n = 3), brewer.pal(name = "YlOrBr", 
                                                            n = 7))

employer_name <- df %>% group_by(EMPLOYER_NAME) %>% summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% slice(1:50)

employer_name <- employer_name[1:30,]
colnames(employer_name)[2] <- "Count"
employer_name$EMPLOYER_NAME <- str_wrap(employer_name$EMPLOYER_NAME, width = 10)

ggplot(employer_name, aes(fill = EMPLOYER_NAME, area = Count, 
                          label = EMPLOYER_NAME )) + 
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre") + 
  theme(legend.position = "right") +
  scale_color_manual(values = mycolors)


rm(employer_name)
rm(df)
rm(mycolors)

# Making of quarterly 

denied <- Main.df %>% filter(CASE_STATUS == "DENIED")


denied <- denied[,c("CASE_STATUS", "EMPLOYER_NAME", "CASE_SUBMITTED", "H1B_DEPENDENT")]

denied$CASE_SUBMITTED <- dmy(denied$CASE_SUBMITTED)
denied_list <- split(denied, cut(as.Date(denied$CASE_SUBMITTED), "quarter" ))
Q3_2018 <- data.frame(denied_list$`2018-07-01`)
Q4_2018 <- data.frame(denied_list$`2018-10-01`)
Q1_2019 <- data.frame(denied_list$`2019-01-01`)
Q2_2019 <- data.frame(denied_list$`2019-04-01`)
Q3_2018$Quarter <- "2018-3" 
Q4_2018$Quarter <- "2018-4"
Q1_2019$Quarter <- "2019-1"
Q2_2019$Quarter <- "2019-2"
denied_all <- rbind(Q3_2018, Q4_2018, Q1_2019, Q2_2019)
denied_all_1 <- denied_all %>% group_by(Quarter, EMPLOYER_NAME) %>% summarise(count = n())
colnames(denied_all_1)[3] <- "count_denied" 


denied_all


accepted <- Main.df %>% filter(CASE_STATUS == "CERTIFIED")


accepted <- accepted[,c("CASE_STATUS", "EMPLOYER_NAME", 
                        "CASE_SUBMITTED","H1B_DEPENDENT")]


accepted$CASE_SUBMITTED <- dmy(accepted$CASE_SUBMITTED)
accepted <- split(accepted, cut(as.Date(accepted$CASE_SUBMITTED), "quarter" ))
Q3_2018_A <- data.frame(accepted$`2018-07-01`)
Q4_2018_A <- data.frame(accepted$`2018-10-01`)
Q1_2019_A <- data.frame(accepted$`2019-01-01`)
Q2_2019_A <- data.frame(accepted$`2019-04-01`)
Q3_2018_A$Quarter <- "2018-3" 
Q4_2018_A$Quarter <- "2018-4"
Q1_2019_A$Quarter <- "2019-1"
Q2_2019_A$Quarter <- "2019-2"
certified_all <- rbind(Q3_2018_A, Q4_2018_A, Q1_2019_A, Q2_2019_A)
certified_all_1 <- certified_all %>% group_by(Quarter, EMPLOYER_NAME) %>% 
  summarise(count = n())



# joining both columns 
all <- left_join(certified_all_1, denied_all_1)

all$count_denied[is.na(all$count_denied)] <- 0
all$reject_per <- (all$count_denied/(all$count + all$count))*100

q_reject <- all %>% group_by(Quarter) %>% 
                    summarise(per = mean(reject_per))
q_reject$reject <- "reject %" 

# 3rd Plot

ggplot(q_reject, aes(x = Quarter, per, group = reject )) + 
  geom_line(aes(color = reject)) + geom_point(aes(color = reject)) + 
  scale_y_continuous(limits = c(0,1.5) ) + 
  ggtitle("Rejection rate across four Quarters") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=10, face = "bold")) +
  xlab("Quarters") + ylab("Percentage % ") +
  scale_color_manual(values = "Blue")
scale_color_manual(values = wes_palette(n = 1, "BottleRocket2"))



# Creating an index for top employers sponsoring h1b visa 

top_applicants <- Main.df %>% 
  group_by(EMPLOYER_NAME) %>% summarise(ct = n()) %>%
  arrange(desc(ct)) %>%
  slice(1:50)

all_index <- top_applicants[1:20,]
all_index <- all_index$EMPLOYER_NAME

# Filtering by employers sponsoring h1b visa the most 

top_20 <- all %>% filter(EMPLOYER_NAME %in% all_index)
top_20 <- top_20[,c(1,2,5)]
top_20$EMPLOYER_NAME <- str_wrap(top_20$EMPLOYER_NAME, width = 10)
top_20_index <- top_20 %>% group_by(EMPLOYER_NAME) %>% 
  summarise(avg = mean(reject_per)) %>%
  arrange(desc(avg)) %>% slice(1:10)
top_20 <- top_20 %>% filter(EMPLOYER_NAME %in% top_20_index$EMPLOYER_NAME)

#Fourth Plot 


ggplot(top_20, aes(EMPLOYER_NAME,reject_per, group = Quarter, fill = Quarter)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.5) + 
  theme(text = element_text(size=10, face = "bold"), 
        plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), legend.position = c(.96,.89) )  + 
  ylab("Percentage %") +
  ggtitle("Rejection Rate According to top 10 Employers") +
  scale_fill_manual(values=wes_palette(n=4, name="Rushmore1")) 


rm(certified_all_1)
rm(denied_all_1)
rm(all)

# Dependend


certified_all <- certified_all[, c("CASE_STATUS", "Quarter", "H1B_DEPENDENT")]
denied_all <- denied_all[, c("CASE_STATUS", "Quarter", "H1B_DEPENDENT")]


denied_all_1 <- denied_all %>% group_by(Quarter, H1B_DEPENDENT) %>% summarise(freq = n())
certified_all_1 <- certified_all %>% group_by(Quarter, H1B_DEPENDENT) %>% summarise(freq = n())
colnames(certified_all_1)[3] <- "accepted_freq" 
all <- left_join(certified_all_1, denied_all_1)

all$per <- (all$freq/(all$accepted_freq + all$freq))*100 
all$H1B_DEPENDENT <- ifelse(all$H1B_DEPENDENT == 1, "Y", "N" )
all$H1B_DEPENDENT[is.na(all$H1B_DEPENDENT)] <- "N"


#Ploting 5th plot 
ggplot(data = all, aes(H1B_DEPENDENT, per, group = Quarter, fill = Quarter)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Rejection Percentage For Dependent Vs Non Dependent Applicants") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5), text = element_text(size = 10, face = "bold" ) ) +
  ylab("Percentage of Rejection")  +
  scale_fill_manual(values=wes_palette(n=4, name="Rushmore1")) 









