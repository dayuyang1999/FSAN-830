


library(dplyr)
rawDataDF = read.csv("Salaries.csv",stringsAsFactors = FALSE, colClasses = c("character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","integer","character","character","character"), na.strings = c("NotProvided"))
set.seed(111)

##Get a random sample of rows to ensure that you are using R and the dplyr packages to do the homework
salaryDF = rawDataDF %>% sample_frac(size = 0.75) %>% as_tibble()





############ homework get started #####################




## find largest overntimepay/totalpaybenefit
salaryDF %>%
  filter(Year == 2014) %>%
  summarise(ratio = OvertimePay/TotalPayBenefits) %>%
  arrange(desc(ratio))

# In 2014, which job title had the most employees who received at least 40% of their income from overtime pay (OvertimePay / TotalPayBenefits)?

salaryDF %>% 
  filter(OvertimePay/TotalPayBenefits >=0.4 & Year ==2014) %>%
  group_by(JobTitle) %>%
  summarise(JobTitle, n = n()) %>%
  arrange(desc(n))






### what was the average (mean) overtime pay received by people with the job title that has the highest average overtime pay (OvertimePay)

salaryDF %>% 
  group_by(JobTitle) %>% 
  summarise(Mean = mean(OvertimePay, na.rm=TRUE))%>% 
  arrange(desc(Mean))


### what job title has the highest average base pay (BasePay)?
salaryDF %>%
  group_by(JobTitle) %>% 
  summarise(JobTitle, Mean = mean(BasePay, na.rm=TRUE))%>% 
  arrange(desc(Mean))

## what was the maximum TotalPayBenefits of a "Custodian".
salaryDF %>%
  filter(Year == 2014) %>%
  filter(JobTitle == "Custodian") %>%
  summarise(TotalPayBenefits)%>%
  arrange(desc(TotalPayBenefits))

# What is the sixth most common job title in 2014?
salaryDF %>%
  filter(Year == 2014) %>%
  group_by(JobTitle)  %>%
  summarise(n = n()) %>%
  arrange(desc(n))


# Enter the last name of the employee who received the highest pay (TotalPay) in 2014

salaryDF %>%
  filter(Year == 2014) %>%
  summarise(EmployeeName,TotalPay) %>%
  arrange(desc(TotalPay))

