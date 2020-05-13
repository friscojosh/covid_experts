# Convert expertAnswersDB.csv into easier-to-work-with cleanedResponses.csv
# Also remove any invalid responses from the dataset

# Q1: How many total confirmed cases of COVID-19 in the U.S. will The COVID Tracking Project report as of Sunday 10 May 2020?
# Less than or equal to 1,200,000
# Between 1,200,000 and 1,250,000, inclusive
# More than 1,250,000 but less than 1,300,000
# Between 1,300,000 and 1,350,000, inclusive
# More than 1,350,000 but less than 1,400,000
# Between 1,400,000 and 1,450,000, inclusive
# More than 1,450,000

# Q2: How many US states, territories and jurisdictions  (50 states plus District of Columbia, Guam, the Northern Mariana Islands, Puerto Rico, and the U.S Virgin Islands.) will report more new COVID-19 cases for September 2020 than for June 2020?

## Q5: As of Sunday, May 3, 2020, 61,868 deaths from COVID-19 have been reported in the US in 2020. How many deaths due to COVID-19 will occur in the US in 2020? Please report a 10th, 50th and 90th percentile, in other words a 80% confidence interval and a median.

library(tidyverse)
# Question ids that contain response triplets (e.g. 5th, 50th, 95th percentile)
qids <- c("QF2", "QF3", "QF4")

responses <- read_delim(file = "expertAnswersDB.csv", delim = ',')
responses$question <- substr(responses$questionLabel, 1, 3)
responses$questionOpt <- substr(responses$questionLabel, 5, 6)

cleaned_responses <- responses %>%
  filter(question %in% qids) %>%
  spread(questionType, answer) %>%
  group_by(question, expertID) %>%
  summarise(min = max(as.double(percentile_10_50_90__low), na.rm = TRUE), mode = max(as.double(percentile_10_50_90__mode), na.rm = TRUE), max = max(as.double(percentile_10_50_90__high), na.rm = TRUE)) %>%
  ungroup() %>%
  filter(min != -Inf & mode != -Inf & max != -Inf)

write.csv(cleaned_responses, "cleanedResponses.csv", row.names=FALSE)
