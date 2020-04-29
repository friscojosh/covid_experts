# Convert expertAnswersDB.csv into easier-to-work-with cleanedResponses.csv
# Also remove any invalid responses from the dataset

library(tidyverse)

# Question ids that contain response triplets (e.g. 5th, 50th, 95th percentile)
qids <- c("QF2", "QF4", "QF5")

responses <- read_delim(file = "expertAnswersDB.csv", delim = ',')
responses$question <- substr(responses$questionLabel, 1, 3)
responses$questionOpt <- substr(responses$questionLabel, 5, 6)
responses$questionType <- ifelse(responses$questionType=="probCat" & responses$questionOpt=="1", "triplet_low", responses$questionType)
responses$questionType <- ifelse(responses$questionType=="probCat" & responses$questionOpt=="2", "triplet_mode", responses$questionType)
responses$questionType <- ifelse(responses$questionType=="probCat" & responses$questionOpt=="3", "triplet_high", responses$questionType)

cleaned_responses <- responses %>%
  filter(question %in% qids) %>%
  spread(questionType, answer) %>%
  group_by(question, expertID) %>%
  summarise(min = max(as.double(triplet_low), na.rm = TRUE), mode = max(as.double(triplet_mode), na.rm = TRUE), max = max(as.double(triplet_high), na.rm = TRUE)) %>%
  ungroup() %>%
  filter(min != -Inf & mode != -Inf & max != -Inf)

write.csv(cleaned_responses, "cleanedResponses.csv", row.names=FALSE)
