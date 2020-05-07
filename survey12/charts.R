library(tidyverse)
library(ggalt)
library(theme538)
library(hash)
library(scales)
extrafont::loadfonts()

# Generates two types of charts:
#   response charts show individual expert responses and the consensus forecast
#   distribution charts are for questions where the experts assign probabilities to different categories

distribution_qids <- c("QF1", "QF2", "QF3", "QF4")
response_qids <- c("QF5")

# really rough x-axis labels
config <- hash()
title1 <- "How many confirmed cases May 10?"
title2 <- "How many states will report new infections?"
title3 <- "How many new positive cases in Texas?"
title4 <- "If Texas had not relaxed, how many new cases?"
config[["QF5_title"]] <- "How many deaths in 2020?"

# Cleaned responses, produced via prep.R
cleaned_responses <- read_delim(file = "cleanedResponses.csv", delim = ',')
cleaned_responses$consensus <- 0

# Consensus forecasts, added to the bottom of each _responses chart
consensus_forecasts <- read_delim(file = "categoricalConsensus.csv", delim = ',')
consensus_forecasts$questionOpt <- substr(consensus_forecasts$questionLabel, 5, 6)

# Distribution questions
cat_probabilities <- read_delim(file = "categoricalConsensus.csv", delim = ',')
cat_probabilities$cat <- as.double(substr(str_replace(cat_probabilities$questionLabel, "F", ""), 4, 5))

# response charts
for (qid in response_qids) {
  print(qid)

  q_responses <- cleaned_responses %>%
    filter(question == qid)

  q_responses <- q_responses %>%
    add_row(
      question=qid,
      expertID=9999,
      min= consensus_forecasts %>% filter(Q==qid & questionOpt=="1") %>% summarise(prob) %>% last(),
      mode=consensus_forecasts %>% filter(Q==qid & questionOpt=="2") %>% summarise(prob) %>% last(),
      max= consensus_forecasts %>% filter(Q==qid & questionOpt=="3") %>% summarise(prob) %>% last(),
      consensus=1
    )

  q_responses <- q_responses %>%
    arrange(1-consensus, mode, max, min)
  q_responses$i <- 1
  q_responses$expert <- factor(q_responses$expertID, levels=q_responses$expertID)
  q_responses$recttop=cumsum(q_responses$i) #*nrow(q_responses)*(1/nrow(q_responses))+1

  plot <- q_responses %>%
    ggplot() +
      geom_point(aes(x=mode, y=expert), colour="#1790d2", size=3) +
      geom_rect(aes(xmin=min, xmax=max, ymin=recttop+(10/nrow(q_responses)/2), ymax=recttop-(10/nrow(q_responses)/2)), alpha = .4, fill = "#1790d2") +
      scale_x_continuous(labels = comma) +
      labs(x=config[[paste(qid,"title",sep="_")]],
           y="Expert",
           title="",
           subtitle="") +
      theme_538 +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())

  ggsave(paste(qid, "_responses.png", sep=''))
}

# distribution charts

q_probs_1 <- cat_probabilities %>%
  filter(Q == "QF1")

q_probs_1 %>%
  ggplot(aes(x=cat, y=prob)) +
  geom_bar(stat="identity", fill="#fccd25") +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  ggtitle("") +
  xlab(title1) +
  ylab("Probability") +
  theme_538
ggsave("QF1_distribution.png")

q_probs_2 <- cat_probabilities %>%
  filter(Q == "QF2")

q_probs_2 %>%
  ggplot(aes(x=cat, y=prob)) +
    geom_bar(stat="identity", fill="#fccd25") +
    scale_x_discrete() +
    scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
    ggtitle("") +
    xlab(title2) +
    ylab("Probability") +
    theme_538
ggsave("QF2_distribution.png")

q_probs_3 <- cat_probabilities %>%
  filter(Q == "QF3")

q_probs_3 %>%
  ggplot(aes(x=cat, y=prob)) +
  geom_bar(stat="identity", fill="#fccd25") +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  ggtitle("") +
  xlab(title3) +
  ylab("Probability") +
  theme_538
ggsave("QF3_distribution.png")

q_probs_4 <- cat_probabilities %>%
  filter(Q == "QF4")

q_probs_4 %>%
  ggplot(aes(x=cat, y=prob)) +
  geom_bar(stat="identity", fill="#fccd25") +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  ggtitle("") +
  xlab(title4) +
  ylab("Probability") +
  theme_538
ggsave("QF4_distribution.png")
