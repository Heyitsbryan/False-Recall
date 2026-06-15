library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "false_recall"

processed_data <- read_csv(here(processed_data_directory,paste0(file_name,"-processed-data.csv"))) 

#focus just on recognition trials
recognition_data <- processed_data %>%
  filter(task=="recognition") %>%
  filter(condition!="attention_check") %>%
  mutate(
    familiarity = case_when(
      condition %in% c("unrelated","weakly related","critical") ~ "new",
      TRUE ~ "old"
    )
  ) 

#average subject ratings
avg_subj_ratings <- recognition_data %>%
  group_by(participant_id,condition,familiarity) %>%
  summarize(
    N=n(),
    avg_rating = mean(likert_rating)
  ) 

avg_subj_ratings$condition <- fct_relevel(avg_subj_ratings$condition, c("studied","critical","weakly related","unrelated"))

avg_subj_ratings <- avg_subj_ratings %>%
  arrange(participant_id,condition,familiarity) 

overall_ratings <- avg_subj_ratings %>%
  group_by(condition,familiarity) %>%
  summarize(
    N=n(),
    mean_rating=mean(avg_rating),
    sd = sd(avg_rating),
    sem = sd / sqrt(N)
  ) %>%
  arrange(condition,familiarity)

ggplot(avg_subj_ratings,aes(condition,avg_rating,color=familiarity))+
  geom_violin()

ggplot(overall_ratings,aes(condition,mean_rating,color=familiarity,fill=familiarity))+
  geom_bar(stat="identity",width=0.5)+
  geom_errorbar(aes(ymin=mean_rating-sem,ymax=mean_rating+sem),width=0.1,color="black")


ggplot(avg_subj_ratings,aes(condition,avg_rating,color=familiarity))+
  geom_violin(fill=NA)+
  geom_line(aes(group=participant_id),color="black",alpha=0.1,position=position_jitter(width=0.1,height=0,seed=123))+
  geom_jitter(position=position_jitter(width=0.1,height=0,seed=123),alpha=0.3)+
  geom_line(data=overall_ratings,aes(y=mean_rating,group=1),linewidth=1.5,color="black")+
  geom_point(data=overall_ratings,aes(y=mean_rating),size=4)+
  geom_errorbar(data=overall_ratings,aes(y=mean_rating,ymin=mean_rating-sem,ymax=mean_rating+sem),width=0)+
  xlab("Word Type")+
  ylab("Average Rating\n(0 = Definitely New)            (3 = Definitely Old)")+
  theme_minimal(base_size=16)
  


