library(tidyverse)
library(janitor)

scout_url <- "https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv"
womens_games_url <- "https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv"

scout_raw <- read_csv(scout_url, col_types = cols(game_date = col_date(format = "%Y-%m-%d")), guess_max = 75873 ) %>% 
  clean_names() %>% 
  mutate(id = "scount")

womens_games_raw <- read_csv(womens_games_url, guess_max = 99875) %>% 
  clean_names() %>% 
  mutate(id = "womens_games")

full_df <- bind_rows(scout_raw, womens_games_raw)

# full_df %>% 
#   count(event)

xg_df <- full_df %>% 
  filter(event == "Goal" | event == "Shot" ) #%>% 
#  mutate(is_pp = )

fvf_xg_df <- filter(xg_df, home_team_skaters == 5 & away_team_skaters == 5 )


state_df <- tibble(
  shooter_diff = -3:3#, shooter_state = c()
)

all_xg_df <- mutate(xg_df,
                    shooter_team = ifelse(team == home_team, "home", "away" ),
                    shooter_diff =  ifelse(shooter_team == "home", 
                                           home_team_skaters - away_team_skaters, 
                                           away_team_skaters-home_team_skaters )
                    #, state = ifelse(is_home_team & home_adv )
                    )

library(tidymodels)

set.seed(4)
i_split <- initial_split(fvf_xg_df, prop = .8,  strata = "event" )

train_set_fvf <- training(i_split)
test_set_fvf <- testing(i_split)


# ggplot(train_set_fvf, aes(x_coordinate, y_coordinate)) + 
#   geom_point( alpha = .2)+
#   facet_grid(.~event)


# 5 on 5 xg model ---------------------------------------------------------

model_df_5v5 <- train_set_fvf %>%
  select(event, x_coordinate, y_coordinate, detail_1, detail_3, detail_4)

library(caret)

tr_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = TRUE)

glm_fvf <- train(event~., method = "glm", family = binomial, trControl = tr_ctrl, data = model_df_5v5)
xgb1_fvf <- train(event~., method = "xgbLinear", trControl = tr_ctrl, data = model_df_5v5)
xgb2_fvf <- train(event~., method = "xgbTree", trControl = tr_ctrl, data = model_df_5v5)
xgb3_fvf <- train(event~., method = "xgbDART", trControl = tr_ctrl, data = model_df_5v5)


glm_fvf_preds <- predict(glm_fvf, model_df_5v5, type = "prob")[,"Goal"]
xgb1_fvf_preds <- predict(xgb1_fvf, model_df_5v5, type = "prob")[,"Goal"]
xgb2_fvf_preds <- predict(xgb2_fvf, model_df_5v5, type = "prob")[,"Goal"]
xgb3_fvf_preds <- predict(xgb3_fvf, model_df_5v5, type = "prob")[,"Goal"]


model_df_5v5 %>% 
  count(event)

sum(glm_fvf_preds)
sum(xgb1_fvf_preds)
sum(xgb2_fvf_preds)
sum(xgb3_fvf_preds)


glm_fvf_preds_test <- predict(glm_fvf, test_set_fvf, type = "prob")[,"Goal"]
xgb1_fvf_preds_test <- predict(xgb1_fvf, test_set_fvf, type = "prob")[,"Goal"]
xgb2_fvf_preds_test <- predict(xgb2_fvf, test_set_fvf, type = "prob")[,"Goal"]
xgb3_fvf_preds_test <- predict(xgb3_fvf, test_set_fvf, type = "prob")[,"Goal"]

test_set_fvf %>% 
  count(event)
 
sum(glm_fvf_preds_test)
sum(xgb1_fvf_preds_test)
sum(xgb2_fvf_preds_test)
sum(xgb3_fvf_preds_test)

# eng vars:
# Time since last shot (Should cover rebounds)
# Was passed to/ time since pass?
# Behind the net?
# Shot angle/ shot distance?




# All states model --------------------------------------------------------


