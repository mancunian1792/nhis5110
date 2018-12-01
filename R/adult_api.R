# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

load_data <- function(){
  load("data/sampleAdult.rda")
}
get_meditation_response <- function(){
  load_data()
  adult_set <- dplyr::select(sampleAdult, MBO_MAN1:MBO_PRO1)
  return(adult_set)
}

plot_meditation_response <- function(){
  #require(ggplot2)
  med_data <- get_meditation_cols()
  gathered <- tidyr::gather(med_data, meditation, response, colnames(med_data) )
  gathered <- dplyr::filter(gathered, !is.na(meditation), !is.na(response))
  gathered <- dplyr::mutate(gathered, meditation = factor(meditation,
                                              levels = c("MBO_MND1","MBO_SPR1", "MBO_IMG1","MBO_MAN1", "MBO_PRO1"),
                                              labels = c("Mindful", "Spiritual", "Imagery", "Mantra", "Progressive Relaxation")),
                                      response = factor(response, levels = c(1, 2), labels = c("Yes", "No"))) %>%
    filter(response == "Yes")
  return(ggplot2::ggplot(data = gathered, mapping = aes(x = meditation)) + geom_bar())

}

get_feelings_response <- function(){
  load_data()
  cols <- c("ASISAD", "ASINERV", "ASIHOPLS", "ASIRSTLS", "ASIEFFRT", "ASIWTHLS")
  feelings_data <- dplyr::select(sampleAdult , cols)
  feelings_data <- dplyr::mutate_all(feelings_data, funs(factor(., labels = c(`1` = "All the Time",
                                                                              `2` = "Most times",
                                                                              `3` = "Some times",
                                                                              `4` = "Little",
                                                                              `5` = "Never",
                                                                              `7` = "Not ascertained",
                                                                              `8` = "Refused",
                                                                              `9` = "Don't Know"))))

  feelings_data <- dplyr::rename(feelings_data, `Sad` = ASISAD,
                          `Nervous` =  ASINERV,
                          `Hopeless` = ASIHOPLS,
                          `Fidgety` = ASIRSTLS,
                        `Worthless` =  ASIWTHLS ,
                         `Required Effort` =  ASIEFFRT)


  return(feelings_data)
}

get_freq_table_by_emotion <- function(emotion){
  #Takes a list of emotions as input - can be one of sad, nervous, worthless, hopeless, fidgety or required effort
feel_data <- get_feelings_response()
result <- data.frame(matrix(ncol = 0, nrow = 8))
result['Frequency'] <- levels(feel_data$Sad)
for(feeling in emotion){
  result[feeling] = count(feel_data, .dots = feeling) %>% select(n)
}
return(result)
}

plot_emotion_data <- function(){
  feel_data <- get_feelings_response()
  all_feelings <- tidyr::gather(data = feel_data,
                                emotion, frequency, colnames(feel_data))

  ggplot2::ggplot(all_feelings, aes(x = emotion, fill=frequency)) + geom_bar() + coord_flip()
}

compute_emotion_score <- function(){
  load_data()
  feel_data <- dplyr::filter_at(sampleAdult, vars(ASISAD:ASIWTHLS), all_vars((. %in% c(1, 2, 3, 4))))
   feel_data <- dplyr::mutate(feel_data, ASISAD = (5 - ASISAD) ,
           ASINERV = (5 - ASINERV) ,
           ASIHOPLS = (5 - ASIHOPLS ) ,
           ASIWTHLS = (5 - ASIWTHLS) ,
           ASIEFFRT = (5 - ASIEFFRT),
           ASIRSTLS = (5 - ASIRSTLS))
   feel_data <- dplyr::mutate(feel_data, sum_score = ASISAD + ASINERV + ASIRSTLS + ASIHOPLS + ASIEFFRT + ASIWTHLS,
           neg_emotion_score = sum_score/24
    )
   return(dplyr::select(feel_data, neg_emotion_score))
}

get_yoga_cols <- function(){
  load_data()
  yoga_data <- dplyr::select(sampleAdult, starts_with("YTQU"))
  return(yoga_data)
}


get_medYoga_metadata <- function(){
  displayName <- c("ASISAD", "ASINERV", "ASIHOPLS", "ASIWTHLS", "ASIEFFRT", "ASIRSTLS", "MBO_MAN1",
                    "MBO_MND1", "MBO_IMG1", "MBO_SPR1", "MBO_PRO1", "YTQU_YG1", "YTQU_TA1", "YTQU_QG1")
  values <- c("How often did you feel sad", "How often did you nervous", "How often did you hopeless",
              "How often did you feel worthless", "How often did you feel everything required effort",
              "How often did you feel fidgety", "Mantra Meditation", "Mindful meditation", "Guided Imagery", "Spiritual Meditation",
              "Progressive Relaxation", "Yoga", "Tai Chi", "Qi Gong")

  result <- setNames(as.list(values), displayName)
  return(result)
}

