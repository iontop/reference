rm(list=ls())
gc()

library(rvest)
library(tidyverse)


eng_news <- function(term) {
  
  html_dat <- read_html(paste0("https://news.google.com/search?q=",term,"&hl=en-US&gl=US&ceid=US%3Aen"))
  
  dat <- data.frame(Link = html_dat %>%
                      html_nodes('.VDXfz') %>% 
                      html_attr('href')) %>% 
    mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))
  
  news_dat <- data.frame(
    Title = html_dat %>%
      html_nodes('.DY5T1d') %>% 
      html_text(),
    Link = dat$Link
  )
  
  return(news_dat)
}

kor_news <- function(term) {
  html_dat <- read_html(paste0("https://news.google.com/search?q=",term,"&hl=ko&gl=KR&ceid=KR%3Ako"))
  
  dat <- data.frame(Link = html_dat %>%
                      html_nodes('.VDXfz') %>% 
                      html_attr('href')) %>% 
    mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))
  
  news_dat <- data.frame(
    Title = html_dat %>%
      html_nodes('.DY5T1d') %>% 
      html_text(),
    Link = dat$Link
  )
  
  return(news_dat)
}


df1 <- eng_news("macbook")
df2 <- kor_news(URLencode('맥북'))

########### word cloud #############
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_321')
library(rJava)
library(KoNLP)
rJava::.jinit() # 0이 출력되면 정상

# KoNLP installation
# install.packages("KoNLP",
#                  repos = c("https://forkonlp.r-universe.dev",
#                            "https://cloud.r-project.org"),
#                  INSTALL_opts = c("--no-multiarch")
# )


library(wordcloud2)
library(stringr)

title_text <- df2$Title

nouns_vec <- 
  title_text %>% 
  str_replace_all("[[:punct:]]", "") %>% 
  str_replace_all("ㅣ", " ") %>% 
  extractNoun() %>% # 명사만 추출하고,
  unlist() # list를 vector로 변환해서

nouns_df <- 
  nouns_vec %>% 
  table() %>% 
  as.data.frame(stringsAsFactors = FALSE)

names(nouns_df) <- c("words","freq")

tibble(nouns_df)

nouns_df <- 
  nouns_df %>% 
  mutate_if(is.factor, is.character) %>% 
  filter(nchar(words) >= 2) %>% 
  arrange(desc(freq))

nouns_df %>% wordcloud2()
