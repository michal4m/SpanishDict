
library(tidyverse)
library(rvest)

#### List of most important spanish verbs ####

wl <- list()

for (i in 1:4){

url_2 <- "https://www.linguasorb.com/spanish/verbs/most-common-verbs"

first_page <- read_html(paste0(url_2,"/",i))
                        
wl[[i]] <-  first_page %>% 
                # The '.' indicates the class
                html_nodes('span.vIrreg') %>% 
                # Extract the raw text as a list
                html_text() 

}

wl_full <-tibble(most_popular_verbs = unlist(wl)) 

wl_full %>% 
  group_by(most_popular_verbs) %>% 
  summarise(n = n()) %>%
  arrange(desc(n)) %>% view

wl_clean <- wl_full %>% 
              filter(!(most_popular_verbs %in% c("Irregular verbs","red")) )

wl_clean %>% 
  group_by(most_popular_verbs) %>% 
  summarise(n = n()) %>%
  arrange(desc(n)) %>% view

#### List of most important spanish verbs ####

#### How many ir,er,ar verbs are there? ####

wl_endings <- wl_clean %>% 
                mutate(ending = substr(most_popular_verbs,
                                       nchar(most_popular_verbs)-1,
                                       nchar(most_popular_verbs))) %>%
                group_by(ending) %>% 
                count() 

wl_clean %>%
  mutate(ending = substr(most_popular_verbs,
                         nchar(most_popular_verbs)-1,
                         nchar(most_popular_verbs))) %>%
  filter(ending == 'ír') %>% view

#### How many ir,er,ar verbs are there? ####

# Link to SpanishDict conjugation section
chosen_word <- "hacer"
url_1 <- paste0("https://www.spanishdict.com/conjugate/",chosen_word)

first_page <- read_html(url_1)

pronouns <- first_page %>% 
              # The '.' indicates the class
              html_nodes('td.vtable-pronoun') %>% 
              # Extract the raw text as a list
              html_text() %>%
              as_tibble() %>% 
              rename(pronoun = value) %>% 
              distinct() %>% 
              filter(!(pronoun %in% c("Ud.","Uds.")))

tenses <- first_page %>% 
            # The '.' indicates the class
            html_nodes('td.vtable-title') %>% 
            # Extract the raw text as a list
            html_text() %>% 
            as_tibble() %>% 
            rename(tense = value)

moods <- c(rep("Indicative",5),
           rep("Subjunctive",4),
           rep("Imperative",2),
           rep("Progressive",5),
           rep("Perfect",5),
           rep("Perfect Subjunctive",3)) %>% 
          as_tibble() %>% 
          rename(mood = value)           

table <- bind_cols(tenses,moods)

# moods <- first_page %>% 
#   # The '.' indicates the class
#   html_nodes('span.vtable-label-link-text') %>% 
#   # Extract the raw text as a list
#   html_text() %>%
#   as_tibble() %>% 
#   rename(mood = value)

table <- crossing(pronouns,table)  

#table <- crossing(table,moods)            

table <- table %>% 
          arrange(match(tense,tenses$tense)) %>% 
          arrange(match(pronoun,pronouns$pronoun)) %>%
          arrange(match(mood,moods$mood)) 

verb_forms <- first_page %>% 
                # The '.' indicates the class
                html_nodes('td.vtable-word') %>% 
                # Extract the raw text as a list
                html_text() %>%
                as_tibble() %>% 
                rename(verb_form = value)

table <- bind_cols(table,verb_forms)
  
# to do: WRITE CONJUNGATION FUNCTION (according to basic grammar rules) 
# inputs: verb_indefinido, pronoun, tense, mood
# output: conjugated verb according to grammar rules 

table %>% 
  mutate(indefinido_form = chosen_word) %>%
  mutate(root = substr(indefinido_form,1,nchar(indefinido_form)-2)) %>%
  mutate(ending = substr(indefinido_form,nchar(indefinido_form)-1,nchar(indefinido_form))) %>%
  mutate(verb_form_rules = case_when(
                              mood == "Indicative" 
                                     
                                     
                                     )
         )
  view

table %>% 
  # mutate(mood_tense = paste(mood,"_",tense)) %>% 
  # spread(mood_tense,verb_form,drop = c("tense","mood")) %>% view
  pivot_wider(names_from = c(mood,tense),
              values_from = verb_form) %>% view
  t() %>% view

# Conjugation basics
# 1) Presente Indicativo: 
#   a) Drop last 2 letters and add
#      aa) ar:   o; as; a; amos; áis; an
#      ab) er:   o; es; e; emos; éis; en
#      ac) ir:   o; es; e; imos; ís; en
# 2) Preterito Indicativo: 
#   a) Drop last 2 letters and add
#      aa) ar:   é; aste; ó; amos; asteis; aron
#      ab) er:   í; iste; ió; imos; isteis; ieron
#      ac) ir:   í; iste; ió; imos; isteis; ieron
  
  
  
# and so on
# Create conjugation according to the rules and point out differences ex. list letters which differ

# List of most important spanish tenses

# To do: add styling for irregularities
# To do: WRAP UP EVERYTHING IN DATATABLE
  
# Accents frequency stats
    