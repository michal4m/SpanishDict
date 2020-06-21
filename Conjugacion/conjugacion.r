
library(tidyverse)
library(rvest)

#### List of most important spanish verbs ####

wl <- list()

for (i in 1:4){

url_2 <- "https://www.linguasorb.com/spanish/verbs/most-common-verbs"

first_page <- read_html(paste0(url_2,"/",i))
                        
wl[[i]] <-  first_page %>% 
                # The '.' indicates the class
                html_nodes('span') %>% 
                # Extract the raw text as a list
                html_text() 

}
most_popular_verbs = unlist(wl)

is_good <- function(x){
  ifelse(x %in% c("Toggle navigation","","Spanish","Irregular verbs","\n\n","\n\n ","red"),
         FALSE,TRUE)
}

most_popular_verbs <- most_popular_verbs[is_good(most_popular_verbs)]
wl_full <- tibble(most_popular_verbs) 

#### List of most important spanish verbs ####

#### How many ir,er,ar verbs are there? ####

wl_endings <- wl_full %>% 
                mutate(ending = substr(most_popular_verbs,
                                       nchar(most_popular_verbs)-1,
                                       nchar(most_popular_verbs))) %>%
                group_by(ending) %>% 
                count() 

# wl_full %>%
#   mutate(ending = substr(most_popular_verbs,
#                          nchar(most_popular_verbs)-1,
#                          nchar(most_popular_verbs))) %>%
#   filter(ending == 'ír') %>% view

#### How many ir,er,ar verbs are there? ####

# Link to SpanishDict conjugation section
chosen_word <- "hacer"
url_1 <- paste0("https://www.spanishdict.com/conjugate/",chosen_word)
first_page <- read_html(url_1)

#### Preapring conjugation table ####

pronouns <- first_page %>% 
              # The '.' indicates the class
              # html_nodes('td.vtable-pronoun') %>% # old class name
              html_nodes('td._2UqTZCc2') %>% # new class name
              # Extract the raw text as a list
              html_text() %>%
              as_tibble() %>% 
              rename(pronoun = value) %>% 
              distinct() %>% 
              filter(!(pronoun %in% c("Ud.","Uds.")))

tenses <- first_page %>% 
            # The '.' indicates the class
            #html_nodes('td.vtable-title') %>% # old class name
            html_nodes('td._32tEU4Z5') %>%  # new class name
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

# Binding tenses and moods
table <- bind_cols(tenses,moods)
# Creating conjugation table
table <- crossing(pronouns,table)  

# Sorting conjugation table
table <- table %>% 
  arrange(match(pronoun,pronouns$pronoun)) %>%
  arrange(match(tense,tenses$tense)) %>% 
  arrange(match(mood,moods$mood)) 

#### Preapring conjugation table ####

#### CONJUGACION RULES ####

conj_ar <- c("o","as","a","amos","áis","an",         # presente indicativo
        "é","aste","ó","amos","asteis","aron",       # preterite perfecto simple
        "aba","abas","aba","ábamos","abais","aban",  # pretérito imperfecto
        "ía","ías","ía","íamos","íais","ían" ,       # condicional (keeps the ending)
        "é","ás","á","emos","éis","án",              # el futuro simple (keeps the ending)
        "e","es","e","emos","éis","en",              # el presente de subjuntivo (keeping yo present form wihtout -o)
        "ra","ras","ra","ramos","rais","ran",        # el imperfecto del subjuntivo (ustedes form minus ron) -version 1!, (in nosotros form add accent before ending!)  
        "re","res","re","remos","reis","ren",        # el futuro del subjuntivo (ustedes form minus ron) (rarely used - instead presente subjuntivo) (in nosotros form add accent before ending!)
        "se","ses","se","semos","seis","sen",        # el imperfecto del subjuntivo (ustedes form minus ron) -version 2!, (in nosotros form add accent before ending!) 
        rep("",6*2),                                 # imperative
        rep("ando",6*5),                             # progressive: to be (indicativo) + ando,iendo gerundio (stem without ending)
        rep("ado",6*5),                              # present perfect: haber (indicativo) + ado, ido (stem without ending)
        rep("ado",6*3)                               # present perfect subjunctive: haber (subjunctive) + ado, ido (stem without ending)
        )

conj_er <- c("o","es","e","emos","éis","en",         # presente indicativo
        "í","iste","ió","imos","isteis","ieron",     # preterite perfecto simple
        "ía","ías","ía","íamos","íais","ían",        # pretérito imperfecto
        "ía","ías","ía","íamos","íais","ían" ,       # condicional (keeps the ending)
        "é","ás","á","emos","éis","án",              # el futuro simple (keeps the ending)
        "a","as","a","amos","áis","an",              # el presente del subjuntivo (keeping yo present form wihtout -o)
        "ra","ras","ra","ramos","rais","ran",        # el imperfecto del subjuntivo (ustedes form minus ron) -version 1!, (in nosotros form add accent before ending!)  
        "re","res","re","remos","reis","ren",        # el futuro del subjuntivo (ustedes form minus ron) (rarely used - instead presente subjuntivo) (in nosotros form add accent before ending!)
        "se","ses","se","semos","seis","sen",        # el imperfecto del subjuntivo (ustedes form minus ron) -version 2!, (in nosotros form add accent before ending!) 
        rep("",6*2),                                 # imperative
        rep("iendo",6*5),                            # progressive: to be (indicativo) + gerundio (stem without ending)
        rep("ido",6*5),                              # present perfect: haber (indicativo) + ado, ido (stem without ending)
        rep("ido",6*3)                               # present perfect: haber (subjunctive) + ado, ido (stem without ending)
)

conj_ir <- c("o","es","e","imos","is","en",          # presente indicativo
        "í","iste","ió","imos","isteis","ieron",     # preterite perfecto simple
        "ía","ías","ía","íamos","íais","ían",        # pretérito imperfecto
        "ía","ías","ía","íamos","íais","ían" ,       # condicional (keeps the ending)
        "é","ás","á","emos","éis","án",              # el futuro simple (keeps the ending)
        "a","as","a","amos","áis","an",              # el presente de subjuntivo (keeping yo present form wihtout -o)
        "ra","ras","ra","ramos","rais","ran",        # el imperfecto del subjuntivo (ustedes form minus ron) -version 1!, (in nosotros form add accent before ending!)  
        "re","res","re","remos","reis","ren",        # el futuro del subjuntivo (ustedes form minus ron) (rarely used - instead presente subjuntivo) (in nosotros form add accent before ending!)
        "se","ses","se","semos","seis","sen",        # el imperfecto del subjuntivo (ustedes form minus ron) -version 2!, (in nosotros form add accent before ending!) 
        rep("",12),                                  # imperative          
        rep("iendo",6*5),                            # progressive: to be (indicativo) + gerundio (stem without ending)
        rep("ido",6*5),                              # present perfect: haber (indicativo) + ado, ido (stem without ending)
        rep("ido",6*3)                               # present perfect: haber (subjunctive) + ado, ido (stem without ending)
)

#### CONJUGACION RULES ####

table_conj <- cbind(conj_ar,conj_er,conj_ir)      
table <- cbind(table,table_conj)

# saveRDS(table, file = "table.rds")

#### Scraping conjugation ####

conj_list <- vector("list",100)

most_popular_verbs_2 <- gsub("oír","oir",gsub("í","i",most_popular_verbs))
for(i in seq_along(most_popular_verbs_2)) {

url_3 <- paste0("https://www.spanishdict.com/conjugate/",most_popular_verbs_2[i])
first_page_3 <- read_html(url_3)

conj_list[[i]] <- first_page_3 %>% 
                # The '.' indicates the class
                # html_nodes('td.vtable-word') %>% # old class name
                html_nodes('td._20Fm6xF3') %>% # new class name
                # Extract the raw text as a list
                html_text() %>%
                as_tibble() %>% 
                rename(verb_form = value)

}

verb_forms <- do.call(cbind,conj_list)
colnames(verb_forms) <- most_popular_verbs
verb_forms <- as_tibble(verb_forms) 

#saveRDS(verb_forms_n, file = "verb_forms.rds")

#### Scraping conjugation ####

# END of data preparation (scraping)

# Custom functions

table_new <- bind_cols(table,verb_forms)

table_new %>% 
  pivot_longer(cols = 7:ncol(table_new),
               names_to = "indefinido",
               values_to = "verb_form") -> atest

atest
  
# to do: WRITE CONJUNGATION FUNCTION (according to basic grammar rules) 
# inputs: verb_indefinido, pronoun, tense, mood
# output: conjugated verb according to grammar rules 

conj_func <- function(indefinido,pronoun,tense,mood){
  
  # input rules
  if(!(pronoun %in% c("yo","tu","tú","el","él","ella",
                    "usted","Usted","Ud.","Ud","ud","ud.",
                    "nosotros","vosotros","ellos","ellas",
                    "ustedes","Ustedes","Uds.","Uds","uds","uds."
  ))) { stop("pronoun must be yo,tu, ...") }
  
  if(!(tense %in% c("Present","present","preterite","Preterite","Imperfect","imperfect",
                      "Conditional","conditional","future","Future","Imperfect 2","imperfect 2",
                      "Affirmative","affirmative","Negative","negative","Past","past"
  ))) { stop("tense must be present,preterite, ,...") }
  
  if(!(mood %in% c("Indicative","indicative","Subjunctive","subjunctive",
                   "Imperative","imperative","Progressive","progressive",
                   "Perfect","perfect","Perfect Subjunctive","perfect subjunctive"
  ))) { stop("tense must be present,preterite, ,...") }
  
  # describing row in a table
  row <- (table$pronoun == "yo")*(table$tense == "Present")*(table$mood == "Indicative")
  # describing column (based on ending)
  col <- 
  
  
  table[,] %>% view
  
  
  
  root = substr(indefinido,1,nchar(indefinido)-2)  
  ending = substr(indefinido,nchar(indefinido)-1,nchar(indefinido))
  
  result = case_when()
  
  ending
}

conj_func(indefinido = "estar",pronoun = "as")

# table %>% 
#   mutate(indefinido_form = chosen_word) %>%
#   mutate(root = substr(indefinido_form,1,nchar(indefinido_form)-2)) %>%
#   mutate(ending = substr(indefinido_form,nchar(indefinido_form)-1,nchar(indefinido_form))) %>%
#   view
# 
# table %>% 
#   # mutate(mood_tense = paste(mood,"_",tense)) %>% 
#   # spread(mood_tense,verb_form,drop = c("tense","mood")) %>% view
#   pivot_wider(names_from = c(mood,tense),
#               values_from = verb_form) %>% view
#   t() %>% view

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
    