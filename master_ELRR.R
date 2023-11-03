# Load the proper libraries
library(knitr)  # Using kable to create fantastic tables
library(janitor)  #clean names from the dataframe
library(rscopus)  # directly made a search from Scopus
library(tidyverse) # classical library, manipulating data frames, and data visualization
library(tidytext)
library(wordcloud2)
library(ggthemes)
library(bibliometrix)
#############################################################################

## Read the scopus file for ELRR
## According to the scopus webpage, that is the coverage for the journal Scopus coverage years:from 1990 to Present
## Publisher:SAGE ISSN:1035-3046E-ISSN:1838-2673

## We got the references on a csv file to read it


# We export the dataset in csv format and we read it with an old function (read.csv)

scopus_search <- read.csv("scopus.csv")


# Take a look at the dataset

class(scopus_search) # As a data frame
glimpse(scopus_search) # All variables are text variables


# Use the clean names function from janitor package (improve a  bit)

clean_scopus_search_1 <- scopus_search %>% 
  mutate(citing_id = paste0("A", 1:n())) %>% # We create a unique ID for each article of the corpus
  clean_names()

clean_scopus_search_1
clean_scopus_search_1 = clean_scopus_search_1[-c(302, 459, 471),] # Cleaning up some... rows

##########################################################################
length(unique(clean_scopus_search_1$year))  ## 34 years
length(unique(clean_scopus_search_1$citing_id))  ## 585 articles or records

##########################################################################
## Table with total citations (TC = 4058) for the period of analysis
###########################################################################
table = clean_scopus_search_1 %>% arrange(desc(year)) %>% 
  group_by(year) %>% 
  mutate (csum = cumsum(cited_by)) %>%   # Cumulative sum for each year. To learn the number of citations per year
  select(authors, year, cited_by, csum)

############################################################################
#Data visualization of citations
ggplot(table, aes(x = as.factor(year), y = cited_by)) +
  geom_bar(position = "stack", stat = "identity", fill="green") +
  xlab("Year") + ylab("Number of citations") + ggtitle("Cumulative citations per year") +
  coord_flip() +
  theme_classic()
############################################################################
# Data visualization. Annual papers in the journal 
###############################################################################
clean_scopus_search_1 %>% 
  count(year) %>% 
  ggplot(aes(year, n)) +
  geom_col(show.legend = FALSE, fill = "#ca225e") +
  labs(y = "Number of papers", x = "Year") +
  ggtitle("Annual Number of papers published in the ELRR. 1983-2023") + theme_economist()
############################################################################################
### Plot annual production over time (line graph)

tableannualproduction <- clean_scopus_search_1 %>% group_by(year) %>% 
  summarise(count = n())
tableannualproduction
plot(tableannualproduction$year, tableannualproduction$count, xlim = c(1990,2023), 
     xlab = "Year",
     ylab = "Number of papers published",  main = "Annual number of papers published in the ELRR. 1983-2023", col = "blue", type = "l")

###############################################################################################
  
### Table with affiliations

affiliations_raw <- clean_scopus_search_1 %>% 
select(citing_id, authors, affiliations, authors_with_affiliations)
knitr::kable(head(affiliations_raw, n = 15))

##

scopus_affiliations <- affiliations_raw %>% 
  separate_rows(authors, sep = "; ") %>% 
  separate_rows(contains("with"), sep = "; ") %>% 
  mutate(authors_from_affiliation = str_extract(authors_with_affiliations, 
                                                "^(.+?)\\.(?=,)"),
         authors_from_affiliation = str_remove(authors_from_affiliation, ","),
         affiliations = str_remove(authors_with_affiliations, "^(.+?)\\., "),
         country = str_remove(affiliations, ".*, ")) %>% # Country is after the last comma
  filter(authors == authors_from_affiliation) %>% 
  select(citing_id, authors, affiliations, country)

knitr::kable(head(scopus_affiliations), n= 2)



##### Some affiliations are not specific country. need to clean up

scopus_affiliations$country[scopus_affiliations$country == 'University of New South Wales'] = 'Australia'

#########################################################################################################
## Visualization- Country affiliation- Top 10 for the whole period
###########################################################################################################
scopus_affiliations %>% 
  drop_na %>%
  group_by(country) %>%
  summarize(
    count = n(),
    percent = count / nrow(.) * 100
  ) %>%
  arrange(desc(count)) %>%
  head(10) 

######################################################
### Extracting and cleaning references#################
references_extract <- clean_scopus_search_1 %>% 
  filter(! is.na(references)) %>% 
  select(citing_id, references) %>% 
  separate_rows(references, sep = "; ") %>% 
  mutate(id_ref = 1:n()) %>% 
  as_tibble

knitr::kable(head(references_extract))



