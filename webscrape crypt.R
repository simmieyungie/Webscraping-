library(rvest)
library(tidyverse)
library(stringr)
#Reading table from wikipage
page <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_life_expectancy")


## Obtain the peice of the web page that corresponds to the "wikitable" node
my.table <- html_node(page, ".wikitable")


# Convert the html table element into a data frame
my.table <- html_table(my.table, fill = TRUE)

# Extracting the columns from the table and turning into a named vector
x <- my.table[,4]
names(x) <- my.table[,2]


#for
x <- my.table %>% 
  select(2,4) %>% 
  filter(!my.table$`State/federal district/territory` %in% c("Northern Mariana Islands", "Guam", "American Samoa", "Puerto Rico", "U.S. Virgin Islands"))



#try for crycurrency page
#get url contents
crpyto_url <- read_html("https://www.tradingview.com/markets/cryptocurrencies/prices-all/")

#get table html
cryp_table <- html_nodes(crpyto_url, css = "table")

#get table 
crypt_table <- html_table(cryp_table, fill = T) %>% 
  as.data.frame()

#Clean dataframe and remove unwanted values/symbols
crypt_table <- apply(crypt_table[,2:9], 2, gsub, pattern = "[B M % K]", replacement = "") %>% 
  cbind.data.frame(crypt_table$Var.1) %>% 
  select(9, 1:7)

#create vector of names
x <- c("Currency","Mkt Cap", "Fd Mkt Cap", "Last", "Avail coins", "Total coins", "Traded Vol", "Chng%")

names(crypt_table) <- x


crypt_table1 <- data.frame(apply(crypt_table[2:8],2, as.numeric)) %>% 
  cbind(currency = crypt_table$Currency) %>% 
  select(8,1:7)

#Perform some eda on the data
#top 10 traded vol
crypt_table1 %>% 
  select(currency, Traded.Vol) %>% 
  filter(Traded.Vol > 600) %>% 
  ggplot(., aes(reorder(currency, Traded.Vol), Traded.Vol )) +
  geom_bar(stat = "identity", aes(fill = Traded.Vol)) + coord_flip() +
  ggtitle("Top 10 Traded Volume") +
  xlab("Currency") + ylab("Traded Volume") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title.x = element_text(face = "bold", size =  13),
        axis.title.y = element_text(face = "bold", size = 13))


#Examine currency with positive change
crypt_table1 %>% 
  select(currency, Chng.) %>% 
  filter(Chng. > 2.00) %>% 
  top_n(10) %>% 
  ggplot(., aes(reorder(currency, Chng.), Chng.)) +
  geom_bar(stat = "identity", aes(fill = Chng.)) + coord_flip() +
  ggtitle("Positive changes Top 10") +
  xlab("Currency") + ylab("change (%)")+ 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title.x = element_text(face = "bold", size =  13),
        axis.title.y = element_text(face = "bold", size = 13))



