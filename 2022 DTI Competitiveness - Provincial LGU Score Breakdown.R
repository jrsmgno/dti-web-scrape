#2022 Web Scraping DTI Competitiveness Data for Branch Segmentation

### Load packages
pacman::p_load(tidyverse,
               dplyr,
               rvest,
               magrittr,
               stringr,
               xml2,
               urltools,
               tibble,
               eeptools,
               broman)

#"""All provinces  
#Province, year to get municipalities and cities 
#Highly urbanized cities"""

##Get all provinces

province <- read_html("https://cmci.dti.gov.ph/rankings-data.php?unit=Provinces") %>% 
  html_nodes("td , .sorting , .sorting_disabled") %>% 
  html_text() -> temp

##2022 Province URLs

provinces_2022_urls <- read.csv(file = "All Provinces URLs 2022.csv", 
                                header = TRUE,
                                stringsAsFactors = FALSE,
                                sep = ",",
                                fileEncoding = "latin1")

colnames(provinces_2022_urls) <- c("rank", "score", "province", 
                                   "region", "url_2022")

provinces_2022_urls <- provinces_2022_urls %>%
  mutate_if(is.character, str_trim)

head(provinces_2022_urls)

##Iterate scraping all province URLs  
##This will get data about all cities (except highly urbanized cities) inside all provinces.  
# initialize
n <- 1
provincial_cities <- data.frame(matrix(ncol = 10))

colnames(provincial_cities) <- c("provinces_2022_urls.province.n.", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")

# loop thru all provinces
while (n <= nrow(provinces_2022_urls)) {
  read_html(provinces_2022_urls$url_2022[n]) %>% 
    html_nodes("#data-province td, #data-province tr:nth-child(1) tr") %>% 
    html_text() -> temp1
  
  temp1 %>%
    matrix(ncol = 9, byrow = TRUE) %>%
    data.frame() -> temp_df1
  
  provincial_cities <- rbind(provincial_cities, 
                             data.frame(provinces_2022_urls$province[n], 
                                        temp_df1))
  n <- n + 1
}

provincial_cities <- provincial_cities[-1,]

colnames(provincial_cities) <- c("province", "lgu", "category", "score", 
                                 "population", "pop_weight_50", "revenue", 
                                 "rev_weight_50", "tot_pct", "prov_score")

#write.csv(provincial_cities, "2022 Provincial Cities.csv", row.names=FALSE)

#Iterate to get LGU links per province
n <- 1
lgu_links <- data.frame(matrix(ncol = 3))

colnames(lgu_links) <- c("provinces_2022_urls.province.n.", "X1", "X2")

while (n <= nrow(provinces_2022_urls)) {
  url <- read_html(provinces_2022_urls$url_2022[n])
  nodes_link <- url %>%
    html_nodes("#data-province td:nth-child(1) td, 
             #data-province.table-responsive a") %>%
    html_attr("href") %>%
    URLencode()
  
  nodes_link2 <- paste0("https://cmci.dti.gov.ph/", nodes_link)
  
  nodes_name <- url %>%
    html_nodes("#data-province td:nth-child(1)") %>%
    html_text()
  
  temp_lgu <- cbind(nodes_name, nodes_link2)
  temp_lgu %>%
    matrix(ncol = 2, byrow = FALSE) %>%
    data.frame() -> tempdf_lgu
  
  lgu_links <- rbind(lgu_links, 
                     data.frame(provinces_2022_urls$province[n], 
                                tempdf_lgu))
  n <- n + 1
}

lgu_links <- lgu_links[-1,]

colnames(lgu_links) <- c("province", "lgu", "lgu_link")

#write.csv(lgu_links, "2022 Provincial with Per LGU Links.csv", row.names=FALSE)

# Iterate to get breakdown of scores and ranks per LGU
n <- 1
lgu_scores <- data.frame(matrix(ncol = 14))

colnames(lgu_scores) <- c("lgu_links.lgu.n.", "X1", "X2", 
                          "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                          "X10", "X11", "X12", "X13")

while (n <= nrow(lgu_links)) {
  lgu_url <- read_html(lgu_links$lgu_link[n])
  
  lgu_prov <- lgu_url %>%
    html_nodes(".economy-characteristics-table tr:nth-child(3) td:nth-child(4)") %>%
    html_text()
  
  lgu_rank <- lgu_url %>%
    html_nodes(".col-md-6 tr:nth-child(1) td") %>% 
    html_text() %>%
    str_replace("(st|nd|th|rd)", "") %>%
    trimws()
  
  lgu_score <- lgu_url %>%
    html_nodes("input.knob") %>%
    html_attr("value")
  
  #comp_rankscore <- lgu_url %>% #get all components that resulted to overall lgu score
  #  html_nodes(".col-md-4 td") %>%
  #  html_text() %>%
  #  trimws()
  
  comp_ranks <- lgu_url %>% #get all components that resulted to overall lgu score
    html_nodes(".col-md-4 td:nth-child(1)") %>%
    html_text() %>%
    trimws() %>%
    str_subset("(st|nd|th|rd)") %>% # filter only those containing st, nd, th, rd
    str_replace("(st|nd|th|rd)", "")
    
  
  comp_scores <- lgu_url %>% #get all components that resulted to overall lgu score
    html_nodes(".col-xs-6 td+ td") %>%
    html_text() %>%
    trimws()
  
  #long format
  #ranks <- grep("st|nd|rd|th", lgu_rankscore, value = TRUE)
  #rank_val <- str_replace(ranks, "(?<=\\d)(st|nd|rd|th)","")
  
  #comp_ranks <- comp_rankscore %>%
  #  str_subset("(st|nd|th|rd)") %>% # filter only those containing st, nd, th, rd
  #  str_replace("(st|nd|th|rd)", "") # remove the st, nd, th, rd suffix
  #comp_scores <- str_extract(comp_rankscore, "^(?!.*\\b(?:st|nd|rd|th)\\b)\\s*\\d+\\.?\\d*$")
  #comp_scores <- comp_rankscore[!grepl("(st|nd|th|rd)", comp_rankscore)]
  
  temp1_lgu <- cbind(lgu_prov, lgu_rank, lgu_score)
  temp2_lgu <- cbind(comp_ranks, comp_scores) %>%
    as.numeric() %>%  # convert values to numeric
    matrix(ncol = 10, byrow = TRUE) # create a matrix with 5 columns
  
  consolgu_temp <- cbind(temp1_lgu, temp2_lgu)
  
  consolgu_temp %>%
    matrix(ncol = 13, byrow = TRUE) %>%
    data.frame() -> tempdf_lgu2
  
  if (nrow(tempdf_lgu2) > 0) {
    lgu_scores <- rbind(lgu_scores, 
                        data.frame(lgu_links$lgu[n], 
                                   tempdf_lgu2))
  }
  
  n <- n + 1
}

lgu_scores <- lgu_scores[-1,]

colnames(lgu_scores) <- c("lgu", "province", "lgu_rank", "lgu_score", 
                          "eco_rank", "gov_rank", "infra_rank",
                          "resiliency_rank", "innov_rank", "eco_score", 
                          "gov_score", "infra_score", "resiliency_score",
                          "innov_score")

cols_arrange <- c("province","lgu", "lgu_rank", "lgu_score", 
                  "eco_rank", "eco_score", "gov_rank", 
                  "gov_score", "infra_rank", "infra_score", 
                  "resiliency_rank", "resiliency_score",
                  "innov_rank", "innov_score")

lgu_scores <- lgu_scores[, cols_arrange]


test_bind <- full_join(lgu_scores, provincial_cities, 
                       by = c("lgu","province")) %>%
  select(-score) %>%
  mutate(population = decomma(population)) %>%
  mutate_if(grepl("score|rank|population", colnames(.)), 
            ~as.numeric(as.character(.)))

#write.csv(lgu_scores, "2022 Provincial Cities LGU Score Breakdown.csv", 
#          row.names=FALSE)
#write.csv(test_bind, "[Consolidated] 2022 Provincial Cities Score.csv",
#          row.names=FALSE)

## Consolidate with HUC - need to run 2022 DTI Competitiveness Data first
huc_comb <- left_join(huc_2022_urls, hucs, by = "lgu") %>%
  select(-c("url_2022", "region")) %>%
  mutate_if(grepl("score|rank|population", colnames(.)), 
            ~as.numeric(as.character(.))) %>%
  add_column(category = "HUC", .after = "innov_score")

colnames(huc_comb) <- c("lgu_rank", "lgu_score", "lgu", 
                        "province", "eco_rank", "eco_score", 
                        "gov_rank", "gov_score", "infra_rank", 
                        "infra_score", "resiliency_rank", "resiliency_score",
                        "innov_rank", "innov_score", "category",
                        "population")

conso <- bind_rows(test_bind, huc_comb) %>%
  mutate(population = prettyNum(population, big.mark = ',', 
                                scientific=FALSE))

#write.csv(conso, "[Consolidated] 2022 Provincial and HUC DTI Competitivenes Score.csv", row.names = FALSE)







## -- TEST CODE DO NOT RUN -- ##

#n <-1

#test_scores <- data.frame(matrix(ncol = 14))

#colnames(test_scores) <- c("lgu_links.lgu.n.", "X1", "X2", 
#                          "X3", "X4", "X5", "X6", "X7", "X8", "X9",
#                          "X10", "X11", "X12", "X13")

#lgu_url <- read_html(lgu_links$lgu_link[n])

#lgu_prov <- lgu_url %>%
#  html_nodes(".economy-characteristics-table tr:nth-child(3) td:nth-child(4)") %>%
#  html_text()

#lgu_rank <- lgu_url %>%
#  html_nodes(".col-md-6 tr:nth-child(1) td") %>% 
#  html_text() %>%
#  str_replace("th", "") %>%
#  trimws()

#lgu_score <- lgu_url %>%
#  html_nodes("input.knob") %>%
#  html_attr("value")

#lgu_rankscore <- lgu_url %>%
#  html_nodes(".col-md-4 td") %>%
#  html_text() %>%
#  trimws()

####long format
####ranks <- grep("st|nd|rd|th", lgu_rankscore, value = TRUE)
####rank_val <- str_replace(ranks, "(?<=\\d)(st|nd|rd|th)","")

#ranks <- lgu_rankscore %>%
#  str_subset("(st|nd|th|rd)") %>% # filter only those containing st, nd, th, rd
#  str_replace("(st|nd|th|rd)", "") # remove the st, nd, th, rd suffix
#scores <- str_extract(lgu_rankscore, "^(?!.*\\b(?:st|nd|rd|th)\\b)\\s*\\d+\\.?\\d*$")
#scores_alt <- lgu_rankscore[!grepl("(st|nd|th|rd)", lgu_rankscore)]

#temp_scores1 <- cbind(lgu_prov, lgu_rank, lgu_score)
#temp_scores2 <- cbind(ranks, scores) %>%
#  as.numeric() %>%  # convert values to numeric
#  matrix(ncol = 10, byrow = TRUE) # create a matrix with 5 columns

#conso_tmp <- cbind(temp_scores1, temp_scores2)
#conso_tmp <- conso_tmp %>%
#  matrix(ncol = 13, byrow = TRUE) %>%
#  data.frame() -> tempdf_scores

#test_scores <- rbind(test_scores, 
#                     data.frame(lgu_links$lgu[n], 
#                                tempdf_scores))

#test_scores <- test_scores[-1,]
#test_test_scores <- test_scores
#colnames(test_test_scores) <- c("lgu", "province", "lgu_rank", "lgu_score", 
#                           "eco_rank", "gov_rank", "infra_rank",
#                           "resiliency_rank", "innov_rank", "eco_score", 
#                           "gov_score", "infra_score", "resiliency_score",
#                           "innov_score")

#cols_arrange <- c("province","lgu", "lgu_rank", "lgu_score", 
#                  "eco_rank", "eco_score", "gov_rank", 
#                  "gov_score", "infra_rank", "infra_score", 
#                  "resiliency_rank", "resiliency_score",
#                  "innov_rank", "innov_score")

#test_test_scores <- test_test_scores[, cols_arrange]
