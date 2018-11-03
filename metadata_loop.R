# Rvesting information on Top 5 articles (AER, QJE, JPE, Ecta [wiley and pre-wiley], REStud) using RePEc metadata.
# Citation data from google scholar not included, can be appended with rPython - see Card and Della Vigna (2013)

if (!require("pacman")) install.packages("pacman")
pacman::p_load('rvest', 'curl', 'psych', 'plyr', 'data.table', 'ggplot2', 'humaniformat')

if (Sys.getenv("HOME") == "/Users/gforte") {
  setwd(paste0(Sys.getenv("HOME"),'/Dropbox/git/evidencefrom'))
} else {
  setwd(dirname(file.choose()))
}

remove(list = ls())

#EconPapers Journal IDs
#AER = "aea/aecrev"        JPE = "ucp/jpolec"     QJE = "oup/qjecon"
#REStud = "oup/restud"     Ecta = "ecm/emetrp"    Ecta_w = "wly/emetrp"

#==== Locals ====

delet_this <- paste0("erratum|comment|minutes of the|report of the|corrigendum|Appendix|",
                     "Comment|Comments|Reply|Corrigendum|Response|: A Correction|: Discussion.|",
                     "Symposium on|Editorial Announcement")

nowant     <- paste0("Classification-JEL|Keywords:|Month:|File-Format:|File-Restriction:|",
                     "File-Function|data.zip|_app.pdf|_ds.zip|See http|Handle: RePEc|Note:",
                     " DOI:|/aer/data/|aer/contents/|/aer/app/|articles/attachments|content",
                     "/file|Author-Email|Author-Workplace")

#==== 1. QJE ====
#Coverage: All volumes.
#For some reason 2011-2014 QJE articles have a format different from the other ~130 years.

URL <- "ftp://ftp.repec.org/opt/ReDIF/RePEc/oup/qjecon/"
pg <- html_text(read_html(URL)) %>% strsplit(., "-rw-r--r--")
qjefiles <- pg[[1]] %>% gsub(".*    ", "",.) %>% gsub(".* ", "",.) %>% gsub("\n", "",.) %>% .[nchar(.)>0] %>%
  paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/oup/qjecon/", .)

df_articles <- data.table()

for (yr in qjefiles) {
  
  urli <- yr
  url  <- urli %>% read_html(.) %>% html_text(.) %>% iconv(., "UTF-8", "UTF-8",sub='')
  
  if(length(grep("\\n", url))==1){
    url_cut <- url %>% strsplit(.[[1]], split = "\n") %>% unlist(.) %>% gsub("\r", "",.)
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }

  url_cut <- url_cut[!grepl(nowant, url_cut)] %>% .[. != ""] %>% trimws(.) %>% .[. != ""] %>% rle(.)$values
  
  #Create a data frame for each article eliminating unwanted details; then, generate
  #an Author list() column and append all articles.
  #We play with the "Template-Type: ReDIF-Article 1.0" lines.
  
  article_list <- list()
  m <- 1
  o <- 1
  
  for (n in 1:length(url_cut)) {
    if(url_cut[n] == "Template-Type: ReDIF-Article 1.0" & o!=n) {
      j <- o+1
      k <- n-1
      
      foo <- assign(paste0("article", m), url_cut[j:k])
      article_list[[m]]<- foo
      q <- which(grepl("Journal:", foo)==T) - 1
      l <- which(grepl("Abstract:", foo)==T)
      
      if(length(q)==0) { #If a paper has no journal info, add it
        lt <- l-1
        foo <- c(foo[1:lt], "Journal: Quarterly Journal of Economics", foo[l:length(foo)])
        q <- which(grepl("Journal:", foo)==T) - 1
      }
      if(length(l)==0) { #If a paper has no abstract info, add an empty abstract.
        foo <- c(foo, "Abstract:")
        l <- q
      }
      
      #Some files have Abstract before journal, others after; some don't even have journal - different behaviours
      qt <- q+1
      if(qt>l & q!=l){ #If the Journal comes after the abstract, we use it as a point of reference.
        foo1 <- paste(article_list[[m]][l:q], collapse = "")
        foo[l] <- foo1
        ll <- l + 1
        foo <- foo[- (ll:q) ]
      }
      else {
        if(qt<l){ #If the Journal comes before the abstract, we temporarily change point of reference to foo length.
          q <- length(foo) - 1
          foo1 <- paste(article_list[[m]][l:q], collapse = "")
          foo[l] <- foo1
          ll <- l + 1
          foo <- foo[- (ll:q) ]
        }
      }
      foo <- trimws(foo)
      article_list[[m]]<- foo
      foo2 <- paste(article_list[[m]][grep("Author-Name:", article_list[[m]])], sep="")
      foo2[grep("Author-Name", foo2)] <- sub(",", "", foo2[grep("Author-Name", foo2)])
      foo2[1] <- paste(foo2, collapse = "")
      foo2 <- foo2[1] %>% gsub("^Author-Name: ","",.) %>% gsub("Author-Name:",",",.) %>%
        gsub("  ", " ",.) %>% gsub(" ,",",",.) %>% trimws(.) %>%
        unlist(strsplit(., ", "))
      foo3 <- article_list[[m]][- grepl("Author-Name:", article_list[[m]])]
      foo3 <- c(foo2, foo3)
      foo4 <- data.table(t(foo3))
      names_list <- c("URL", "Year", "Volume", "Issue", "Pages", "Journal", "Abstract", "Title")
      for (z in names_list) {
        foo5 <- which(grepl(paste0(z, ":"), foo3))
        colnames(foo4)[foo5] <- z
      }
      foo4[,-1] <- sub(".*?: ", "", as.matrix(foo4[,-1]))
      colnames(foo4)[grep("^X", colnames(foo4))] <- "Author"
      foo4$Authors <- list(foo2)
      foo4[colnames(foo4)=="Author"] <- NULL
      end <- ncol(foo4) - 1
      foo4 <- foo4[,c(ncol(foo4), 1:end)] #ncol(foo4) is the Authors column as it gets appended at the end.
      df_articles <- rbind.fill(df_articles,foo4) #switch to rbind when modifying code to find errors.
      m <- m+1
      o <- n
    }
  }
}


#<< The "The" is silent >>
df_articles$Journal <- sub("The Quarterly Journal of Economics", "Quarterly Journal of Economics", df_articles$Journal)

#I do not adjust for ReDIF characters, eg. \" instead of ", as it takes time and is currently unnecessary.

#Eliminating prefaces and other amenities; Sorting
qje_dt <- df_articles
qje_dt <- unique(qje_dt)
qje_dt <- subset(qje_dt, Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
qje_dt$gr <- grepl(delet_this, qje_dt$Title)
qje_dt <- qje_dt[!grepl(delet_this, qje_dt$Title),]
qje_dt$gr <- NULL
qje_dt$Issue[grep("_Part", qje_dt$Issue)] <- substr(qje_dt$Issue[grep("_Part", qje_dt$Issue)], 0, 1)
qje_dt$Year <- as.numeric(qje_dt$Year)
sum(is.na(qje_dt$Year))
qje_dt$Year[is.na(qje_dt$Year) & qje_dt$Volume >= 132] <- 2017 #Articles from recent volume are missing year
qje_dt$Volume <- as.numeric(qje_dt$Volume)
#qje_dt$Issue <- as.numeric(qje_dt$Issue) There is a Supplement issue in 1985, let us keep it named as Supplement.
qje_dt <- qje_dt[with(qje_dt, order(Year, Volume, Issue)),]

#Cleaning up
rm(j, k, l, ll, lt, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, foo4, foo5, names_list, upto, end, yr, vol,
   qjefiles, url, url_cut, urli)
rm(list = ls(pattern = "article"))

save(qje_dt, file = "qje_repec.Rdata")
write.csv(qje_dt, file = "qje_repec.csv", row.names = F)

#==== 2. REStud ====
#Coverage: All volumes.


URL <- "ftp://ftp.repec.org/opt/ReDIF/RePEc/oup/restud/"
pg <- html_text(read_html(URL)) %>% strsplit(., "-rw-r--r--")
restudfiles <- pg[[1]] %>% gsub(".*    ", "",.) %>% gsub(".* ", "",.) %>% gsub("\n", "",.) %>% .[nchar(.)>0]

df_articles <- data.table()

for (yr in restudfiles) {
  
  url <- yr %>% read_html(.) %>% html_text(.)
  url <- iconv(url, "UTF-8", "UTF-8",sub='')
  
  if(length(grep("\\n", url))==1){
    url_cut <- url %>% strsplit(.[[1]], split = "\n") %>% unlist(.) %>% gsub("\r", "",.) 
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }
  
  url_cut <- url_cut[!grepl(nowant, url_cut)] %>% .[. != ""] %>% rle(.)$values

  #Create a data frame for each article eliminating unwanted details; then, generate an Author list()
  #column and append all articles.
  #We play with the "Template-Type: ReDIF-Article 1.0" lines.
  
  article_list <- list()
  m <- 1
  o <- 1
  
  for (n in 1:length(url_cut)) {
    if(url_cut[n] == "Template-Type: ReDIF-Article 1.0" & o!=n) {
      j <- o+1
      k <- n-1
      
      foo <- assign(paste0("article", m), url_cut[ j:k])
      article_list[[m]]<- foo
      q <- which(grepl("Journal:", foo)==T) - 1
      l <- which(grepl("Abstract:", foo)==T)
      
      if(length(q)==0) { #If a paper has no journal info, add it
        lt <- l-1
        foo <- c(foo[1:lt], "Journal: Quarterly Journal of Economics", foo[l:length(foo)])
        q <- which(grepl("Journal:", foo)==T) - 1
      }
      if(length(l)==0) { #If a paper has no abstract info, add an empty abstract.
        foo <- c(foo, "Abstract:")
        l <- q
      }
      
      #Some files have Abstract before journal, others after; some don't even have journal - different behaviours
      qt <- q+1
      if(qt>l & q!=l){ #If the Journal comes after the abstract, we use it as a point of reference.
        foo1 <- paste(article_list[[m]][l:q], collapse = "")
        foo[l] <- foo1
        ll <- l + 1
        foo <- foo[- (ll:q) ]
      }
      else {
        if(qt<l){ #If the Journal comes before the abstract, we temporarily change point of reference to foo length.
          q <- length(foo) - 1
          foo1 <- paste(article_list[[m]][l:q], collapse = "")
          foo[l] <- foo1
          ll <- l + 1
          foo <- foo[- (ll:q) ]
        }
      }
      foo <- trimws(foo)
      article_list[[m]]<- foo
      foo2 <- paste(article_list[[m]][grep("Author-Name:", article_list[[m]])], sep="")
      foo2[grep("Author-Name", foo2)] <- sub(",", "", foo2[grep("Author-Name", foo2)])
      foo2[1] <- paste(foo2, collapse = "")
      foo2 <- foo2 %>% .[1] %>% gsub("^Author-Name: " , "" ,.) %>% gsub("Author-Name:" , "," ,.) %>%
        gsub("  " , " " ,.) %>% gsub(" ," , "," ,.) %>% trimws(.) %>% unlist(strsplit(., ", "))
      foo3 <- article_list[[m]][- grepl("Author-Name:", article_list[[m]])]
      foo3 <- c(foo2, foo3)
      foo4 <- data.table(t(foo3))
      names_list <- c("URL", "Year", "Volume", "Issue", "Pages", "Journal", "Abstract", "Title")
      for (z in names_list) {
        foo5 <- which(grepl(paste0(z, ":"), foo3))
        colnames(foo4)[foo5] <- z
      }
      foo4[,-1] <- sub(".*?: ", "", as.matrix(foo4[,-1]))
      colnames(foo4)[grep("^X", colnames(foo4))] <- "Author"
      foo4$Authors <- list(foo2)
      foo4[colnames(foo4)=="Author"] <- NULL
      end <- ncol(foo4) - 1
      foo4 <- foo4[,c(ncol(foo4), 1:end)] #ncol(foo4) is the Authors column as it gets appended at the end.
      df_articles <- rbind.fill(df_articles,foo4) #switch to rbind when modifying code to find errors.
      m <- m+1
      o <- n
    }
  }
}


#<< The "The" is silent >>
df_articles$Journal <- sub("The Review of Economic Studies", "Review of Economic Studies", df_articles$Journal)

#I do not adjust for ReDIF characters, eg. \" instead of ", as it takes time and is currently unnecessary.

#Eliminating prefaces and other amenities; Sorting
restud_dt <- df_articles
restud_dt <- unique(restud_dt)
restud_dt <- subset(restud_dt, Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
restud_dt$gr <- grepl(delet_this, restud_dt$Title)
restud_dt <- restud_dt[!grepl(delet_this, restud_dt$Title),]
restud_dt$gr <- NULL
restud_dt$Issue[grep("_Part", restud_dt$Issue)] <- substr(restud_dt$Issue[grep("_Part", restud_dt$Issue)], 0, 1)
restud_dt$Year <- as.numeric(restud_dt$Year)
sum(is.na(restud_dt$Year))
restud_dt$Year[is.na(restud_dt$Year)] <- 1932 + restud_dt$Volume 

restud_dt$Volume <- as.numeric(restud_dt$Volume)
restud_dt <- restud_dt[with(restud_dt, order(Year, Volume, Issue)),]

#Cleaning up
rm(j, k, l, ll, lt, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, foo4, foo5, names_list, upto, end, yr, vol,
   restudfiles, url, url_cut, urli)
rm(list = ls(pattern = "article"))

save(restud_dt, file = "restud_repec.Rdata")
write.csv(restud_dt, file = "restud_repec.csv", row.names = F)

#==== 3. AER ====
#Coverage: 1969 onwards (AER start: 1911)

URL <- "ftp://ftp.repec.org/opt/ReDIF/RePEc/aea/aecrev/"
pg <- html_text(read_html(URL)) %>% strsplit(., "-rw-r--r--")
aerfiles <- pg[[1]] %>% gsub(".*    ", "",.) %>% gsub(".* ", "",.) %>% gsub("\n", "",.) %>% .[nchar(.)>0] %>%
  paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/aea/aecrev/", .)

df_articles <- data.table()

for (yr in aerfiles) {
  urli <- yr
  url <- urli %>% read_html(.) %>% html_text(.)
  if(length(grep("\\n", url))==1){
    url_cut <- url %>% iconv(., "UTF-8", "UTF-8",sub='') %>% strsplit(.[[1]], split = "\n") %>% unlist(.) %>%
      gsub("\r", "", .)
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }

  url_cut <- url_cut[!grepl(nowant, url_cut)] %>% .[. != ""] %>% trimws(.) %>% rle(.)$values
  
  #Create a data frame for each article eliminating unwanted details; then, generate an Author list()
  #column and append all articles.
  #We play with the "Template-Type: ReDIF-Article 1.0" lines.
  
  article_list <- list()
  m <- 1
  o <- 1
  
  for (n in 1:length(url_cut)) {
    if(url_cut[n] == "Template-Type: ReDIF-Article 1.0" & o!=n) {
      j <- o+1
      k <- n-1
      
      foo <- assign(paste0("article", m), url_cut[ j:k])
      article_list[[m]]<- foo
      q <- which(grepl("Journal:", foo)==T) - 1
      qt <- which(grepl("Journal:", foo)==T)
      l <- which(grepl("Abstract:", foo)==T)
      #Some files have Abstract before journal, others after - different behaviours
      if(length(l)==0) {
        foo <- c(foo, "Abstract:")
        l <- q
        qt <- q+1
      }
      if(qt>=l){
        if(q!=l){
          foo1 <- paste(article_list[[m]][l:q], collapse = "")
          foo[(grepl("Abstract:", foo)==T)] <- foo1
          ll <- l + 1
          foo <- foo[- (ll:q) ]
        }
      }
      else {
        if(qt<l){
          q <- length(foo) - 1
          foo1 <- paste(article_list[[m]][l:q], collapse = "")
          foo[(grepl("Abstract:", foo)==T)] <- foo1
          ll <- l + 1
          foo <- foo[- (ll:q) ]
        }
      }
      article_list[[m]]<- foo
      foo2 <- paste(article_list[[m]][grep("Author-Name:", article_list[[m]])], sep="")
      foo2[grep("Author-Name", foo2)] <- sub(",", "", foo2[grep("Author-Name", foo2)])
      foo2[1] <- paste(foo2, collapse = "")
      foo2 <- foo2 %>% .[1] %>% gsub("^Author-Name: " , "" ,.) %>% gsub("Author-Name:" , "," ,.) %>%
        gsub("  " , " " ,.) %>% gsub(" ," , "," ,.) %>% trimws(.) %>%
        unlist(strsplit(., ", "))
      foo3 <- article_list[[m]][- grepl("Author-Name:", article_list[[m]])]
      foo3 <- c(foo2, foo3)
      foo4 <- data.table(t(foo3))
      names_list <- c("URL", "Year", "Volume", "Issue", "Pages", "Journal", "Abstract", "Title")
      for (z in names_list) {
        name1 <- paste0(z, ":")
        foo5 <- which(grepl(name1, foo3))
        colnames(foo4)[foo5] <- z
      }
      foo4[,-1] <- sub(".*?: ", "", as.matrix(foo4[,-1]))
      colnames(foo4)[grep("^X", colnames(foo4))] <- "Author"
      foo4$Authors <- list(foo2)
      foo4[colnames(foo4)=="Author"] <- NULL
      end <- ncol(foo4) - 1
      foo4 <- foo4[,c(ncol(foo4), 1:end)]
      df_articles <- rbind.fill(df_articles,foo4)
      m <- m+1
      o <- n
    }
  }
}

df_articles$Abstract<- gsub("Abstract:", "", df_articles$Abstract)

#Some recent articles are missing year
#df_articles$Year[df_articles$Year=="" & df_articles$Volume ==84] <- 2017
sum(df_articles$Year=="")

#I do not adjust for ReDIF characters, eg. \" instead of ", as it takes time and is currently unnecessary.

#Eliminating prefaces and other amenities; Sorting
aer_dt <- df_articles
aer_dt <- unique(aer_dt)
aer_dt <- subset(aer_dt, Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
aer_dt$gr <- grepl(delet_this, aer_dt$Title)
aer_dt <- aer_dt[!grepl(delet_this, aer_dt$Title),]
aer_dt$gr <- NULL
aer_dt$Year <- as.numeric(aer_dt$Year)
aer_dt$Volume <- as.numeric(aer_dt$Volume)
aer_dt$Issue <- as.numeric(aer_dt$Issue)
aer_dt <- aer_dt[with(aer_dt, order(Year, Volume, Issue, Pages)),]

#Cleaning up
rm(j, k, l, ll, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, url, url_cut, urli, aerfiles, foo5, name1,
   foo4, names_list, upto, end, yr, vol)
rm(list = ls(pattern = "article"))

save(aer_dt, file = "aer_repec.Rdata")
write.csv(aer_dt, file = "aer_repec.csv", row.names = F)

#==== 4. JPE ====
#Coverage: All volumes.

URL <- "ftp://ftp.repec.org/opt/ReDIF/RePEc/ucp/jpolec/"
pg <- html_text(read_html(URL)) %>% strsplit(., "-rw-r--r--")
jpefiles <- pg[[1]] %>% gsub(".*    ", "",.) %>% gsub(".* ", "",.) %>% gsub("\n", "",.) %>% .[nchar(.)>0] %>%
  paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/ucp/jpolec/", .)


df_articles <- data.table()

for (yr in jpefiles) {
  urli <- yr
  url  <- urli %>% read_html(.) %>% html_text(.)
  if(length(grep("\\n", url))==1){
    url <- iconv(url, "UTF-8", "UTF-8",sub='')
    url_cut <- unlist(strsplit(url[[1]], split = "\n")) %>% gsub("\r", "", .)
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }

  url_cut <- url_cut[!grepl(nowant, url_cut)] %>% .[. != ""] %>% trimws(.) %>% rle(.)$values
  
  #Create a data frame for each article eliminating unwanted details; then, generate an Author list()
  #column and append all articles.
  #We play with the "Template-Type: ReDIF-Article 1.0" lines.
  
  article_list <- list()
  m <- 1
  o <- 1
  
  for (n in 1:length(url_cut)) {
    if(url_cut[n] == "Template-Type: ReDIF-Article 1.0" & o!=n) {
      j <- o+1
      k <- n-1
      
      foo <- assign(paste0("article", m), url_cut[ j:k])
      article_list[[m]]<- foo
      q <- which(grepl("Journal:", foo)==T) - 1
      qt <- which(grepl("Journal:", foo)==T)
      l <- which(grepl("Abstract:", foo)==T)
      #Some files have Abstract before journal, others after - different behaviours
      if(length(l)==0) {
        foo <- c(foo, "Abstract:")
        l <- q
        qt <- q+1
      }
      if(qt>=l){
        if(q!=l){
          foo1 <- paste(article_list[[m]][l:q], collapse = "")
          foo[(grepl("Abstract:", foo)==T)] <- foo1
          ll <- l + 1
          foo <- foo[- (ll:q) ]
        }
      }
      else {
        if(qt<l){
          q <- length(foo) - 1
          foo1 <- paste(article_list[[m]][l:q], collapse = "")
          foo[(grepl("Abstract:", foo)==T)] <- foo1
          ll <- l + 1
          foo <- foo[- (ll:q) ]
        }
      }
      article_list[[m]]<- foo
      foo2 <- paste(article_list[[m]][grep("Author-Name:", article_list[[m]])], sep="")
      foo2[grep("Author-Name", foo2)] <- sub(",", "", foo2[grep("Author-Name", foo2)])
      foo2[1] <- paste(foo2, collapse = "")
      foo2 <- foo2[1] %>% gsub("^Author-Name: " , "" ,.) %>% gsub("Author-Name:" , "," ,.) %>%
        gsub("  " , " " ,.) %>% gsub(" ," , "," ,.) %>% trimws(.) %>%
        unlist(strsplit(., ", "))
      foo3 <- article_list[[m]][- grepl("Author-Name:", article_list[[m]])]
      foo3 <- c(foo2, foo3)
      foo4 <- data.table(t(foo3))
      names_list <- c("URL", "Year", "Volume", "Issue", "Pages", "Journal", "Abstract", "Title")
      for (z in names_list) {
        name1 <- paste0(z, ":")
        foo5 <- which(grepl(name1, foo3))
        colnames(foo4)[foo5] <- z
      }
      foo4[,-1] <- sub(".*?: ", "", as.matrix(foo4[,-1]))
      colnames(foo4)[grep("^X", colnames(foo4))] <- "Author"
      foo4$Authors <- list(foo2)
      foo4[colnames(foo4)=="Author"] <- NULL
      end <- ncol(foo4) - 1
      foo4 <- foo4[,c(ncol(foo4), 1:end)]
      df_articles <- rbind.fill(df_articles,foo4)
      m <- m+1
      o <- n
    }
  }
}

df_articles$Abstract<- gsub("Abstract:", "", df_articles$Abstract)

#Some recent articles are missing year
#df_articles$Year[df_articles$Year=="" & df_articles$Volume ==84] <- 2017
sum(df_articles$Year=="")

#I do not adjust for ReDIF characters, eg. \" instead of ", as it takes time and is currently unnecessary.

#Eliminating prefaces and other amenities; Sorting
jpe_dt <- df_articles
jpe_dt <- unique(jpe_dt)
jpe_dt <- subset(jpe_dt, Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
jpe_dt$gr <- grepl(delet_this, jpe_dt$Title)
jpe_dt <- jpe_dt[!grepl(delet_this, jpe_dt$Title),]
jpe_dt$gr <- NULL
jpe_dt$Year <- as.numeric(jpe_dt$Year)
jpe_dt$Volume <- as.numeric(jpe_dt$Volume)
jpe_dt$Issue <- as.numeric(jpe_dt$Issue)
jpe_dt <- jpe_dt[with(jpe_dt, order(Year, Volume, Issue, Pages)),]

#Cleaning up
rm(j, k, l, ll, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, url, url_cut, urli, jpefiles, foo5, name1,
   foo4, names_list, upto, end, yr, vol)
rm(list = ls(pattern = "article"))

save(jpe_dt, file = "jpe_repec.Rdata")
write.csv(jpe_dt, file = "jpe_repec.csv", row.names = F)

#==== 5. ECTA ====
#Coverage: All volumes

#WLY is the current one to be increased in the future.
URL <- "ftp://ftp.repec.org/opt/ReDIF/RePEc/wly/emetrp/"
pg <- html_text(read_html(URL)) %>% strsplit(., "-rw-r--r--")
wly <- pg[[1]] %>% gsub(".*    ", "",.) %>% gsub(".* ", "",.) %>% gsub("\n", "",.) %>% .[nchar(.)>0] %>%
  .[!grepl(".html",.)] %>% paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/wly/emetrp/", .)

#ECM is the pre-Wiley Econometrica.
URL <- "ftp://ftp.repec.org/opt/ReDIF/RePEc/ecm/emetrp/"
pg <- html_text(read_html(URL)) %>% strsplit(., "-rw-r--r--")
ecm <- pg[[1]] %>% gsub(".*    ", "",.) %>% gsub(".* ", "",.) %>% gsub("\n", "",.) %>% .[nchar(.)>0] %>%
  paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/ecm/emetrp/", .)

ectafiles <- c(wly, ecm)

df_articles <- data.table()

for (yr in ectafiles) {
  
  urli <- yr
  url <- urli %>% read_html(.) %>% html_text(.)
  if(length(grep("\\n", url))==1){
    url_cut <- url %>% iconv(., "UTF-8", "UTF-8",sub='') %>% strsplit(.[[1]], split = "\n") %>% 
      unlist(.) %>% gsub("\r", "",.) 
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }

  url_cut <- url_cut[!grepl(nowant, url_cut)] %>% .[. != ""] %>% trimws(.) %>% .[. != ""] %>% rle(.)$values
  
  #Create a data frame for each article eliminating unwanted details; then, generate an Author list()
  #column and append all articles.
  #We play with the "Template-Type: ReDIF-Article 1.0" lines.
  
  article_list <- list()
  m <- 1
  o <- 1
  
  for (n in 1:length(url_cut)) {
    if(url_cut[n] == "Template-Type: ReDIF-Article 1.0" & o!=n) {
      j <- o+1
      k <- n-1
      
      foo <- assign(paste0("article", m), url_cut[ j:k])
      article_list[[m]]<- foo
      q <- which(grepl("Journal:", foo)==T) - 1
      qt <- which(grepl("Journal:", foo)==T)
      l <- which(grepl("Abstract:", foo)==T)
      lt <- l-1
      if(length(q)==0) {foo <- c(foo[1:lt], "Journal: Econometrica", foo[l:length(foo)])
      q <- which(grepl("Journal:", foo)==T) - 1
      qt <- which(grepl("Journal:", foo)==T)
      }
      
      #Some files have Abstract before journal, others after - different behaviours
      if(length(l)==0) {
        foo <- c(foo, "Abstract:")
        l <- q
        qt <- q+1
      }
      if(qt>=l){
        if(q!=l){
          foo1 <- paste(article_list[[m]][l:q], collapse = "")
          foo[(grepl("Abstract:", foo)==T)] <- foo1
          ll <- l + 1
          foo <- foo[- (ll:q) ]
        }
      }
      else {
        if(qt<l){
          q <- length(foo) - 1
          foo1 <- paste(article_list[[m]][l:q], collapse = "")
          foo[(grepl("Abstract:", foo)==T)] <- foo1
          ll <- l + 1
          foo <- foo[- (ll:q) ]
        }
      }
      article_list[[m]]<- foo
      foo2 <- paste(article_list[[m]][grep("Author-Name:", article_list[[m]])], sep="")
      foo2[grep("Author-Name", foo2)] <- sub(",", "", foo2[grep("Author-Name", foo2)])
      foo2[1] <- paste(foo2, collapse = "")
      foo2 <- foo2 %>% .[1] %>% gsub("^Author-Name: " , "" ,.) %>% gsub("Author-Name:" , "," ,.) %>%
        gsub("  " , " " ,.) %>% gsub(" ," , "," ,.) %>% trimws(.) %>%
        unlist(strsplit(., ", "))
      foo3 <- article_list[[m]][- grepl("Author-Name:", article_list[[m]])]
      foo3 <- c(foo2, foo3)
      foo4 <- data.table(t(foo3))
      names_list <- c("URL", "Year", "Volume", "Issue", "Pages", "Journal", "Abstract", "Title")
      for (z in names_list) {
        name1 <- paste0(z, ":")
        foo5 <- which(grepl(name1, foo3))
        colnames(foo4)[foo5] <- z
      }
      foo4[,-1] <- sub(".*?: ", "", as.matrix(foo4[,-1]))
      colnames(foo4)[grep("^X", colnames(foo4))] <- "Author"
      foo4$Authors <- list(foo2)
      foo4[colnames(foo4)=="Author"] <- NULL
      end <- ncol(foo4) - 1
      foo4 <- foo4[,c(ncol(foo4), 1:end)]
      df_articles <- rbind.fill(df_articles,foo4)
      m <- m+1
      o <- n
    }
  }
}

df_articles$Abstract<- gsub("Abstract:", "", df_articles$Abstract)

#Some recent articles are missing year
#df_articles$Year[df_articles$Year=="" & df_articles$Volume ==84] <- 2017
sum(df_articles$Year=="")

#I do not adjust for ReDIF characters, eg. \" instead of ", as it takes time and is currently unnecessary.

#Eliminating prefaces and other amenities; Sorting
ecta_dt <- df_articles %>% unique(.) %>% subset(., Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
ecta_dt$gr <- grepl(delet_this, ecta_dt$Title)
ecta_dt <- ecta_dt[!grepl(delet_this, ecta_dt$Title),]
ecta_dt$gr <- NULL
ecta_dt$Year <- as.numeric(ecta_dt$Year)
ecta_dt$Volume <- as.numeric(ecta_dt$Volume)
ecta_dt$Issue <- as.numeric(ecta_dt$Issue)
ecta_dt <- ecta_dt[with(ecta_dt, order(Year, Volume, Issue, Pages)),]

#Cleaning up
rm(j, k, l, ll, lt, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, url, url_cut, urli, ectafiles, foo5, name1,
   wly, ecm, foo4, names_list, upto, end, yr, vol)
rm(list = ls(pattern = "article"))

save(ecta_dt, file = "ecta_repec.Rdata")
write.csv(ecta_dt, file = "ecta_repec.csv", row.names = F)

#==== 6. TOP5 MERGE ====

top5_dt <- rbind(aer_dt, ecta_dt, jpe_dt, qje_dt, restud_dt)
top5_dt$Abstract[top5_dt$Abstract=="Abstract:"] <- NA
save(top5_dt, file = "top5_dt.Rdata")
write.csv(top5_dt, file = "top5_repec.csv", row.names = F)

#==== 7. Finding evidence from" papers ====

load('top5_dt.Rdata')

library('ggplot2')

top5_dt$Title <- tolower(top5_dt$Title)
top5_dt$Evidence <- grepl("evidence from", top5_dt$Title)
frequency <- aggregate(top5_dt$Evidence, by=list(Year=top5_dt$Year, Journal=top5_dt$Journal), FUN=sum)
frequency <- subset(frequency, frequency$Year > 1969)

#-->> 7.1 Graphing "evidence from" over time ----

#All Top 5 taken together
frequency_all <- aggregate(frequency$x, by=list(Year=frequency$Year), FUN=sum)
frequency_all$events <- frequency_all$x

print(histogram <- (ggplot(data=frequency_all, aes(x=frequency_all$Year, y=frequency_all$events, width=.7))
                    + geom_col(col="red",aes(fill= frequency_all$events), size=0)
                    + xlab('Year of Publication') + ylab('Appearances in Top 5 Journals'))
      + scale_x_continuous(breaks=c(seq(1970,2010,by=5),2017))
      + scale_y_continuous(breaks=c(seq(0,40,by=5)), minor_breaks = seq(0,40, by=1))
      + scale_fill_gradient("Count", low="green", high="red")
      + guides(fill=guide_legend(title=NULL, reverse=T)) + ggtitle("Number of \"evidence from\" articles over the years")
      + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5))
      + theme(legend.position=c(0,1), legend.justification=c(0, 1),
              legend.direction="vertical", legend.background = element_rect(colour = NA, fill = NA))
)
ggsave("frequency_all_hist.png")

#By Journal
frequency$events <- frequency$x
frequency$journal <- reorder(frequency$Journal, frequency$events)
print(histogram <- (ggplot(data=frequency, aes(x=frequency$Year, y=frequency$events, width=.7))
                    + geom_col(aes(fill= frequency$journal), size=0.25, color="black")
                    + xlab('Year of Publication') + ylab('Appearances in Top 5 Journals'))
      + scale_x_continuous(breaks=c(seq(1970,2010,by=5),2017))
      + scale_y_continuous(breaks=c(seq(0,40,by=5)), minor_breaks = seq(0,40, by=1))
      + theme(legend.position=c(0,1), legend.justification=c(0, 1),
              legend.direction="vertical", legend.background = element_rect(colour = NA, fill = NA))
      + guides(fill=guide_legend(title=NULL, reverse=T)) + ggtitle("Number of \" evidence from\" articles over the years")
      + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5)) + scale_fill_brewer(palette = "RdBu")
)
ggsave("frequency_byjn_hist.png")


#==== 8. Author Names ====
load('top5_dt.Rdata')

top5_dt$Title <- tolower(top5_dt$Title)
top5_dt$Evidence <- grepl("evidence from", top5_dt$Title)
top5_evidence <- subset(top5_dt, top5_dt$Evidence==TRUE)

authors_evidence <- unlist(top5_dt$Authors[top5_dt$Evidence==TRUE])
head(authors_evidence)
