# Rvesting information on Top 5 (AER, QJE, JPE, Ecme [wiley and pre-wiley], REStud) using RePEc metadata. 
# Citation data from google scholar not included, can be appended with rPython - see Card and Della Vigna (2013)  

require('rvest')

library('rvest')
library('curl')
library('psych')
library('plyr')
library('ggplot2')

setwd("/Work/Research/evidence from")

#EconPapers Journal IDs
#AER    = "aea/aecrev"
#JPE    = "ucp/jpolec"
#QJE    = "oup/qjecon"
#REStud = "oup/restud"
#Ecme   = "ecm/emetrp"
#Ecme_w = "wly/emetrp"

remove(list = ls())

############## QJE ################
#For some reason 2011-2014 QJE articles have a format different from the other ~130 years.

#This is the final frame in which all articles will be held. 
qjefiles <- c(paste0("qjecon_", 1886:2010), paste0("qjecon_", 2015:2017), 
              "qje_2014", paste0(1261:1264, "-qjecon"), 
              paste0(1271:1274, "-qjecon"), paste0(1281:1284, "-qjecon"))

df_articles = data.frame()

#Loop for most years.
for (yr in qjefiles) { #People of the future - you'll have to replace the latest year.
  
  urli <- paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/oup/qjecon/", yr, ".rdf")
  url <- html_text(read_html(urli))
  url <- iconv(url, "UTF-8", "UTF-8",sub='')
  
  if(length(grep("\\n", url))==1){
    url_cut <- unlist(strsplit(url[[1]], split = "\n"))
    url_cut <- sub("\r", "", url_cut)
    url_cut <- sub("\r", "", url_cut) #This double appears to be necessary in some files.
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }
  
  nowant <- "Classification-JEL|Keywords:|Month:|File-Format:|File-Restriction:|File-Function|data.zip|_app.pdf|_ds.zip|See http|Handle: RePEc|Note: DOI:|/aer/data/|aer/contents/|/aer/app/|articles/attachments|content/file"
  url_cut <- url_cut[!grepl(nowant, url_cut)]
  url_cut <- url_cut[url_cut != ""]
  url_cut <- rle(url_cut)$values 
  
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
      foo <- trimws(foo, which=c("both"))
      article_list[[m]]<- foo
      foo2 <- paste(article_list[[m]][grep("Author-Name:", article_list[[m]])], sep="")
      foo2[grep("Author-Name", foo2)] <- sub(",", "", foo2[grep("Author-Name", foo2)])
      foo2[1] <- paste(foo2, collapse = "")
      foo2 <- foo2[1]
      foo2 <- gsub("^Author-Name: " , "" , foo2)
      foo2 <- gsub("Author-Name:" , "," ,  foo2)
      foo2 <- gsub("  " , " " ,  foo2)
      foo2 <- gsub(" ," , "," ,  foo2)
      foo2 <- trimws(foo2, which = c("both"))
      foo2 <- unlist(strsplit(foo2, ", "))
      foo3 <- article_list[[m]][- grep("Author-Name:", article_list[[m]])]
      #Is line 111 necessary? Can we not simply add Author-Name: to the grepl below?
      foo3 <- foo3[!grepl("File-Format|File-Restriction|Classification-JEL|Keywords:", foo3)]
      foo3 <- c(foo2, foo3)
      foo4 <- data.frame(t(foo3))
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
qje_df <- df_articles
qje_df <- unique(qje_df)
qje_df <- subset(qje_df, Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
delet_this <- "erratum|comment|minutes of the|report of the|corrigendum|Appendix|Comment|Comments|Reply|Corrigendum|Response|: A Correction|: Discussion."
qje_df$gr <- grepl(delet_this, qje_df$Title)
qje_df <- qje_df[!grepl(delet_this, qje_df$Title),]
qje_df$gr <- NULL
qje_df$Issue[grep("_Part", qje_df$Issue)] <- substr(qje_df$Issue[grep("_Part", qje_df$Issue)], 0, 1)
qje_df$Year <- as.numeric(qje_df$Year)
sum(is.na(qje_df$Year))
qje_df$Year[is.na(qje_df$Year) & qje_df$Volume >= 132] <- 2017 #Articles from recent volume are missing year 
qje_df$Volume <- as.numeric(qje_df$Volume)
#qje_df$Issue <- as.numeric(qje_df$Issue) There is a Supplement issue in 1985, let us keep it named as Supplement. 
qje_df <- qje_df[with(qje_df, order(Year, Volume, Issue)),] 

#Cleaning up
rm(j, k, l, ll, lt, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, foo4, foo5, names_list, upto, end, yr, vol, 
   qjefiles, url, url_cut, urli, nowant, delet_this)
rm(list = ls(pattern = "article"))

save(qje_df, file = "ReDif/qje_repec.Rdata")

############## REStud ################

df_articles = data.frame()

#People of the future - you'll have to replace the latest year.
restudfiles <- c(paste0("restud_", 1933:2001),paste0("restud_", 2015:2017), "restud_2014_2", "restud_81_3_4")
restudfiles <- c(paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/oup/restud/", restudfiles, ".rdf"), 
                 "ftp://ftp.repec.org/opt/ReDIF/RePEc/oup/restud/restud_uniqueE329.rdf")

#Loop for most years.
for (yr in restudfiles) { 
  
  url <- html_text(read_html(yr))
  url <- iconv(url, "UTF-8", "UTF-8",sub='')
  
  if(length(grep("\\n", url))==1){
    url_cut <- unlist(strsplit(url[[1]], split = "\n"))
    url_cut <- sub("\r", "", url_cut)
    url_cut <- sub("\r", "", url_cut) #This double appears to be necessary in some files.
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }
  
  nowant <- "Classification-JEL|Keywords:|Month:|File-Format:|File-Restriction:|File-Function|data.zip|_app.pdf|_ds.zip|See http|Handle: RePEc|Note: DOI:|/aer/data/|aer/contents/|/aer/app/|articles/attachments|content/file"
  url_cut <- url_cut[!grepl(nowant, url_cut)]
  url_cut <- url_cut[url_cut != ""]
  url_cut <- rle(url_cut)$values 
  
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
      foo <- trimws(foo, which=c("both"))
      article_list[[m]]<- foo
      foo2 <- paste(article_list[[m]][grep("Author-Name:", article_list[[m]])], sep="")
      foo2[grep("Author-Name", foo2)] <- sub(",", "", foo2[grep("Author-Name", foo2)])
      foo2[1] <- paste(foo2, collapse = "")
      foo2 <- foo2[1]
      foo2 <- gsub("^Author-Name: " , "" , foo2)
      foo2 <- gsub("Author-Name:" , "," ,  foo2)
      foo2 <- gsub("  " , " " ,  foo2)
      foo2 <- gsub(" ," , "," ,  foo2)
      foo2 <- trimws(foo2, which = c("both"))
      foo2 <- unlist(strsplit(foo2, ", "))
      foo3 <- article_list[[m]][- grep("Author-Name:", article_list[[m]])]
      #Is line 111 necessary? Can we not simply add Author-Name: to the grepl below?
      foo3 <- foo3[!grepl("File-Format|File-Restriction|Classification-JEL|Keywords:", foo3)]
      foo3 <- c(foo2, foo3)
      foo4 <- data.frame(t(foo3))
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
restud_df <- df_articles
restud_df <- unique(restud_df)
restud_df <- subset(restud_df, Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
delet_this <- "erratum|comment|minutes of the|report of the|corrigendum|Appendix|Comment|Comments|Reply|Corrigendum|Response|: A Correction|: Discussion.|Symposium on|Editorial Announcement"
restud_df$gr <- grepl(delet_this, restud_df$Title)
restud_df <- restud_df[!grepl(delet_this, restud_df$Title),]
restud_df$gr <- NULL
restud_df$Issue[grep("_Part", restud_df$Issue)] <- substr(restud_df$Issue[grep("_Part", restud_df$Issue)], 0, 1)
restud_df$Year <- as.numeric(restud_df$Year)
sum(is.na(restud_df$Year))
restud_df$Year[is.na(restud_df$Year) & restud_df$Volume >= 84] <- 2017 #Articles from recent volume are missing year 

restud_df$Volume <- as.numeric(restud_df$Volume)
#restud_df$Issue <- as.numeric(restud_df$Issue) There is a Supplement issue in 1985, let us keep it named as Supplement. 
restud_df <- restud_df[with(restud_df, order(Year, Volume, Issue)),] 

#Cleaning up
rm(j, k, l, ll, lt, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, foo4, foo5, names_list, upto, end, yr, vol, 
   restudfiles, url, url_cut, urli, nowant, delet_this)
rm(list = ls(pattern = "article"))

save(restud_df, file = "ReDif/restud_repec.Rdata")

############## AER ################

#This is the final frame in which all articles will be held. 
df_articles = data.frame()

aerfiles <- gsub(" ", "", c(
    apply(expand.grid("AER_", 89:100, "0", 1:5), 1, paste, collapse=""), apply(expand.grid("AER_", 101:103, "0", 1:7), 1, paste, collapse=""),
    apply(expand.grid("AER_", 104:106, "0", 1:9), 1, paste, collapse=""), apply(expand.grid("AER_", 104:106, 10:12), 1, paste, collapse=""),
    apply(expand.grid("AER_", 107,"0", 1:9), 1, paste, collapse=""), c(paste0("p", 1:8, "aer"), paste0("q", 1:9, "aer"), 
    paste0("r", 1:9, "aer"), paste0("s", 1:4, "aer"))
    ))

for (yr in aerfiles) { #People of the future - you'll have to replace the latest file.
  urli <- paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/aea/aecrev/", yr, ".rdf")
  url <- html_text(read_html(urli))
  if(length(grep("\\n", url))==1){
  url <- iconv(url, "UTF-8", "UTF-8",sub='')
  url_cut <- unlist(strsplit(url[[1]], split = "\n"))
  url_cut <- sub("\r", "", url_cut)
  url_cut <- sub("\r", "", url_cut) #This double is necessary in some files.
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }
  url_cut <- url_cut[url_cut != ""]
  nowant <- "Classification-JEL|Keywords:|Month:|File-Format:|File-Restriction:|File-Function|data.zip|_app.pdf|_ds.zip|See http|Handle: RePEc|Note: DOI:|/aer/data/|aer/contents/|/aer/app/|articles/attachments|content/file"
  url_cut <- url_cut[!grepl(nowant, url_cut)]
  url_cut <- trimws(url_cut, which=c("both"))
  url_cut <- rle(url_cut)$values 
  
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
       foo2 <- foo2[1]
       foo2 <- gsub("^Author-Name: " , "" , foo2)
       foo2 <- gsub("Author-Name:" , "," ,  foo2)
       foo2 <- gsub("  " , " " ,  foo2)
       foo2 <- gsub(" ," , "," ,  foo2)
       foo2 <- trimws(foo2, which = c("both"))
       foo2 <- unlist(strsplit(foo2, ", "))
       foo3 <- article_list[[m]][- grep("Author-Name:", article_list[[m]])]
       foo3 <- foo3[!grepl("File-Format|File-Restriction|Classification-JEL|Keywords:", foo3)]
       foo3 <- c(foo2, foo3)
       foo4 <- data.frame(t(foo3))
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
aer_df <- df_articles
aer_df <- unique(aer_df)
aer_df <- subset(aer_df, Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
delet_this <- "erratum|comment|minutes of the|report of the|corrigendum|Appendix|Comment|Comments|Reply|Corrigendum|Response|: A Correction|: Discussion."
aer_df$gr <- grepl(delet_this, aer_df$Title)
aer_df <- aer_df[!grepl(delet_this, aer_df$Title),]
aer_df$gr <- NULL
aer_df$Year <- as.numeric(aer_df$Year)
aer_df$Volume <- as.numeric(aer_df$Volume)
aer_df$Issue <- as.numeric(aer_df$Issue) 
aer_df <- aer_df[with(aer_df, order(Year, Volume, Issue, Pages)),] 

#Cleaning up
rm(j, k, l, ll, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, url, url_cut, urli, aerfiles, foo5, name1, 
   foo4, names_list, upto, end, yr, vol, delet_this, nowant)
rm(list = ls(pattern = "article"))

save(aer_df, file = "ReDif/aer_repec.Rdata")


############## JPE ################

#This is the final frame in which all articles will be held. 
df_articles = data.frame()

jpefiles <- gsub(" ", "", c(apply(expand.grid("JPEv", 77:114, "n", 1:6), 1, paste, collapse=""),
            apply(expand.grid("JPEv", 116:124, "n", 1:6), 1, paste, collapse=""),
            apply(expand.grid("JPEv", 115, "n", 4:6), 1, paste, collapse=""),
            apply(expand.grid("JPEv", 125, "n", 1:4), 1, paste, collapse=""),
            "JPEv107nS6", "JPEv112nS1", "JPEunsorted"))

for (yr in jpefiles) { #People of the future - you'll have to replace the latest file - most likely in expand.grid()
  urli <- paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/ucp/jpolec/", yr, ".repec.rdf")
  
  url <- html_text(read_html(urli))
  if(length(grep("\\n", url))==1){
    url <- iconv(url, "UTF-8", "UTF-8",sub='')
    url_cut <- unlist(strsplit(url[[1]], split = "\n"))
    url_cut <- sub("\r", "", url_cut)
    url_cut <- sub("\r", "", url_cut) #This double is necessary in some files.
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }
  url_cut <- url_cut[url_cut != ""]
  nowant <- "Classification-JEL|Keywords:|Month:|File-Format:|File-Restriction:|File-Function|data.zip|_app.pdf|_ds.zip|See http|Handle: RePEc|Note: DOI:|/aer/data/|aer/contents/|/aer/app/|articles/attachments|content/file"
  url_cut <- url_cut[!grepl(nowant, url_cut)]
  url_cut <- trimws(url_cut, which=c("both"))
  url_cut <- rle(url_cut)$values 
  
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
      foo2 <- foo2[1]
      foo2 <- gsub("^Author-Name: " , "" , foo2)
      foo2 <- gsub("Author-Name:" , "," ,  foo2)
      foo2 <- gsub("  " , " " ,  foo2)
      foo2 <- gsub(" ," , "," ,  foo2)
      foo2 <- trimws(foo2, which = c("both"))
      foo2 <- unlist(strsplit(foo2, ", "))
      foo3 <- article_list[[m]][- grep("Author-Name:", article_list[[m]])]
      foo3 <- foo3[!grepl("File-Format|File-Restriction|Classification-JEL|Keywords:", foo3)]
      foo3 <- c(foo2, foo3)
      foo4 <- data.frame(t(foo3))
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
jpe_df <- df_articles
jpe_df <- unique(jpe_df)
jpe_df <- subset(jpe_df, Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
delet_this <- "erratum|comment|minutes of the|report of the|corrigendum|Appendix|Comment|Comments|Reply|Corrigendum|Response|: A Correction|: Discussion."
jpe_df$gr <- grepl(delet_this, jpe_df$Title)
jpe_df <- jpe_df[!grepl(delet_this, jpe_df$Title),]
jpe_df$gr <- NULL
jpe_df$Year <- as.numeric(jpe_df$Year)
jpe_df$Volume <- as.numeric(jpe_df$Volume)
jpe_df$Issue <- as.numeric(jpe_df$Issue) 
jpe_df <- jpe_df[with(jpe_df, order(Year, Volume, Issue, Pages)),] 

#Cleaning up
rm(j, k, l, ll, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, url, url_cut, urli, jpefiles, foo5, name1, 
   foo4, names_list, upto, end, yr, vol, delet_this, nowant)
rm(list = ls(pattern = "article"))

save(jpe_df, file = "ReDif/jpe_repec.Rdata")


############## ECTA ################

#This is the final frame in which all articles will be held. 
df_articles = data.frame()

#WLY is the current one to be increased in the future.
wly <- c(apply(expand.grid("ftp://ftp.repec.org/opt/ReDIF/RePEc/wly/emetrp/",
                           "ECTA", 82:83, ".", 1:6, ".rdf"), 1, paste, collapse=""),
         apply(expand.grid("ftp://ftp.repec.org/opt/ReDIF/RePEc/wly/emetrp/",
                           "ECTA", 84, ".", 1:5, ".rdf"), 1, paste, collapse=""))
#ECM is the pre-Wiley Econometrica. 
ecm <- c(apply(expand.grid("ftp://ftp.repec.org/opt/ReDIF/RePEc/ecm/emetrp/",
                           "ECTA", 79:81, ".", 1:6, ".rdf"), 1, paste, collapse=""),
                           "ftp://ftp.repec.org/opt/ReDIF/RePEc/ecm/emetrp/ECTA78.6.rdf", 
                          paste0("ftp://ftp.repec.org/opt/ReDIF/RePEc/ecm/emetrp/ecta_", c(773:776,781:785), ".rdf"),
                          "ftp://ftp.repec.org/opt/ReDIF/RePEc/ecm/emetrp/emetrp_unique9319noauth.rdf")

ectafiles <- c(wly, ecm)

for (yr in ectafiles) { #People of the future - you'll have to replace the latest file - most likely in expand.grid()
  urli <- yr
  
  url <- html_text(read_html(urli))
  if(length(grep("\\n", url))==1){
    url <- iconv(url, "UTF-8", "UTF-8",sub='')
    url_cut <- unlist(strsplit(url[[1]], split = "\n"))
    url_cut <- sub("\r", "", url_cut)
    url_cut <- sub("\r", "", url_cut) #This double is necessary in some files.
  }
  if(length(grep("\\n", url))==0){
    url_cut <- unlist(strsplit(url[[1]], split = "\r"))
  }
  url_cut <- url_cut[url_cut != ""]
  nowant <- "Classification-JEL|Keywords:|Month:|File-Format:|File-Restriction:|File-Function|data.zip|_app.pdf|_ds.zip|See http|Handle: RePEc|Note: DOI:|/aer/data/|aer/contents/|/aer/app/|articles/attachments|content/file|Author-Email|Author-Workplace"
  url_cut <- url_cut[!grepl(nowant, url_cut)]
  url_cut <- trimws(url_cut, which=c("both"))
  url_cut <- rle(url_cut)$values 
  
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
      foo2 <- foo2[1]
      foo2 <- gsub("^Author-Name: " , "" , foo2)
      foo2 <- gsub("Author-Name:" , "," ,  foo2)
      foo2 <- gsub("  " , " " ,  foo2)
      foo2 <- gsub(" ," , "," ,  foo2)
      foo2 <- trimws(foo2, which = c("both"))
      foo2 <- unlist(strsplit(foo2, ", "))
      foo3 <- article_list[[m]][- grep("Author-Name:", article_list[[m]])]
      foo3 <- foo3[!grepl("File-Format|File-Restriction|Classification-JEL|Keywords:", foo3)]
      foo3 <- c(foo2, foo3)
      foo4 <- data.frame(t(foo3))
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
ecta_df <- df_articles
ecta_df <- unique(ecta_df)
ecta_df <- subset(ecta_df, Title!='foreword' & Title!='editors\' introduction' & Title!='preface')
delet_this <- "erratum|comment|minutes of the|report of the|corrigendum|Appendix|Comment|Comments|Reply|Corrigendum|Response|: A Correction|: Discussion."
ecta_df$gr <- grepl(delet_this, ecta_df$Title)
ecta_df <- ecta_df[!grepl(delet_this, ecta_df$Title),]
ecta_df$gr <- NULL
ecta_df$Year <- as.numeric(ecta_df$Year)
ecta_df$Volume <- as.numeric(ecta_df$Volume)
ecta_df$Issue <- as.numeric(ecta_df$Issue) 
ecta_df <- ecta_df[with(ecta_df, order(Year, Volume, Issue, Pages)),] 

#Cleaning up
rm(j, k, l, ll, lt, m, o, n, q, qt, z, foo, foo1, fool, foo2, foo3, url, url_cut, urli, ectafiles, foo5, name1, 
   wly, ecm, foo4, names_list, upto, end, yr, vol, delet_this, nowant)
rm(list = ls(pattern = "article"))

save(ecta_df, file = "ReDif/ecta_repec.Rdata")


############## TOP5 MERGE ################

top5_df <- rbind(aer_df, ecta_df, jpe_df, qje_df, restud_df)
top5_df$Abstract[top5_df$Abstract=="Abstract:"] <- NA
save(top5_df, file = "ReDif/top5_df.Rdata")


############## Finding guilty papers ################

setwd("/Work/Research/evidence from/ReDif")
load('top5_df.Rdata')

library('ggplot2')

top5_df$Title <- tolower(top5_df$Title)
top5_df$Evidence <- grepl("evidence from", top5_df$Title)
frequency <- aggregate(top5_df$Evidence, by=list(Year=top5_df$Year, Journal=top5_df$Journal), FUN=sum)
frequency <- subset(frequency, frequency$Year > 1969) 

############## Graphing the evolution over time ################

#All Top 5 taken together
frequency_all <- aggregate(frequency$x, by=list(Year=frequency$Year), FUN=sum)
frequency_all$events <- frequency_all$x

print(histogram <- (ggplot(data=frequency_all, aes(x=frequency_all$Year, y=frequency_all$events, width=.7)) 
                    + geom_col(col="red",aes(fill= frequency_all$events), size=0) + xlab('Year of Publication') + ylab('Appearances in Top 5 Journals'))
      + scale_x_continuous(breaks=c(seq(1970,2010,by=5),2017)) + scale_y_continuous(breaks=c(seq(0,40,by=5)), 
                                                                    minor_breaks = seq(0,40, by=1)) + scale_fill_gradient("Count", low="green", high="red") 
      + guides(fill=guide_legend(title=NULL, reverse=T)) + ggtitle("Number of \"evidence from\" articles over the years")
      + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5))
      + theme(legend.position=c(0,1), legend.justification=c(0, 1), legend.direction="vertical", legend.background = element_rect(colour = NA, fill = NA))
)
ggsave("frequency_all_hist.png")

#By Journal
frequency$events <- frequency$x
frequency$journal <- reorder(frequency$Journal, frequency$events)
print(histogram <- (ggplot(data=frequency, aes(x=frequency$Year, y=frequency$events, width=.7)) 
                    + geom_col(aes(fill= frequency$journal), size=0.25, color="black") + xlab('Year of Publication') + ylab('Appearances in Top 5 Journals'))
      + scale_x_continuous(breaks=c(seq(1970,2010,by=5),2017)) + scale_y_continuous(breaks=c(seq(0,40,by=5)), 
         minor_breaks = seq(0,40, by=1))  + theme(legend.position=c(0,1), legend.justification=c(0, 1), legend.direction="vertical", legend.background = element_rect(colour = NA, fill = NA))
      + guides(fill=guide_legend(title=NULL, reverse=T)) + ggtitle("Number of \" evidence from\" articles over the years")
      + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5)) + scale_fill_brewer(palette = "RdBu")
)
ggsave("frequency_byjn_hist.png")


############## Author Names ################
install.packages('humaniformat')
library('humaniformat')

top5_evidence <- subset(top5_df, top5_df$Evidence==TRUE)
top5_evidence$Authors[top5_evidence$Journal=="American Economic Review"] <- format_reverse(top5_evidence$Authors)
authors_evidence <- unlist(top5_df$Authors[top5_df$Evidence==TRUE])
head(authors_evidence)
