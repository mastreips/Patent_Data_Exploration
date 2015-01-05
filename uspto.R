library(XML)
library(httr)
library(tidyr)
library(dplyr)
library(plyr)

url <- "http://e-foia.uspto.gov/Foia/DispatchTTABISServlet?Objtype=serNo&SearchId=&precedIndMenu=Y&SearchRng=issDt&txtInput_StartDate=12%2F31%2F2012&txtInput_EndDate=12%2F29%2F2014&docTextSearch=&page=60"
html <- htmlTreeParse(url, useInternalNodes=T)
rootNode <- xmlRoot(html)
rootNode[[1]][[1]]


xpathSApply(html, "//title", xmlValue)
cont1 <- xpathSApply(html, "//td", xmlValue)

tables <- readHTMLTable(url)
cont2 <- data.frame(tables)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
cont2 <- tables[[which.max(n.rows)]]

xpathSApply(html, "//td[@id='col-citedby']", xmlValue)

//*[@id="efoiaLst"]

######
library(RCurl)
library(XML)
library(XML)
library(httr)
library(tidyr)
library(dplyr)
library(plyr)
library(plotrix)
## changed URL to have pages=1706 (default was 100)
theurl <- "http://e-foia.uspto.gov/Foia/DispatchTTABISServlet?SearchRng=issDt&precedIndMenu=Y&docTextSearch=&d-3995525-p=1&page=1706&txtInput_EndDate=12%2F29%2F2014&SearchId=&Objtype=serNo&txtInput_StartDate=12%2F31%2F2012"
webpage <- getURL(theurl)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)

pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)


###from example
# tablehead <- xpathSApply(pagetree, "//*/table[@class='wikitable sortable']/tr/th", xmlValue)
# results <- xpathSApply(pagetree, "//*/table[@class='wikitable sortable']/tr/td", xmlValue)

#my lame effort
# //*[@id="efoiaLst"] //*[@id="efoiaLst"]
tablehead <- xpathSApply(pagetree, "//tr/th", xmlValue)
results <- xpathSApply(pagetree, "//tr/td", xmlValue)
# results <- xpathSApply(pagetree, "//*[@id='efoiaLst']", xmlValue)
# results <- xpathSApply(pagetree, "//*[@class='odd']", xmlValue)
# content <- as.data.frame(matrix(results, ncol=12, byrow=TRUE))
# results2 <- as.character(results[22:744])
results2 <- as.character(results[22:20496]) ##to accomodate bigger data set. 
content <- as.data.frame(matrix(results2, ncol = 12, byrow = TRUE), stringsAsFactors=FALSE)
tablehead <- gsub("\n", "", tablehead)
tablehead <- gsub(" ", "", tablehead)
names(content)<-tablehead

content[,6]<- gsub("\n\t\t","", content$Issue)
content[,6] <- gsub("\n\t","", content$Issue)
content2<-tbl_df(content)
# content2 <- content2[1:60,]

labs <- names(table(content2$Issue))

# par(mar=c(20.1,18.1,4.1,2.1))
par(mar=c(5.1, 4.1, 4.1, 2.1))
with(content2, {
        x<-barplot(table(Issue), xaxt="n", ylim=c(0,30),main="TTAB Appeals by Issue\n(11/07/2014-12/12/2014)")
        text(cex=.8, x=x-.01, y=-.05, labs, xpd=TRUE, srt=45, adj=1)
})
dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()

# content2<- select(content, Issue, TTABDecision)
table1 <- as.data.frame(table(content2$Issue))
table2 <- filter(table1, Freq >=5 & Freq <=70) ##adjust to control for amount of variables. 
# barplot(table2$Freq) #easer plot
# par(mar=c(20.1,18.1,4.1,2.1))
par(mar=c(10.1,10.1,4.1,2.1))
labs2<-table2$Var1
table3 <- arrange(table2, table2$Freq)

with(table3, {
#         q<-barplot(table2$Freq, log="y")
        q<-barplot(table3$Freq)
        text(cex=.8, x=q-.01, y=-.05, labs2, xpd=TRUE, srt=45, adj=1)
})

# axis.break(2,200)

# table2 <- table1[3:17,]
# table2 <- table2[,2:3]
plot(table2$Var1, table2$Freq)
# table2 <- as.data.frame(t(table1))
# table2[2,3:17] <- as.nume1ric(table2[2,3:17])
# hist[table2]

# Convert character vector to dataframe
content <- as.data.frame(matrix(results, ncol = 8, byrow = TRUE))

# Clean up the results
content[,1] <- gsub("? ", "", content[,1])
tablehead <- gsub("? ", "", tablehead)
names(content) <- tablehead

tabletest <- xpathSApply(pagetree, "//table", xmlValue)
content <-as.data.frame(matrix(tabletest))

#ttrying rvest: http://www.r-bloggers.com/rvest-easy-web-scraping-with-r/
library(rvest)
url2 <- html("http://e-foia.uspto.gov/Foia/DispatchTTABISServlet?Objtype=serNo&SearchId=&precedIndMenu=Y&SearchRng=issDt&txtInput_StartDate=12%2F31%2F2012&txtInput_EndDate=12%2F29%2F2014&docTextSearch=&page=60")
table1 <- url2 %>%
  html_nodes("table") %>%
    html_table(fill=TRUE)
table2 <- as.data.frame(table1)
