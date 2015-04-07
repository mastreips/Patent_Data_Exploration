library(xlsx)
library(XML) # HTML processing
options(stringsAsFactors = FALSE)

# Base URL
dir.create("informative")
base.url = 'http://www.uspto.gov/sites/default/files/documents/informative_opinions_worksheet_1-6-2015_.xlsx'
download.folder = './informative/'
download.file(base.url, destfile = "download.folder/uspto.csv", method="curl")
# csv directory
directory <- read.xlsx('./informative/informative_opinions_worksheet_1-6-2015_.xlsx', sheetIndex=1, header=TRUE)
# directory <- subset(directory, 
#                     !(school.type %in% c("Secondary (Year 9-15)", "Secondary (Year 11-15)")))
# 
# # Reading file obtained from stuff.co.nz obtained from here:
# # http://schoolreport.stuff.co.nz/index.html
# fairfax <- read.csv('SchoolReport_data_distributable.csv')
# fairfax <- subset(fairfax, !is.na(reading.WB)) 

# Defining schools with missing information
to.get <- merge(directory, fairfax, by = 'school.id', all.x = TRUE)
to.get <- subset(to.get, is.na(reading.WB))

# Looping over schools, to find name of PDF file
# with information and download it

for(school in to.get$school.id){
        
        # Read HTML file, extract PDF link name
        cat('Processing school ', school, '\n')
        doc.html <- htmlParse(paste(base.url, school, sep = ''))
        doc.links <- xpathSApply(doc.html, "//a/@href")
        pdf.url <- as.character(doc.links[grep('pdf', doc.links)])
        if(length(pdf.url) > 0) {
                pdf.name <- paste(download.folder, 'school_', school, '.pdf', sep = '')
                download.file(pdf.url, pdf.name, method = 'auto', quiet = FALSE, mode = "w",
                              cacheOK = TRUE, extra = getOption("download.file.extra"))
        }
}

#html links:

unzip("./informative/informative_opinions_worksheet_1-6-2015.zip")
xml <- xmlParse("sheet1.xml.rels")
src = xpathApply(xml, "//a[@href]", xmlGetAttr, "href")

root = xmlRoot(xml)
child = xmlChildren(root)
first = child[[1]]
xmlGetAttr(first, name = 'Target')
links <- as.data.frame(sapply(child, xmlGetAttr, "Target"))  #THIS DID IT!!!!
names.url <- as.character(links[1:195,])
pdf.url <- as.character(links[grep('pdf', links)])
write(names.url, file="list.txt")
#wget --no-proxy -i list.txt
# /usr/local/bin/pdftotext example.pdf
require(tm)
mycorpus <- Corpus(URISource("104722-033.txt"))
summary(mycorpus)
mycorpus <- tm_map(mycorpus, tolower)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, removeNumbers)
myStopwords <- c(stopwords('english'), '\n','\"','\f')
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
corpus_clean <- tm_map(mycorpus, PlainTextDocument)
dtm <- DocumentTermMatrix(corpus_clean)
freq_table <- as.data.frame(dtm)
inspect(dtm)
findFreqTerms(dtm, lowfreq=2)
findAssocs(dtm, "issue", .3)
test1 <- as.character(mycorpus$content)
test1


wordMatrix = as.data.frame(t(as.matrix(dtm)))
wordMatrix$names <- rownames(wordMatrix)
hCluster = hclust(dist(t(wordMatrix[1:583,1:2])))

###SPAM EXAMPLE
library(caret)
set.seed(3435) 
trainIndicator=rbinom(4601,size=1,prob=0.5) 
table(trainIndicator)
trainSpam=spam[trainIndicator==1,] 
testSpam=spam[trainIndicator==0,]
names(trainSpam)
table(trainSpam$type)
hClusterUpdated=hclust(dist(t(log10(trainSpam[,1:55]+1)))) 
plot(hClusterUpdated)
dist(t(log10(trainSpam[,1:55]+1)))
trainSpam$numType=as.numeric(trainSpam$type)-1
predictionModel=glm(numType~charDollar,family="binomial",data=trainSpam)
predictionTest=predict(predictionModel,testSpam)
predictedSpam=rep("nonspam",dim(testSpam)[1])
predictedSpam[predictionModel$fitted>0.5]="spam"
table(predictedSpam,testSpam$type)
confusionMatrix(predictedSpam,testSpam$type)

vars <- as.vector(names(trainSpam[,30:59]))
library(MASS)
fitAIC <- lm(numType ~ money + your + charDollar + capitalTotal, trainSpam)
fitAIC <- lm(numType ~ . -money -charDollar -your -capitalTotal, trainSpam)
step <- stepAIC(fitAIC, direction="both")
fitanova1 <- lm(numType ~ money + your + charDollar + capitalTotal, trainSpam)
fitanova1
summary(fitanova1)

##convert from spam to uspto
frequency <- trainSpam
tsnames <- c(rownames(wordMatrix[1:59,]))
colnames(frequency)<-tsnames
frequency <- frequency[,-58]
names(frequency)[58]<-"appeal"
inTrain <- createDataPartition(y=frequency$appeal, p=0.70, list=FALSE)
train <- frequency[inTrain,]
table(train$appeal)
