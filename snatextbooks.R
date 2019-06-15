library(bib2df)
sndata<-read.csv("snatextbooks.csv",stringsAsFactors=FALSE)
snbib<-bib2df("snatextbooks.bib",separate_names=FALSE)

#Shorten cns
colnames(sndata)<-c("Timestamp","FaveBook","StudyArea","Gender","HighestDegree","YearDegree","GraduateStudent")

#Correct Cunningham et al.
sndata[grep("Dark",sndata$FaveBook),"FaveBook"]<- gsub("c\\(","by ",gsub("\"","",paste(snbib[12,"TITLE"][[1]],snbib[12,"AUTHOR"][[1]])))

#Assume blank gender is pnts
sndata[which(sndata$Gender==""),"Gender"]<-"Prefer not to say"

#Bind keys and merge
sndata$key<-""
lu<-sapply(strsplit(snbib$BIBTEXKEY,"[[:digit:]]"),function(x) x[[1]])
lu2<-sapply(lu,function(x) grep(x,sndata$FaveBook,ignore.case=TRUE))
for(i in 1:length(lu2)){
sndata$key[lu2[[i]]]<-snbib$BIBTEXKEY[[i]]
}

sndata<-merge(sndata,snbib,by.x="key",by.y="BIBTEXKEY")


png("SNATBRaw.png",width=1200,height=900,pointsize=18,res=100)
par(mar=c(5,14,4,1))
barplot(sort(prop.table(table(sndata$key)))*100,horiz=TRUE,las=1,main="Percent of Sample Favoring a \n Specific SNA Introductory Textbook (n=35)",xlab="Percent",xlim=c(0,25))
par(xpd=TRUE)
text(x=22,y=-2,labels="(@csmarcum 2019)")
dev.off()
