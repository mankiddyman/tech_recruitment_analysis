library(googlesheets4) # 
library(tidyverse)

#Read google sheets data into R
df <- read_sheet('https://docs.google.com/spreadsheets/d/1jeDflEQUPXLGlqDr9B8l5nFnOU72FyvL_tbfEeHth1Y/edit?resourcekey#gid=463492530')
df
#write functions to create every plot taking a df as argument?
#give them different dfs
#boom
#first ill print the questions 
print(colnames(df))
#ok so there are 13 questions

#setting appropriate collumns as factor
df <- as.data.frame(df)
#df[,2:11] <- as.factor(df[,2:11])
#df only has one response for cyber security , removing it
df <- df[-145,]
#######################
#have to code q3 as prescence or abscence for in house or outhouse
df$inhouse <- NA
df$outhouse <- NA
for(i in 1:nrow(df)){
  if(df[i,3]=="In-house recruiters (recruiters from the company you are applying to.)"){
    df$inhouse[i]=TRUE
    df$outhouse[i]=FALSE
  }
  if(df[i,3]=="Independent recruiters (agency, head hunters etc.)")
  {df$outhouse[i]=TRUE
   df$inhouse[i]=FALSE
  }
  if(df[i,3]=="Independent recruiters (agency, head hunters etc.), In-house recruiters (recruiters from the company you are applying to.)") {
    df$inhouse[i]=TRUE
    df$outhouse[i]=TRUE
  }
} 
#####################
#I want to see a histogram of 3 and 4 filtered by 2

library(ggwordcloud)
ggplot(data=df,aes(label=df[,13]))+
  geom_text_wordcloud()+
  theme_minimal()
text <- df[,13]
docs <- Corpus(VectorSource(text))
# Convert the text to lower case
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)# Text stemming
# docs <- tm_map(docs, stemDocument)


source('http://www.sthda.com/upload/rquery_wordcloud.r')
res <- rquery.wordcloud(x=text, type ="text", lang = "english",excludeWords=c("candidates","candidate","recruiters",'companys'),max.words=40,textStemming = FALSE,colorPalette = 'Reds')
head(res$freqTable)
#word cloud agrees

library(ggplot2)
library(stringr)
#####
p1 <- ggplot(data=df,aes(x=df[,5],fill=inhouse))+
  geom_histogram(position="dodge")+
  ggtitle(stringr::str_wrap(colnames(df[5])))+
  ylim(0,100)
p2 <-ggplot(data=df,aes(x=df[,5],fill=outhouse))+
  geom_histogram(position="dodge")+
  ggtitle(stringr::str_wrap(colnames(df[5])))+
  ylim(0,100)

p1
p2
library(ggpubr)
ggpubr::ggarrange(p1,p2)
p3 <- ggplot(data=df,aes(x=inhouse,y=df[,5],size=df[,5],colour=inhouse))+
  geom_jitter()+
  ggtitle(stringr::str_wrap(colnames(df[5])))+
  labs(size='Response')
p3
p4<- ggplot(data=df,aes(x=inhouse,y=df[,5],size=df[,5],colour=outhouse))+
  geom_jitter(shape=20)+
  ggtitle(stringr::str_wrap(colnames(df[5])))+
  labs(size='Response')
p4

####

p5 <- ggplot(data=df,aes(x=df[,5],y=df[,3],col=df[,3]))+
  geom_jitter()+
  scale_y_discrete(labels=c("Inh","Out","Mixed"))+
  theme(legend.position = "none")+ggtitle(stringr::str_wrap(colnames(df[5])))
p5
#regardless of recruitment strategy employees feel understood with inhouse feeling more understood, suggests insider knowledge is important
p6 <- ggplot(data=df,aes(x=df[,12],y=df[,3],col=df[,3]))+
  geom_count()+
  scale_y_discrete(labels=c("In-House","Out-House","Mixed"))+
  theme(legend.position = "none")+ggtitle("")+
  labs(x="Age",y="Recruiter type")
p6

#"To what extent do you agree with this\nstatement: \"My recruiter understood \nwhat my top skills were and how to \nmarketme to the hiring manager.\?"
ggsave(filename="bubble_plot_age_recruiter_type.png",plot=p6,width=10,units="cm")
#outhouse recruiters are assoicated with recruiting the latest talent so they should take heed
p7 <- ggplot(data=df,aes(x=df[,11],y=df[,3],col=df[,3]))+
  geom_count()+
  scale_y_discrete(labels=c("Inh","Out","Mixed"))+
  theme(legend.position = "none")+ggtitle("")+
  labs(x="Specialisation",y="Recruiter Type")+
  theme(axis.text.x= element_text(size=4.5))
p7
ggsave(filename="bubble_plot_specialisation_recruiter_type.png",plot=p7,width=10,units="cm")
#most industries are mixed recruitment or inhouse, recruitment
p8 <- ggplot(data=df,aes(x=df[,11],y=df[,5],col=df[,5]))+
  geom_count()+
  theme(legend.position = "none")+ggtitle("To what extent do you agree with this\nstatement:\"My recruiter understood what \nmy top skills were and how to market \nme to the hiring manager.\"")+
  labs(y='Response',x="Specialisation")+
  theme(axis.text.x= element_text(size=4.5))
p8
ggsave(filename="bubble_plot_specialisation_understood_skills.png",plot=p8,width=10,units="cm")

#understanding is lacking in IT sales , product, UX
col_index <- 4
p9 <- ggplot(data=df,aes(x=df[,11],y=df[,col_index],col=df[,col_index]))+
  geom_count()+
  theme(legend.position = "none")+ggtitle("To what extent do you agree with this \nstatement:\"My recruiter had sufficient\nknowledge about different softwares,\ntechnologies and general trends\nin the IT industry.\"")+xlab("Specialisation")+scale_y_continuous(labels=c("1","2",'3','4','5'))+ylab("Response")+
  theme(axis.text.x= element_text(size=4.5))
p9
ggsave(filename="bubble_plot_specialisation_sufficient_knowledge.png",plot=p9,width=10,units="cm")
#recruiters lack knowledge in most area especially data and software#bubbleplot
#most recruiters understood across specilaisations with less so in IT, UX
col_index <- 6
specialisation <- df[,11]
p11 <- ggplot(data=df,aes(x=specialisation,y=df[,col_index],col=df[,col_index]))+
  geom_count()+
  scale_y_discrete(labels=c("Inh","Out","Mixed"))+
  theme(legend.position = "none")+ggtitle("To what extent do you agree with this\n statement:\"My recruiter was transparent \nin their communication about the role and \nmade me excited for the job I was applying \nfor. \"")+xlab("Specialisation")+scale_y_continuous(labels=c("1","2",'3','4','5'))+ylab("Response")+
  theme(axis.text.x= element_text(size=4.5))
p11
ggsave(filename='bubble_plot_specialisation_transparency.png',plot=p11,width=10,units='cm')
#mostly transparent but lacking in certain specialisation incl product 
col_index <- 7
specialisation <- df[,11]
p12 <- ggplot(data=df,aes(x=specialisation,y=df[,col_index],col=df[,col_index]))+
  geom_count()+
  scale_y_discrete(labels=c("Inh","Out","Mixed"))+
  theme(legend.position = "none")+ggtitle("To what extent do you agree with this \nstatement:\"It's important for my \nrecruiter and the hiring team \nto be quick in their decision making.\"")+xlab("Specialisation")+scale_y_continuous(labels=c("1","2",'3','4','5'))+ylab("Response")+
  theme(axis.text.x= element_text(size=4.5))
p12
ggsave(filename="bubble_plot_specialisation_quick.png",plot=p12,width=10,units='cm')
#everyone values quickness but it missing in IT, UX
col_index <- 8
specialisation <- df[,11]
p13 <- ggplot(data=df,aes(x=specialisation,fill=df[,col_index]))+
  geom_bar(position='dodge')+
  ggtitle("On average, how many job offers \nare you considering at a a time when \ndeciding to change jobs in the IT industry?")+
  xlab("Specialisation")+
  theme(axis.text.x=element_text(size=4.3),
        legend.key.size=unit(0.4,'cm'),legend.title=element_text(size=10),legend.text=element_text(size=7))+
        #,
        #legend.text=element_text(size=7))+
  scale_fill_discrete(name = "Response")
  #scale_y_continuous(labels=c("1","2",'3','4','5'))+
  #ylab("Response")
p13
ggsave(filename="barchart_specialisation_n_offers.png",plot=p13,width=12,units='cm')
#data and analytics  is an especially competitive industry so knowledge is more important here and growth, for it sales 
df$satisfaction <- rowMeans(df[,4:7])
library(latex2exp)
kruskal.test(df$satisfaction~df$`What is your specialization within the IT industry?`, data = df)

e2 <- c("Mean Candidate satisfaction is not significantly different across industries","p==0.9013")
title <- paste("Mean candidate satisfaction is not \nsignificantly different across industries.\n(Kruskal-Wallis rank sum test)")
subtitle <- TeX(r"(($\textit{P}_{value}=0.9$ , $\textit{df}=4$ , $\chi^{2}=1.05$,$\textit{n}=160$))")
p14 <- ggplot(data=df,aes(x=specialisation,y=satisfaction,fill=specialisation))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()+
  ggtitle(label=title,subtitle=subtitle)+
  
  theme(axis.text.x=element_text(size=4.3),legend.position='none',plot.title=element_text(vjust=-2.0))+
  labs(x='Specialisation',y="Satisfaction")
  
p14
ggsave(filename="boxplot_specialisation_satisfaction.png",plot=p14,width=10,height=10,units='cm')
#most lowest satisfacition in IT  and UX
#suggests heavy recruitment competition
#satisfaction by type of recruiter
df[,3] <- as.factor(df[,3])
kruskal.test(df$satisfaction~df$`What kind of recruiters have you interacted with in the last 2 years?`, data = df)
title <- paste("Mean candidate satisfaction is\nsignificantly different across recruiter types\n(Kruskal-Wallis rank sum test)")
subtitle <- TeX(r"(($\textit{P}_{value}<0.05$ , $\textit{df}=2$ , $\chi^{2}=8.132$,$\textit{n}=160$))")

rec_house <- df[,3]
p15 <- ggplot(data=df,aes(x=rec_house,y=satisfaction,fill=rec_house))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()+
  scale_x_discrete(labels=c("In-house","Out-house","Mixed"))+
  theme(legend.position = 'none')+
  labs(x='Recruiter Type',y="Satisfaction")+
  ggtitle(label=title,subtitle=subtitle)
  
p15
ggsave(filename='boxplot_recruiter_type_satisfaction.png',plot=p15,width=10,units='cm')
#satisfaction is mostly same across different recruitment strategies (this info is valid to all of them, both ofo them could use this advice)

#now piechart of preferred comm channel
counts <- summary(as.factor(df[,9]))
platform <- levels(as.factor(df[,9]))
labels <- paste(round(counts/sum(counts)*100,2),"%")
df2 <- data.frame(counts,platform,labels)
p16 <- ggplot(data=df2,aes(x="",y=counts,fill=platform))+
  geom_col()+
  coord_polar(theta='y')+
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Platform"))+
  theme_void()+
  ggtitle("Preferred communicational channel \nfor new opportunities?")
  
p16
ggsave(filename="piechart_platform_new_oppurtunities.png",,width=10,units='cm')
#########
counts <- summary(as.factor(df[,10]))
platform <- levels(as.factor(df[,10]))
labels <- paste(round(counts/sum(counts)*100,2),"%")
df2 <- data.frame(counts,platform,labels)
p17 <- ggplot(data=df2,aes(x="",y=counts,fill=platform))+
  geom_col()+
  coord_polar(theta='y')+
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Platform"))+
  theme_void()+
  ggtitle("Preferred communicational channel \nfor progress reports?")

p17
p18 <- ggarrange(p16,p17,common.legend = TRUE,labels="AUTO",legend="right",vjust=7.5,hjust=-0.15)
p18
ggsave(filename = "piecharts_platforms.png",plot=p18,width=20,units='cm')
