3.	Text Mining and Sentiment Analysis for
Maldives Hotel Reviews

3.1	Introduction

The social media plays an important role in today’s world and provides an efficient way for business to interact and communicate with their customers. Each application in social media has a unique way of providing accurate information about your business to potential customers. Star ratings are one of the review practises, that everyone easily understands.

However, hotel’s star rating does not express the exact experience of the customer. Customer reviews submitted at Internet portals are huge and unstructured, giving analytical challenges for existing analysis tools that were designed for well-structured, quantitative data. 

The purpose of this case study offers supervisory suggestions to hotel managers by turning words and reviews into quantitative measurements using natural language pre-processing, text mining and sentiment analysis techniques.


3.2	Aim and Objective of the task

The objective of this task is to analyse the English written 21093 online reviews of 104 hotels in Maldives separately. I have adapted methods from text mining and sentiment analysis of machine learning to illustrate the quality of reviews and sentiment assessment. I am going to extend this model with a supervised sentiment component that can classify a review as positive or negative with accuracy. 
I have developed this application as an interactive web application and deployed in shinyapps cloud with url https://ravikanth.shinyapps.io/maldiveshotelreview/, in which, an individual can select a hotel to get sentiment analysis report for the hotel. For interactive application development of sentiment analysis of hotel reviews I used Shiny R[4], which is used to develop interactive web-based dashboards using R without having to know HTML, CSS or JavaScript. The code consists of two functions called ui and server functions which can be called from another function as shinyApp (ui = ui, server = server)
 
For text mining and sentiment analysis of review data three different approaches have been used. 
1.	A Tidy Text Mining Approach[2]
2.	Corpus Object Approach
3.	Text mining using SAS Enterprise Miner[6]
The model development is going to be implemented on an interactive way so that we are going get the results of text mining and sentiment analysis for each hotel separately.

3.3	Brief Literature Review 

In hospitality industry, customers send a immense amount of textual information about various hotels on various social media sites. There is a growing concern in using automatic methods such as text mining and sentiment analysis to process large amounts of unstructured reviews data and extract meaningful information and perceptions. As an emerging technology, text mining and sentiment analysis aims to extract meaningful information from many textual documents. Text mining techniques have often been used to analyse large amounts of textual data to automatically extract knowledge, insights, useful patterns or trends.  
In this case study, I have analysed reviews of pleased and displeased customers by using a text-mining approach. The dataset comprises  21093 online reviews of 104 hotels. I have done the text mining and sentiment analysis for each hotel separately in an interactive way.


Keywords:
hotel reviews; sentiment analysis of reviews; text mining of reviews; 

