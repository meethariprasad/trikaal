Problem Statement
Welcome to Collectcent Click Prediction Challenge!

At Collececent, Technology is at the heart of what we do. We believe in providing the best experience and returns for our publishers & advertisers for their investments and partnerships with us.

We deliver more than 3 Billion clicks per month to our advertisers delivering 4.5 Million monthly sales events. We want to leverage machine learning to improve the conversions for our customers.

This is how an Advertisement network works in general:

The network buys space on the Publishers site and then shows an advertisement about the Advertiser at that space. The advertiser pays the network for every conversion from the clicks. The network in turns pays to the publisher after keeping it's commission.
Note: In real life, publisher and advertiser can also act as a media exchange

 
We want to open this challenge to Analytics Vidhya community to bring out the best models predicting conversions on our clicks. Needless to say - the person with the best model wins.

All the best!
Evaluation Metric
The evaluation metric for this competition is weighted RMSE
Data
The sample data is provided of 14 days, from 21st August to 3rd September. The train file contains sample data from first 10 days whereas you are asked to predict on sample data of last four days. Also, the public leaderboard would take into account the first day of test predictions, whereas private leaderboard will take into account rest three days of test predictions. Below is the data dictionary:

Variable
Description
ID
Unique ID of click
Country
Country Code
Carrier
Wireless Network Operator Code
TrafficType
Whether the advertisement is for Adults or Mainstream
ClickDate
Date at which the advertisement was clicked
Device
Type of Device from which advertisement was clicked
Browser
Type of Browser from which advertisement was clicked
OS
Type of OS from which advertisement was clicked
RefererUrl
Url of Source website
UserIp
IP of User who clicked
publisherId
Unique ID of publisher
subPublisherId
Unique ID of sub publisher
advertiserCampaignId
Unique ID of campaign of advertisement
Fraud
If the click was fraud or not
ConversionStatus
Was the click Converted or not
ConversionDate
Date of Conversion if Conversion happens
ConversionPayOut
Pay Out of Conversion if Conversion happens (in rupees)
