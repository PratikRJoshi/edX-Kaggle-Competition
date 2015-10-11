#Test your analytics skills by predicting which iPads listed on eBay will be sold

What makes an eBay listing successful?

Sellers on online auction websites need to understand the characteristics of a successful item listing to maximize their revenue.  Buyers might also be interested in understanding which listings are less attractive so as to score a good deal.  In this competition, we challenge you to develop an analytics model that will help buyers and sellers predict the sales success of a set of eBay listings for Apple iPads from spring 2015.

The following screenshot shows an example of iPad listings on eBay:
![Alt text](https://github.com/PratikRJoshi/edX-Kaggle-Competition/blob/master/eBay.png "Kaggle eBay competition details")

**File descriptions**

The data provided for this competition is split into two files:

- **eBayiPadTrain.csv** = the training data set. It consists of 1861 listings.
- **eBayiPadTest.csv** = the testing data set. It consists of 798 listings. 

We have also provided a sample submission file, SampleSubmission.csv. This file gives an example of the format of submission files (see the Evaluation page for more information). The data for this competition comes from eBay.com.

**Data fields**

The dependent variable in this problem is the variable sold, which labels if an iPad listed on the eBay site was sold (equal to 1 if it did, and 0 if it did not). The dependent variable is provided in the training data set, but not the testing dataset. This is an important difference from what you are used to - you will not be able to see how well your model does on the test set until you make a submission on Kaggle.

The independent variables consist of 9 pieces of product data available at the time the iPad listing is posted, and a unique identifier:

- **description** = The text description of the product provided by the seller.
- **biddable** = Whether this is an auction (biddable=1) or a sale with a fixed price (biddable=0).
- **startprice** = The start price (in US Dollars) for the auction (if biddable=1) or the sale price (if biddable=0).
- **condition** = The condition of the product (new, used, etc.)
- **cellular** = Whether the iPad has cellular connectivity (cellular=1) or not (cellular=0).
- **carrier** = The cellular carrier for which the iPad is equipped (if cellular=1); listed as "None" if cellular=0.
- **color** = The color of the iPad.
- **storage** = The iPad's storage capacity (in gigabytes).
- **productline** = The name of the product being sold.
