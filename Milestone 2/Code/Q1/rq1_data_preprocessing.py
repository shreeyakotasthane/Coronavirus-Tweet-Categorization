#!/usr/bin/env python
# coding: utf-8

# In[48]:


import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer


# In[49]:


analyser = SentimentIntensityAnalyzer()


# In[33]:


#loading in data file
#converting to a pandas dataframe
df = pd.DataFrame.from_csv("tweets_with_retweet_count.csv").reset_index()


# In[52]:


#The following cell will perform certain operations
#the operations are as follows

#get the word count of the tweet text, make a new column
df["word_count"] = df['text'].str.lower().str.split().apply(lambda x: len(x))

#get the amount of hashtags, make that its own column
df["hash_count"] = df['hashtags'].str.lower().str.split().apply(lambda x: len(x) if type(x) == list else 0)

#get the sentiment value for the tweet text, make that its own column
df["sentiment"] = df['text'].apply(lambda x: analyser.polarity_scores(x)['compound'])


# In[55]:


#convert to a dataframe

df.to_csv('cleaned_tweets.csv')


# In[ ]:




