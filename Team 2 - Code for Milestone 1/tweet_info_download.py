import tweepy
import pandas as pd

access_token = ""
access_token_secret = ""
consumer_key = ""
consumer_secret = ""

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)

data = pd.read_csv("corona_tweets.csv", encoding='utf-8')
all_urls = data.url.tolist()
tweets_info = []
count = 0

for url in all_urls:
	new = {}
	tweet_id = url.split("/")[-1]
	try:
		tweet = api.get_status(tweet_id, tweet_mode='extended', wait_on_rate_limit=True, wait_on_rate_limit_notify=True)
		new['tweet_id'] = tweet_id
		new['full_text'] = tweet.full_text
		new['favorite_count'] = tweet.favorite_count
		new['retweet_count'] = tweet.retweet_count
		tweets_info.append(new)
		if (len(tweets_info) % 1000 == 0):
			count += 1
			pd.DataFrame(tweets_info).to_csv("tweet_favor_retweet_count_" + str(count) + ".csv", encoding='utf-8')
			print(str(count*1000) + " tweets saved")
			tweets_info = []
	except tweepy.TweepError as er:
		print("Error for url: " + url)
		continue



# tweet = api.get_status("1225576639330910209", tweet_mode='extended')
# print(tweet)
