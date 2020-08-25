from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream
import re
import dataset
from sqlalchemy.exc import ProgrammingError
import datetime, time
import sys

# Enter Twitter API Keys
access_token = ""
access_token_secret = ""
consumer_key = ""
consumer_secret = ""

# Create tracklist with the words that will be searched for
tracklist = ['#coronavirus', '#coronavirus', '#CoronaVirus']
# Initialize Global variable
tweet_count = 0
# Input number of tweets to be downloaded
n_tweets = 500000000
db = dataset.connect("sqlite:///corona_tweets_7.db")


# Create the class that will handle the tweet stream
class StdOutListener(StreamListener):
      
    def on_status(self, data):
        global tweet_count
        global n_tweets
        global stream

        if tweet_count < n_tweets:
            if data.retweeted:
                return
            if data.lang == 'en' and 'RT'.upper() not in data.text:
                try:
                    text = data.extended_tweet["full_text"]
                except AttributeError:
                    text = data.text
                    
                created_at = data.created_at
                tweet_id = data.id_str 
                source = data.source
                user_name = data.user.name 
                screen_name = data.user.screen_name
                user_location = data.user.location 
                user_verified = data.user.verified 
                user_followers_count = data.user.followers_count
                is_quoted_tweet = data.is_quote_status
                quote_count = data.quote_count
                reply_count = data.reply_count 
                retweet_count = data.retweet_count
                favorite_count = data.favorite_count
                url = "https://twitter.com/" + screen_name + "/status/" + tweet_id
                hashtags = ""
                if len(data.entities["hashtags"]) > 0:
                    for tags in data.entities["hashtags"]:
                        tag = tags["text"]
                        hashtags += "#" + tag + " "
                    hashtags = hashtags.rstrip()
        
                user_mentions = " "

                if len(data.entities["user_mentions"]) > 0:
                    for mentions in data.entities["user_mentions"]:
                        mention = mentions["screen_name"]
                        user_mentions += "@" + mention + " "
                    user_mentions = user_mentions.rstrip()


                table = db["tweet_collections"]

                try:
                    table.insert(dict(
                        text = text,
                        created_at = created_at,
                        tweet_id = tweet_id, 
                        url = url,
                        source = source,
                        user_name = user_name, 
                        screen_name = screen_name,
                        user_location = user_location, 
                        user_verified = user_verified, 
                        user_followers_count = user_followers_count,
                        is_quoted_tweet = is_quoted_tweet,
                        quote_count = quote_count,
                        reply_count = reply_count,
                        retweet_count = retweet_count,
                        favorite_count = favorite_count,
                        hashtags = hashtags,
                        user_mentions = user_mentions

                        ))
                    tweet_count += 1
                    if tweet_count % 100 == 0:
                        print("Downloaded " + str(tweet_count) + " tweets")

                except ProgrammingError as err:
                    print(err)

                return True
        else:
            stream.disconnect()

    def on_error(self, status):
        if status == 420:
            cdate = "Error code 420 at:"+str(datetime.datetime.now())
            print(cdate)
            print("Sleeping for 15 mins")
            time.sleep(900)
        return False



# Handles Twitter authetification and the connection to Twitter Streaming API
l = StdOutListener()
auth = OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

def start_stream():
    while True:
        try:
            stream = Stream(auth, l, tweet_mode='extended')
            stream.filter(track=tracklist)

        except:
            e = sys.exc_info()[0]
            print("Error occured: " + str(e))
            continue
start_stream()
