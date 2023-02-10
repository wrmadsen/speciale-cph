import snscrape.modules.twitter as sntwitter
import pandas as pd
import time

# Load data used to scrape Tweets
get_data = pd.read_csv("data-raw/Twitter/get_tweets_users.csv")

# Create function to scrape
# 2023-02-01
def get_tweets_by_user(user, limit, since, until):

    # Created a list to append all tweet attributes(data)
    attributes_container = []

    # Using TwitterSearchScraper to scrape data and append tweets to list
    for i, tweet in enumerate(sntwitter.TwitterSearchScraper("from:" + user + " since:" + since + " until:" + until).get_items()):
        if i > limit:
            break
        attributes_container.append([tweet.date, tweet.likeCount, tweet.sourceLabel, tweet.rawContent])

    # Creating a dataframe from the tweets list above
    tweets_df = pd.DataFrame(attributes_container,
                             columns=["Date Created", "Number of Likes", "Source of Tweet", "Tweets"])

    # Print
    #print("row " + row_no)

    # Save to csv
    tweets_df.to_csv("data-raw/twitter/users/twitter_user_" + user + "_" + since + ".csv", index = False)

    # Pause before proceeding to next search
    time.sleep(2)

# Get tweets
get_data.apply(lambda x: get_tweets_by_user(x.user, x.limit, x.since, x.until), axis=1)