import snscrape.modules.twitter as sntwitter
import pandas as pd
import time

# Load data used to scrape Tweets
get_data = pd.read_csv("data-raw/Twitter/get_tweets_keywords.csv")

# Create function to scrape
# 2023-02-01
def get_tweets_by_keyword(keyword, limit, since, until):

    # Creating list to append tweet data to
    attributes_container = []

    # Scrape
    for i, tweet in enumerate(
        sntwitter.TwitterSearchScraper(keyword + " since:" + since + " until:" + until).get_items()):
        print(str(i) + " " + keyword)

    # When limit is hit
        if i > limit:
            break
        attributes_container.append([tweet.user.username, tweet.date, tweet.coordinates, tweet.likeCount, tweet.sourceLabel, tweet.rawContent])

    # Creating a dataframe to load the list
    tweets_df = pd.DataFrame(attributes_container,
                         columns=["User", "Date Created", "Coordinates", "Number of Likes", "Source of Tweet", "Tweet"])

    # Print
    #print("row " + row_no)

    # Save to csv
    tweets_df.to_csv("data-raw/twitter/twitter_keyword_" + keyword + "_" + since + ".csv", index = False)

    # Pause before proceeding to next search
    time.sleep(2)

# Get tweets
get_data.apply(lambda x: get_tweets_by_keyword(x.keyword, x.limit, x.since, x.until), axis=1)