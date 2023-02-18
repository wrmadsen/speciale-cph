# The effect of Twitter disinformation on radio media in the Central African Republic (CAR)
Thesis at the University of Copenhagen. Speciale på Københavns Universitet.

## To-do list:
* Scrape Twitter
* Scrape other media outlets (radios) in CAR
* Figure out how to use Botometer, Twitter (https://botometer.osome.iu.edu/)
* Find traditional polling data in CAR
* Other covariates?
# Add lag to analysis. Twitter's effect on radio would be lagged, right?

## Hypotheses
* Radio Ndeke Luka's journalists read Twitter less after disinformation campagins began due to the notorious fake news.
* 


## Radio data collection process
* Radio Ndeke Luka: https://www.radiondekeluka.org/actualites.html?start=0
* 


## Twitter data collection process
1. Collect Tweets by keywords
2. Identify Tweets that are relevant to CAR and Russian disinformation
	* Remove Tweets regarding "Poutine" the food
	* Remove non-French tweets? `keyword lang:fr`
	* Add Tweets whose coordinates are located in the CAR or nearby
	* Remove Tweets that refer to Russia/Putin but in a different non-CAR context?
3. Use Tweets from step (2) to create a list of relevant users
	* Use Botometer to identify bots
4. Collect Tweets from relevant users in list of step (3)

End dataset is thus:
* Dataset of Tweets that represent a Russian effort to disinform on Twitter:
	* Bots may represent Russian bots.
	* Pro-Russian/Wagner Tweets may not be Russian nor orchestrated. But they may regurgitate whatever such Russian bots had attempted to set in motion. The original content of Russian bots may be more difficult to find due to being removed by Twitter.



## Analysis


## Data sources
* Twitter with `snscrape`.
* Radio content with web scraping in `R`.


## Other
### Examples of Tweets
* lerepublicain_c, 2018-05-27 10:44:56, Twitter Web Client: `Centrafrique : Touadéra-Poutine,un tandem pour la stabilité du pays https://t.co/FfJStqkuL2 via @wordpressdotcom`
* MChandrin, 2023-02-01 14:13:26, Twitter for Android: `@Gauthier_Pasq La patrie ou la mort nous vaincrons.  Vous finirez par comprendre. Vive POUTINE vive Assimi Goïta vive Ibrahim Traoré vive Touadera vive Doumbouya 🔥🔥🔥vive liran et vive la Chine`

