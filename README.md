# The effect of Twitter disinformation on radio media in the Central African Republic (CAR)
Thesis at the University of Copenhagen. Speciale på Københavns Universitet.

### To-do list:
* Scrape Twitter
* Scrape other media outlets (radios) in CAR
* Figure out how to use Botometer, Twitter (https://botometer.osome.iu.edu/)
* Find traditional polling data in CAR
* Other covariates?
* Add lag to analysis. Twitter's effect on radio would be lagged, right?
* Calculate sentiment
* Subset cosine score to topic-specific datasets
* Tokens:
	* Plot difference in token-use by groups. Grouped by dates around peak and not around peak. Different peaks
	* 

## Analysis

### Finding spikes in data
* Identify unnatural spikes in document frequency
* Create spike variable that distinguishes between documents close and far from these spikes
* Run analysis to see difference in topics by spike variable


### Covariates
* Text data that serves as control. Those that would not be affected, presumably.
* Text data that serves as an indicator of the general flow of events.
	* Reuters for example.
	* Agence France-Presse (AFP): https://twitter.com/afpfr.
	* #politique.
	* BBC Afrique (en français). @bbcafrique.
* Conflict data. 



## Hypotheses
* Radio Ndeke Luka's journalists read Twitter less after disinformation campagins began due to the notorious fake news.
* 

### CAR political topics:
* Crypto-currency to undermine French currency.
* Bozize's exclusion and later heading of rebel-group CPC.
* Wagner group ending rebel blockade. 2021? Got control of several towns. Names of towns?
* UN Report on mercenaries, March 2021.
* Religious differences. Wagner conflating all Muslims (e.g. Fulani). Touadera is Christian. 89 % is either protestant or catholic.
* Framing Wagner mercenaries as “instructors”.
* Ndassima gold mine.
* 2019 peace agreement.
* Calls for new dialogues. But why would Touadera want to invite CPC?
* Refugees.

### Actors:
* Touadera, president.
* Alexander Ivanov, Russian. Framed as advisor. Heads the Officers Union for International Security, which purports to be an independent “peace advocacy” group.
* Vladimir Titorenko. Was Russian Ambassador.
* CPC. Led by Bozize. Fulani group within.
* MINUSCA.
* France, Macron.
* Russia, Putin.
* Lobaye Invest SARLU, Russian-owned company.
* Wagner, Prigozhin.


## Data collection processes
### Radio data collection process
* Radio Ndeke Luka: https://www.radiondekeluka.org/actualites.html?start=0
* Radio Lengo Songo.
* Radio Reseau des journalistes.

| Radio  	    		 | Oldest data available | Newest data available | Collected  |
| -------------------------------| --------------------- | --------------------- | -----------|
| Radio Ndeke Luka  		 | 2017 		 | 2023			 |     Yes    |
| Radio Reseau des journalistes  | 2018  		 | 2023			 |     Yes    |
| Radio Lengo Songo		 | 2020  		 | 2023			 |     Yes    |

Data available: Articles able to be scraped from their radio website. Perhaps their social media pages could be scraped instead in case they don't have relevant websites with published articles.


### Twitter data collection process
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


## Data sources
* Twitter with `snscrape`.
* Radio content with web scraping in `R`.

### French sentiment dictionaries:
* FEEL French Expanded Emotion Lexicon: http://advanse.lirmm.fr/feel.php
* BERT: https://github.com/TheophileBlard/french-sentiment-analysis-with-bert
* proustr, tools for R: https://github.com/ColinFay/proustr
* 


## Other
### Examples of Tweets
* lerepublicain_c, 2018-05-27 10:44:56, Twitter Web Client: `Centrafrique : Touadéra-Poutine,un tandem pour la stabilité du pays https://t.co/FfJStqkuL2 via @wordpressdotcom`
* MChandrin, 2023-02-01 14:13:26, Twitter for Android: `@Gauthier_Pasq La patrie ou la mort nous vaincrons.  Vous finirez par comprendre. Vive POUTINE vive Assimi Goïta vive Ibrahim Traoré vive Touadera vive Doumbouya 🔥🔥🔥vive liran et vive la Chine`

