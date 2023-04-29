# The strategies of propaganda in media outlets by foreign actors in the Central African Republic (CAR)
Thesis at the University of Copenhagen. Speciale på Københavns Universitet.

### To-do list:
* Other covariates?
* Add lag to analysis. Twitter's effect on radio would be lagged, right?
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


## Data sources
* Radio content with web scraping in `R`.

### French sentiment dictionaries:
* FEEL French Expanded Emotion Lexicon: http://advanse.lirmm.fr/feel.php
* BERT: https://github.com/TheophileBlard/french-sentiment-analysis-with-bert
* proustr, tools for R: https://github.com/ColinFay/proustr
* 


## Other

## Method resources
Structural topic models:
* https://juliasilge.com/blog/evaluating-stm/
* https://ianadamsresearch.com/post/tidy-stm/tidying-stm-with-tidytext/
* https://cschwem2er.github.io/stminsights/reference/get_effects.html





