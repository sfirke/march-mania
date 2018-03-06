-   [Making March Madness predictions - a how-to guide](#making-march-madness-predictions---a-how-to-guide)
-   [tl;dr](#tldr)
-   [Each script explained](#each-script-explained)
    -   [Deciding on input parameters](#deciding-on-input-parameters)
    -   [Scraping the input data](#scraping-the-input-data)
        -   [Ken Pomeroy's Ratings](#ken-pomeroys-ratings)
        -   [Preseason rankings](#preseason-rankings)
    -   [Tidying the raw data](#tidying-the-raw-data)
    -   [Training and evaluating models](#training-and-evaluating-models)
    -   [Making predictions to submit](#making-predictions-to-submit)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Making March Madness predictions - a how-to guide
=================================================

tl;dr
=====

Run the scripts in the `/scripts` directory, in numerical order. That, plus the data files in this repository from Kaggle (you can clone the repo, download them via the browser, or get them from Kaggle) will get you up and running with predictions. Below are expanations of the different scripts and the reasoning behind them.

Each script explained
=====================

Deciding on input parameters
----------------------------

There's a lot of released data on Kaggle that you could use. I kept my parameters simple. I included:

-   Ken Pomeroy's team rankings - a well-regarded rating system of team strength
-   Homefield advantage - teams do better at home, and while tournament games are all technically held on neutral ground, most regular season games (which make up the training set) have a host and a visitor
-   Preseason ranking - some article I read (NY Times?) convinced me that independent of team ranking at the time of the tournament (Pomeroy), a team's preseason ranking has some predictive value of tournament success. Also, at least in 2016 this data wasn't supplied by Kaggle nor was it in a flat file on the web. I hoped this unique data source would differentiate my model's prediction. Luck is a huge factor in this tournament, so some differentiation would set me up to get lucky.
    -   Unfortunately the preseason rankings have not made a big difference in my predictions.

There is a leakage issue with the Pomeroy ratings, in that the historical rankings are end-of-season ratings, and incorporate the results of tournament games that I'm using them to predict. I don't think this is a big deal and I do not address it.

Scraping the input data
-----------------------

*Scripts 01 and 02*

### Ken Pomeroy's Ratings

I pseudo-scraped Ken Pomeroy's men's basketball ratings data, going back to 2002. I mirrored his data in a Google Sheets document, then used the `googlesheets` package to extract and tidy it.

See my [short googlesheets accessing script](scripts/01_ken_pom_scraping.R), which grabs the pre-season rankings for each year and then combines them into a single tidy table, and extracts the Pomeroy ratings.

### Preseason rankings

I used the `rvest` package to scrape the preseason rankings from the College Poll Archive. Here are the [pre-season rankings for 2018](http://collegepollarchive.com/mbasketball/ap/app_preseason.cfm?sort=totapp&from=2018&to=2018), conveniently in a scrape-able HTML table - so let's [scrape them for each year](scripts/02_scrape_preseason_ranks.R).

Tidying the raw data
--------------------

*Script 03*

There are lots of data sources to unite and transform. This script takes the various inputs (Kaggle's data on 150k+ past game outcomes, the scraped data, a names crosswalk) and [transforms it into tidy data](scripts/03_tidy_raw_data.R) for modeling.

Training and evaluating models
------------------------------

*Script 04*

I know a lot about tidying dirty data. I don't know much about machine learning. Which means this is the part where I learn the most :) In part 4, the script [trains various models](scripts/04_train_evaluate_models.R) on the input data and tests the results. There we select a model with which we'll make our predictions.

You may wish to tweak this part, and I'd particularly welcome feedback and ideas (and pull requests?) here.

Making predictions to submit
----------------------------

*Script 05*

Pretty simple: once you have your model trained, you'll need to download the Kaggle form of games to predict. In Part 1, it's tourney games from 2014-2017; in the real contest, Part 2, it's every possible game this year. This script takes the Kaggle form and [makes predictions](scripts/05_make_predictions.R) in a ready-to-submit format. (You'll need to have run parts of script 03\_tidy\_raw\_data.R to make functions and a data.frame accessible).
