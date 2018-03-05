-   [Making March Madness predictions - a how-to guide](#making-march-madness-predictions---a-how-to-guide)
-   [tl;dr](#tldr)
    -   [Deciding on input parameters](#deciding-on-input-parameters)
    -   [Scraping the input data](#scraping-the-input-data)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Making March Madness predictions - a how-to guide
=================================================

tl;dr
=====

Run the scripts in the `/scripts` directory, in numerical order. That, plus the data files in this repository from Kaggle (you can clone the repo, download them via the browser, or get them from Kaggle) will get you up and running with predictions. Below are expanations of the different scripts and the reasoning behind them.

Deciding on input parameters
----------------------------

There's a lot of released data on Kaggle that you could use. I kept my parameters simple. I included:

-   Ken Pomeroy's team rankings - a very well-regarded rating system of team strength
-   Homefield advantage - teams do better on the road, and while tournament games are all technically held on neutral ground, most regular season games (which make up the training set) have a host and a visitor
-   Preseason ranking - some article I read (NY Times?) convinced me that independent of team ranking at the time of the tournament (Pomeroy), a team's preseason ranking has some predictive value of tournament success. Also, at least in 2016 this data wasn't supplied by Kaggle nor was it in a flat file on the web. I hoped this unique data source would differentiate my model's prediction. Luck is a huge factor in this tournament, so some differentiation would set me up to get lucky.

There is a leakage issue with the Pomeroy ratings, in that the historical rankings are end-of-season ratings, and incorporate the results of tournament games that I'm using them to predict. I don't think this is a big deal and I didn't address it.

Scraping the input data
-----------------------

I used `rvest` package to scrape the preseason rankings from the College Poll Archive. Here are the [pre-season rankings for 2018](http://collegepollarchive.com/mbasketball/ap/app_preseason.cfm?sort=totapp&from=2018&to=2018), conveniently in a scrape-able HTML table - so let's scrape them for each year.

I pseudo-scraped Ken Pomeroy's men's basketball ratings data, going back to 2002. I mirrored his data in a Google Sheets document, then used the `googlesheets` package to extract and tidy it.

See my [short scraping script](scripts/01_scrape_preseason_ranks.R), which grabs the pre-season rankings for each year and then combines them into a single tidy table, and extracts the Pomeroy ratings.
