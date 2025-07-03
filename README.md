# MLBTRAVEL

```MLBTRAVEL``` is an `R` package that calculates travel distances between MLB games


## Installation

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("aoddspotato/MLBTRAVEL")
```

## Usage

Using ```getMLBtravel``` or ```getMLBtraveldays```:

```getMLBtravel(example=FALSE)``` will do today's MLB games and calculate travel distances and times to yesterday's games.

```getMLBtravel(example=TRUE)``` will do travel between 06/29/2025 and 06/30/2025, which is a good example of a day with a lot of travel in the MLB

```getMLBtraveldays(day1=Sys.Date()-3, day2=Sys.Date()-2)``` will compare any two consecutive dates up to day2=today. In the example here it is calculating travel times from games 3 days ago to games 2 days ago. ```day1``` and ```day2``` variables MUST BE CONSECUTIVE DAYS!

