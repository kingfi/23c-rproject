# Data Analysis Project for HES 23C

## Installation
1. `brew install pre-commit` if `pre-commit` is not already installed on your machine.
2. Run everything in the `requirements.r` file.
3. Hack away on the `main.rmd` notebook!

## Data
Please look through the `/docs` directory for information regarding the source and collection methods for the original data set.

The original data set was formatted in such a way that made per-party data exploration cumbersome. Please see the script `/data/data.R` for information on how the original table was converted to a dataframe that better fit our needs (found in `responses.RData`).

## Structure
All modules are saved in the `/R` folder and are imported in `main.Rmd` using the `lib` keyword. They can be accessed like so:
```r
usa_mean_responses <- lib$by_country$mean_responses("USA")
```
