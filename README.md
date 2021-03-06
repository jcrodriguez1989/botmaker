
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The R `{botmaker}`

<!-- badges: start -->
<!-- badges: end -->

The `{botmaker}` helps with the configuration settings for deploying an
R-based bot that automatically runs on [GitHub
Actions](https://github.com/features/actions).

GitHub Actions allows us to freely run R scripts on the cloud. The
`{botmaker}` takes care of most of the configuration steps to get your R
script running periodically.

The interface of the `{botmaker}` is intended to be similar to the
`{usethis}` package. You open the R package in RStudio and call
`botmaker::make_bot`.

## What You Need?

You need to have an R Package project (as created with RStudio) hosted
in a GitHub repository.

Also, if you wish the `{botmaker}` to upload needed environment
variables as GitHub Secrets, then you will need to have your GitHub
personal access token (PAT) at hand -also known as `github_pat`-.

## Installation

You can install the development version of `{botmaker}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jcrodriguez1989/botmaker")
```

## Example

Suppose we are using the `{botmaker}` to set the
[@RStatsJobsBot](https://github.com/jcrodriguez1989/RStatsJobsBot)
**Twitter bot**, which runs every **15 minutes**. Open the RStatsJobsBot
R package in RStudio, and define the script that should be executed on
every bot run:

``` r
# This code is an example, it should be the code you want to run periodically.
bot_code <- '
library("RStatsJobsBot")
run_rstatsjobsbot(
  rtweet_app = Sys.getenv("RTWEET_APP"),
  rtweet_consumer_key = Sys.getenv("RTWEET_CONSUMER_KEY"),
  rtweet_consumer_secret = Sys.getenv("RTWEET_CONSUMER_SECRET"),
  rtweet_access_token = Sys.getenv("RTWEET_ACCESS_TOKEN"),
  rtweet_access_secret = Sys.getenv("RTWEET_ACCESS_SECRET"),
  from_time = Sys.time() - 60 * 60,
  max_hashtags = 15
)
'
```

If the bot needs to use environment variables (Twitter bots do), we need
to inform this to the `make_bot` function. These environment variables
should be present at the GitHub repository as
[Secrets](https://github.com/r-lib/actions/tree/master/examples#managing-secrets).
To inform about the environment variables to `make_bot`, we should
create a list with these variables:

``` r
env_vars <- list(
  RTWEET_APP = "",
  RTWEET_CONSUMER_KEY = "",
  RTWEET_CONSUMER_SECRET = "",
  RTWEET_ACCESS_TOKEN = "",
  RTWEET_ACCESS_SECRET = ""
)
```

If we want the `{botmaker}` to automatically push these Secrets to
GitHub, we can provide these values to `make_bot`:

``` r
env_vars <- list(
  RTWEET_APP = "RStatsJobsBot_twitter_app",
  RTWEET_CONSUMER_KEY = "myconsumerkey",
  RTWEET_CONSUMER_SECRET = "myconsumersecret",
  RTWEET_ACCESS_TOKEN = "myaccesstoken",
  RTWEET_ACCESS_SECRET = "myaccesssecret"
)
```

And let the `{botmaker}` create the configuration files by executing:

``` r
botmaker::make_bot(
  bot_code,
  cron_schedule = botmaker::cron_run_every(minutes = 15),
  env_vars = env_vars,
  repo = "jcrodriguez1989/RStatsJobsBot"
)
```

And finally, push the generated files to the GitHub repository.
