#' Configure the Bot
#'
#' Configure the settings to get the bot running on Github Actions.
#'
#' @param r_code A character with the R code to be executed by the bot.
#' @param cron_schedule A character with cron-style scheduling. Github actions has a minimum
#'   time period for scheduled runs to 5 minutes. For details about cron, please check
#'   "https://crontab.guru/".
#' @param env_vars A named list with the name and the value of each environment variable to use.
#'   E.g., `list(TWITTER_KEY = "klj3l21kj3l21k", TWITTER_SECRET = "4lh324hl34l23j4hl23j4l23j")`.
#'   If you don't want `make_bot` to push your env vars to Github, then provide a named list with
#'   no values, e.g., `list(TWITTER_KEY = "", TWITTER_SECRET = "")`.
#' @param bot_timeout A numeric with the maximum number of minutes to let the bot script run before
#'   GitHub automatically cancels it.
#' @param repo A character with the address of the Github repository in the format "username/repo".
#'   This field is not mandatory, it will be used to try to push `env_vars` to the repository.
#'   I.e., If the repository is "https://github.com/jcrodriguez1989/botmaker", then repo is
#'   "jcrodriguez1989/botmaker".
#' @param github_pat A character with your Github personal access token (PAT). This field is not
#'   mandatory, it will be used to try to push `env_vars` to the repository. To get your PAT, visit
#'   "https://github.com/settings/tokens".
#'
#' @importFrom glue glue
#' @importFrom usethis use_build_ignore
#'
#' @export
#'
make_bot <- function(r_code, cron_schedule = cron_run_every(minutes = 15), env_vars = list(),
                     bot_timeout = 15, repo = NULL, github_pat = Sys.getenv("GITHUB_PAT")) {
  # Get and read the yaml template file.
  template_path <- system.file("templates", "gh-actions-bot-config.yaml", package = "botmaker")
  template <- readLines(template_path, encoding = "UTF-8", warn = FALSE)
  # Try to push the env vars to Github.
  set_gh_secrets(env_vars, repo, github_pat)
  # Give YAML format.
  r_code <- format_r_code(r_code, "          ")
  env_vars <- format_env_vars(env_vars, "          ")
  bot_config <- glue(paste0(template, collapse = "\n"), .open = "{{{", .close = "}}}")
  # Create GH actions folder structure, and copy the bot config.
  dir.create(".github/workflows/", showWarnings = FALSE, recursive = TRUE)
  use_build_ignore(".github/")
  writeLines(bot_config, ".github/workflows/bot.yaml")
}

#' Write Secrets to Github
#'
#' Tries to write secrets to a Github repository.
#'
#' @param secrets_list A named list with the name and the value of each environment variable to
#'   push to Github.
#' @param repo A character with the address of the Github repository in the format "username/repo".
#' @param github_pat A character with your Github personal access token (PAT).
#'
#' @importFrom base64enc base64decode base64encode
#' @importFrom glue glue
#' @importFrom httr authenticate content GET PUT
#' @importFrom jsonlite toJSON
#' @importFrom sodium simple_encrypt
#'
set_gh_secrets <- function(secrets_list, repo, github_pat) {
  # If there are no secrets (or don't have values), then skip.
  if (length(secrets_list) == 0 || sum(nchar(unlist(secrets_list))) == 0) {
    return()
  }
  api_url <- "https://api.github.com"
  public_key <- content(GET(
    glue("{api_url}/repos/{repo}/actions/secrets/public-key"),
    authenticate(github_pat, "")
  ))
  if (!all(c("key_id", "key") %in% names(public_key))) {
    warning(
      "Could not push secrets to Github, please do it manually as specified at ",
      '"https://github.com/r-lib/actions/tree/master/examples#managing-secrets".',
      call. = FALSE
    )
    return()
  }
  # Decode public key.
  pub_key_dec <- base64decode(public_key$key)
  # Start pushing each secret.
  invisible(lapply(names(secrets_list), function(secret_name) {
    secret <- secrets_list[[secret_name]]
    secret_raw <- charToRaw(secret)
    # Encrypt the secret using the public key.
    secret_raw_encr <- simple_encrypt(secret_raw, pub_key_dec)
    # base64 encode the secret.
    secret_raw_encr_enc <- base64encode(secret_raw_encr)
    # Push the encrypted secret to Github.
    PUT(
      glue("{api_url}/repos/{repo}/actions/secrets/{secret_name}"),
      authenticate(github_pat, ""),
      body = toJSON(list(
        key_id = public_key$key_id,
        encrypted_value = secret_raw_encr_enc
      ), auto_unbox = TRUE)
    )
  }))
}

#' Format the R Code for Github Action YAML File
#'
#' @param r_code A character with the R code to be executed by the bot.
#' @param pre_line A character with the string that should be prepended to each line.
#'
format_r_code <- function(r_code, pre_line) {
  if (sum(nchar(r_code)) == 0) {
    return("")
  }
  # If the R code is a vector, then collapse it.
  r_code <- paste0(r_code, collapse = "\n")
  # Prepend `pre_line` to each line of code.
  r_code <- glue("{pre_line}{strsplit(r_code, '\n')[[1]]}")
  # Prepend the header.
  paste0(c("\n        run: |", r_code), collapse = "\n")
}

#' Format the Environment Variables List for Github Action YAML File
#'
#' @param env_vars A named list with the name and the value of each environment variable to use.
#' @param pre_line A character with the string that should be prepended to each line.
#'
format_env_vars <- function(env_vars, pre_line) {
  if (length(env_vars) == 0) {
    return("")
  }
  # Prepend `pre_line` to each line, and format as GH Actions lines.
  env_vars <- glue(
    "{{{pre_line}}}{{{names(env_vars)}}}: ${{ secrets.{{{names(env_vars)}}} }}",
    .open = "{{{",
    .close = "}}}"
  )
  # Prepend the header.
  paste0(c("\n        env:", env_vars), collapse = "\n")
}
