#Wave 1 Email
#Terrence Pope
#8-2-2018
#This will be the first run of trying to send out a bunch of emails to employers using our script for emails.

setwd("~/Desktop/HP/Wave 1")

suppressPackageStartupMessages(library(gmailr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
library(readr)

#Data
my_dat <- read_csv("Labor Market eDatabase Wave 1.csv")
#my_dat <- my_dat[-65:-72,] #these cases have no data about jobs


#Tremayne Jackson
#Google API
use_secret_file("~/Desktop/HP/Wave 1/Google API/jackson-tremayne-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Tremayne Jackson'
email_sender <- 'Tremayne Jackson <tremayne.jackson999@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "tremayne.jackson999@gmail.com") %>%
  mutate(
    To = sprintf('<%s>', `Contact Email`),
    Bcc = optional_bcc,
    From = email_sender,
    Subject = sprintf(`Job Title (Cleaned)`),
    body = sprintf(body, this_name, `Job Title (Cleaned)`, this_name)) %>%
  select(To, Bcc, From, Subject, body)
write_csv(edat, paste(gsub("\\s+", "_", this_name), "composed-emails.csv", sep = "_"))

emails <- edat %>%
  pmap(mime)
str(emails, max.level = 2, list.len = 2)

safe_send_message <- safely(send_message)
sent_mail <- emails %>% 
  map(safe_send_message)

saveRDS(sent_mail,
        paste(gsub("\\s+", "_", this_name), "sent-emails.rds", sep = "_"))

errors <- sent_mail %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail[errors]


#Tyrone Washington
#Google API
use_secret_file("~/Desktop/HP/Wave 1/Google API/washington-tyrone-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Tyrone Washington'
email_sender <- 'Tyrone Washington <tyrone.washington230@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "tyrone.washington230@gmail.com") %>%
  mutate(
    To = sprintf('<%s>', `Contact Email`),
    Bcc = optional_bcc,
    From = email_sender,
    Subject = sprintf(`Job Title (Cleaned)`),
    body = sprintf(body, this_name, `Job Title (Cleaned)`, this_name)) %>%
  select(To, Bcc, From, Subject, body)
write_csv(edat, paste(gsub("\\s+", "_", this_name), "composed-emails.csv", sep = "_"))

emails <- edat %>%
  pmap(mime)
str(emails, max.level = 2, list.len = 2)

safe_send_message <- safely(send_message)
sent_mail <- emails %>% 
  map(safe_send_message)

saveRDS(sent_mail,
        paste(gsub("\\s+", "_", this_name), "sent-emails.rds", sep = "_"))

errors <- sent_mail %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail[errors]


#Leroy King
#Google API
use_secret_file("~/Desktop/HP/Wave 1/Google API/king-leroy-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Leroy King'
email_sender <- 'Leroy King <leroy.king195@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "leroy.king195@gmail.com") %>%
  mutate(
    To = sprintf('<%s>', `Contact Email`),
    Bcc = optional_bcc,
    From = email_sender,
    Subject = sprintf(`Job Title (Cleaned)`),
    body = sprintf(body, this_name, `Job Title (Cleaned)`, this_name)) %>%
  select(To, Bcc, From, Subject, body)
write_csv(edat, paste(gsub("\\s+", "_", this_name), "composed-emails.csv", sep = "_"))

emails <- edat %>%
  pmap(mime)
str(emails, max.level = 2, list.len = 2)

safe_send_message <- safely(send_message)
sent_mail <- emails %>% 
  map(safe_send_message)

saveRDS(sent_mail,
        paste(gsub("\\s+", "_", this_name), "sent-emails.rds", sep = "_"))

errors <- sent_mail %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail[errors]

#####
#####

#Tao Lee
#Google API
use_secret_file("~/Desktop/HP/Wave 1/Google API/lee-tao-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Tao Lee'
email_sender <- 'Tao Lee <tao.lee735@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "tao.lee735@gmail.com") %>%
  mutate(
    To = sprintf('<%s>', `Contact Email`),
    Bcc = optional_bcc,
    From = email_sender,
    Subject = sprintf(`Job Title (Cleaned)`),
    body = sprintf(body, this_name, `Job Title (Cleaned)`, this_name)) %>%
  select(To, Bcc, From, Subject, body)
write_csv(edat, paste(gsub("\\s+", "_", this_name), "composed-emails.csv", sep = "_"))

emails <- edat %>%
  pmap(mime)
str(emails, max.level = 2, list.len = 2)

safe_send_message <- safely(send_message)
sent_mail <- emails %>% 
  map(safe_send_message)

saveRDS(sent_mail,
        paste(gsub("\\s+", "_", this_name), "sent-emails.rds", sep = "_"))

errors <- sent_mail %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail[errors]



#Chang Yang
#Google API
use_secret_file("~/Desktop/HP/Wave 1/Google API/yang-chang-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Chang Yang'
email_sender <- 'Chang Yang <chang.yang675@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "chang.yang675@gmail.com") %>%
  mutate(
    To = sprintf('<%s>', `Contact Email`),
    Bcc = optional_bcc,
    From = email_sender,
    Subject = sprintf(`Job Title (Cleaned)`),
    body = sprintf(body, this_name, `Job Title (Cleaned)`, this_name)) %>%
  select(To, Bcc, From, Subject, body)
write_csv(edat, paste(gsub("\\s+", "_", this_name), "composed-emails.csv", sep = "_"))

emails <- edat %>%
  pmap(mime)
str(emails, max.level = 2, list.len = 2)

safe_send_message <- safely(send_message)
sent_mail <- emails %>% 
  map(safe_send_message)

saveRDS(sent_mail,
        paste(gsub("\\s+", "_", this_name), "sent-emails.rds", sep = "_"))

errors <- sent_mail %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail[errors]



#Zhiyuan Wang
#Google API
use_secret_file("~/Desktop/HP/Wave 1/Google API/wang-zhiyuan-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Zhiyuan Wang'
email_sender <- 'Zhiyuan Wang <zhiyuan.wang631@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "zhiyuan.wang631@gmail.com") %>%
  mutate(
    To = sprintf('<%s>', `Contact Email`),
    Bcc = optional_bcc,
    From = email_sender,
    Subject = sprintf(`Job Title (Cleaned)`),
    body = sprintf(body, this_name, `Job Title (Cleaned)`, this_name)) %>%
  select(To, Bcc, From, Subject, body)
write_csv(edat, paste(gsub("\\s+", "_", this_name), "composed-emails.csv", sep = "_"))

emails <- edat %>%
  pmap(mime)
str(emails, max.level = 2, list.len = 2)

safe_send_message <- safely(send_message)
sent_mail <- emails %>% 
  map(safe_send_message)

saveRDS(sent_mail,
        paste(gsub("\\s+", "_", this_name), "sent-emails.rds", sep = "_"))

errors <- sent_mail %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail[errors]

