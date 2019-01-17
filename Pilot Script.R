#Pilot Email
#Terrence Pope
#8-2-2018
#This will be the first run of trying to send out a bunch of emails to employers using our script for emails.

setwd("~/Desktop/HP/Pilot")

suppressPackageStartupMessages(library(gmailr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
library(readr)

#Data
my_dat <- read_csv("Labor Market eDatabase Pilot.csv")
my_dat <- my_dat[-65:-72,] #these cases have no data about jobs

#Ebony Jackson
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/jackson-ebony-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Ebony Jackson'
email_sender <- 'Ebony Jackson <ebony.jackson481@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "ebony.jackson481@gmail.com") %>%
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


#Tanisha Washington
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/washington-tanisha-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Tanisha Washington'
email_sender <- 'Tanisha Washington <tanisha.washington116@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "tanisha.washington116@gmail.com") %>%
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

#Kenya King
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/king-kenya-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Kenya King'
email_sender <- 'Kenya King <kenya.king425@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "kenya.king425@gmail.com") %>%
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

#Tremayne Jackson
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/jackson-tremayne-lm.json")
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


#Tremayne Jackson
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/jackson-tremayne-lm.json")
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

#######
#######
#######

#Tyrone Washington
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/washington-tyrone-lm.json")
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
use_secret_file("~/Desktop/HP/Pilot/Google API/king-leroy-lm.json")
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

#Jill Adams
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/adams-jill-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Jill Adams'
email_sender <- 'Jill Adams <jill.adams855@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "jill.adams855@gmail.com") %>%
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


#Carrie Nelson
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/nelson-carrie-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Carrie Nelson'
email_sender <- 'Carrie Nelson <carrie.nelson343@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "carrie.nelson343@gmail.com") %>%
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



#Brad Adams
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/adams-brad-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Brad Adams'
email_sender <- 'Brad Adams <brad.adams315@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "brad.adams315@gmail.com") %>%
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



#Matthew Nelson
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/nelson-matthew-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Matthew Nelson'
email_sender <- 'Matthew Nelson <matthew.nelson289@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "matthew.nelson289@gmail.com") %>%
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


###################
#Laurie Baker
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/baker-laurie-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Laurie Baker'
email_sender <- 'Laurie Baker <laurie.baker746@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "laurie.baker746@gmail.com") %>%
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
######################


#Brett Baker
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/baker-brett-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Brett Baker'
email_sender <- 'Brett Baker <brett.baker573@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "brett.baker573@gmail.com") %>%
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


#Valentina Lopez
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/lopez-valentina-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Valentina Lopez'
email_sender <- 'Valentina Lopez <valentina.lopez8704@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "valentina.lopez8704@gmail.com") %>%
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

#send
saveRDS(sent_mail,
        paste(gsub("\\s+", "_", this_name), "sent-emails.rds", sep = "_"))

errors <- sent_mail %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail[errors]



#Salma Ramirez
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/ramirez-salma-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Salma Ramirez'
email_sender <- 'Salma Ramirez <salma.ramirez650@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "salma.ramirez650@gmail.com") %>%
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



#Luciana Martinez
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/martinez-luciana-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Luciana Martinez'
email_sender <- 'Luciana Martinez <luciana.martinez918@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "luciana.martinez918@gmail.com") %>%
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



#Juan Lopez
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/lopez-juan-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Juan Lopez'
email_sender <- 'Juan Lopez <juan.lopez4437@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "juan.lopez4437@gmail.com") %>%
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


#Jorge Ramirez
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/ramirez-jorge-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Jorge Ramirez'
email_sender <- 'Jorge Ramirez <jorge.ramirez3501@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "jorge.ramirez3501@gmail.com") %>%
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



#Diego Martinez
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/martinez-diego-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Diego Martinez'
email_sender <- 'Diego Martinez <diego.martinez3402@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "diego.martinez3402@gmail.com") %>%
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



#Mei Lee
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/lee-mei-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Mei Lee'
email_sender <- 'Mei Lee <mei.lee334@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "mei.lee334@gmail.com") %>%
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



#Li Na Wong
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/wong-lina-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Li Na Wong'
email_sender <- 'Li Na Wong <lina.wong463@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "lina.wong463@gmail.com") %>%
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



#Ling Yang
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/yang-ling-lm.json")
#force the authorization (seems to be necessary to change sender)
gmail_auth(scope = "full")

#Next we create a data frame where each variable is a key piece of the email, e.g. the "To" field or the body.
this_name <- 'Ling Yang'
email_sender <- 'Ling Yang <ling.yang966@gmail.com>'
optional_bcc <- '<sibl269@gmail.com>'
body <- "To whom it may concern,

My name is %s. In response to your listing, I am interested in applying for the %s position. Are you still hiring?

Sincerely,
%s
"
edat <- my_dat %>%
  filter(`User email` == "ling.yang966@gmail.com") %>%
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



#Tao Lee
#Google API
use_secret_file("~/Desktop/HP/Pilot/Google API/lee-tao-lm.json")
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
use_secret_file("~/Desktop/HP/Pilot/Google API/yang-chang-lm.json")
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
use_secret_file("~/Desktop/HP/Pilot/Google API/wang-zhiyuan-lm.json")
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

