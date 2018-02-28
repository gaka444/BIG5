library(ROAuth)
library(twitteR)
library(httr)
library(RCurl)



consumer_key <- "dTTsWSsNf5KkcV9lT0YFO6FY2"
consumer_secret <-"MZm8gpChc6Rv8IM8r8lQgajYQFuGqdIB4C1jjfQDv8Rr0tYt8M" 
access_token <- "3108469604-bCGx1afzdc42mpQErl5XeAe0eWjuFsAm6AX3q1g"
access_secret <- "j0n38ltEu6IxAZV62kQu5S25lRImvyoHwFO3eonj44ei4"

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem") #downloads the certificate

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

cred <- OAuthFactory$new(consumerKey=consumer_key, 
                         consumerSecret=consumer_secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")