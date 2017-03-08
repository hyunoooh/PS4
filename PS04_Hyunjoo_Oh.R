# ==========================================
# Problem Set 4 | Statistical Programming 
# << Scraping Wikipedia >>
# Author: Hyunjoo Oh 
# ==========================================

# Get ready for scraping.
install.packages("rvest") # installing package
install.packages("xml2") # installing package: rvest requires "xm12"
library(xm12)
library(rvest) # loading library

rm(list=ls()) # cleaning the envirment
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

# Let's start to scrape the information from this table.
# Locate the first table.

allTables <- wikiURL %>%
  read_html %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>% # copying and pasting the xpath from the webpage
  html_table()

elections <- allTables[[1]] # making a dataframe
elections <- elections[-(1:2),] # deleting the first two rows

# Put names on each variable (#of variables = 13)
names(elections) <- c("order"
                      , "year"
                      , "winner"
                      , "pr.party"
                      , "cvote.share"
                      , "cvote.percent"
                      , "pvote.percent"
                      , "pvote.margin.percent"
                      , "pvote"
                      , "pvote.margin"
                      , "runnerup"
                      , "runnerup.party"
                      , "turnout")

# let's make the variables neat in a way to
# (1) remove footnote, [a]
# (2) remove '%'
# (3) remove unnecessary ','
# (4) change '−' to '-'
# (5) change the (numeric) values as numeric

# removing '%', footnotes '[a]'; chaning '−' as '-'
elections <- as.data.frame(sapply(elections, gsub, pattern="%", replacement=""))
elections <- as.data.frame(sapply(elections, gsub, pattern="\\[a\\]", replacement=""))
elections <- as.data.frame(sapply(elections, gsub, pattern='−', replacement='-'))

# removing unnecessary ','
elections$winner <- gsub(",", ", ", elections$winner)
elections$runnerup <- gsub(",", ", ", elections$runnerup)
elections$pvote <- gsub(",", "", elections$pvote)
elections$pvote.margin <- gsub(",", "", elections$pvote.margin)

# changing values as numeric
elections$order <- as.numeric(as.character(elections$order))
elections$year <- as.numeric(as.character(elections$year))
## elections$cvote.share <- as.numeric(as.character(elections$cvote.share))
elections$cvote.percent <- as.numeric(as.character(elections$cvote.percent))
elections$pvote.percent <- as.numeric(as.character(elections$pvote.percent))
elections$pvote.margin.percent <- as.numeric(as.character(elections$pvote.margin.percent))
elections$pvote <- as.numeric(as.character(elections$pvote))
elections$pvote.margin <- as.numeric(as.character(elections$pvote.margin))
elections$turnout <- as.numeric(as.character(elections$turnout))

# save all plots to a single pdf:
setwd("/Users/hyunjoooh/Desktop/2017_Stat_Prog/PS4")
pdf(file = 'Turnout_and_voteshare')

# Visualize: 
plot(elections$year, elections$turnout, 
     type = "p",
     pch = "t",
     main = "Turnout and votes of the winners in Pr. elections, 1824-2016",
     xlab = "Election year",
     ylab = "Turnout or Voteshare(%)")
points(elections$year, elections$cvote.percent, 
       pch = 'c', col = 'green')
points(elections$year, elections$pvote.percent,
       pch = 'p', col = 'brown')
legend("bottomright", c("Turnout", "Electoral college vote", "popular vote"), 
       pch = c("t", "c", "p"),
       col = c("black", "green", "brown"),
       cex = 0.7)

### The voteshare of winner in both popular vote and electoral college vote has been decreased.
plot.new()
with(elections[elections$pr.party == "Rep.",]
     , plot(year, turnout,
            main = 'Turnout and the winning party, 1824-2016',
            xlab = 'Election year',
            ylab = 'turnout',
            pch = 'R',
            col = 'red'))
abline(lm(elections$turnout ~ elections$year), 
       col = 'gray', 
       lty = 3,
       lwd = 2
       )
with(elections[elections$pr.party == "Rep.",]
     , abline(lm(turnout ~ year),
            col = 'red'))
with(elections[elections$pr.party == "Dem.",]
     , points(year, turnout,
            pch = 'D',
            col = 'blue'))
with(elections[elections$pr.party == "Dem.",]
     , abline(lm(turnout ~ year),
              col = 'blue'))
legend("topright", c("General turnout", "Rep. winner", "Dem. winner"),
       pch = c("-", "-", "-"),
       col = c('gray', 'red', 'blue'))


# Let's plot the electoral college / popular voteshares(%) data together.
plot.new()
par(mfrow=c(3,1))
with(elections[elections$pr.party == "Rep.",]
     , plot(year, turnout,
            main = 'Turnout and the winning party, 1824-2016',
            xlab = 'Election year',
            ylab = 'turnout',
            pch = 'R',
            col = 'red'))
abline(lm(elections$turnout ~ elections$year), 
       col = 'gray', 
       lty = 3,
       lwd = 2
)
with(elections[elections$pr.party == "Rep.",]
     , abline(lm(turnout ~ year),
              col = 'red'))
with(elections[elections$pr.party == "Dem.",]
     , points(year, turnout,
              pch = 'D',
              col = 'blue'))
with(elections[elections$pr.party == "Dem.",]
     , abline(lm(turnout ~ year),
              col = 'blue'))

# second plot: voteshare in electoral college vote
with(elections[elections$pr.party == "Rep.",]
     , plot(year, cvote.percent,
            main = 'Electoral college voteshare and the winning party, 1824-2016',
            xlab = 'Election year',
            ylab = 'Voteshare (%)',
            pch = 'R',
            col = 'red'))
abline(lm(elections$cvote.percent ~ elections$year), 
       col = 'gray', 
       lty = 3,
       lwd = 2
)
with(elections[elections$pr.party == "Rep.",]
     , abline(lm(cvote.percent ~ year),
              col = 'red'))
with(elections[elections$pr.party == "Dem.",]
     , points(year, cvote.percent,
              pch = 'D',
              col = 'blue'))
with(elections[elections$pr.party == "Dem.",]
     , abline(lm(cvote.percent ~ year),
              col = 'blue'))

# third plot: voteshare in popular vote
with(elections[elections$pr.party == "Rep.",]
     , plot(year, pvote.percent,
            main = 'Popular voteshare and the winning party, 1824-2016',
            xlab = 'Election year',
            ylab = 'Voteshare (%)',
            pch = 'R',
            col = 'red'))
abline(lm(elections$pvote.percent ~ elections$year), 
       col = 'gray', 
       lty = 3,
       lwd = 2
)
with(elections[elections$pr.party == "Rep.",]
     , abline(lm(pvote.percent ~ year),
              col = 'red'))
with(elections[elections$pr.party == "Dem.",]
     , points(year, pvote.percent,
              pch = 'D',
              col = 'blue'))
with(elections[elections$pr.party == "Dem.",]
     , abline(lm(pvote.percent ~ year),
              col = 'blue'))
dev.off()

### Comments:
### The general turnout has been decreasing.
### Relatively, Republican winners has won the elections more often when turnout was higher,
### while Democratic winners has won the elections more often when turnout was lower.
### But this trend is has no statistical meaning, 
### because this is a simple trend without controlling other variables.
### The voteshare of winner in electoral college or popular votes does not show any trends
### related to the winning party.



# ==========
# Read the second URL
webData <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'
install.packages("htmltab") # using package, 'htmltab'
library(htmltab)
electoral.college <- htmltab(webData, which = 3)

# change the variable name corresponding to "elections" data.
names(electoral.college)[1] <- "year"
names(electoral.college)[7] <- "electoral.votes"
names(electoral.college)[3] <- "candidate"

# Create variables and set them as numeric:
# votes.gained: number of electoral votes gained by winner
electoral.college$votes.gained <- gsub(" .*", "", electoral.college$electoral.votes) 
electoral.college$votes.gained <- as.numeric(as.character(electoral.college$votes.gained))
# votes.total: total number of electoral votes
electoral.college$votes.total <- gsub(".* ", "", electoral.college$electoral.votes)
electoral.college$votes.total <- as.numeric(as.character(electoral.college$votes.total))

# Make another dataframe which includes the four variables only
# : year, candidtate, votes.gained, votes.total
election.data <- NULL
election.data$year <- electoral.college$year
election.data$candidate <- electoral.college$candidate
election.data$votes.gained <- electoral.college$votes.gained
election.data$votes.total <- electoral.college$votes.total

# recode the character values in a same form for matching
election.data$candidate[168] <- "Adlai Stevenson"
election.data$candidate[170] <- "Adlai Stevenson"
election.data$candidate[141] <- "William Taft"
election.data$candidate[145] <- "William Taft"

election.data <- as.data.frame(election.data)

# change year as numeric
election.data$year <- as.numeric(as.character(election.data$year))
# drop the cases, year is earlier than 1824
# let's use the fact that the variables are arranged by year
election.data <- election.data[which(election.data$year == 1824)[1]:nrow(election.data),]

# recode the character values in a same form for matching
elections$winner <- gsub(".*,", "", elections$winner) # removing last name and comma in front of fullname
elections$winner <- gsub("\n.*", "", elections$winner) # removing middle name
elections$winner <- gsub("[A-Z]. ", "", elections$winner) # removing middle name

elections$runnerup <- gsub(".*,", "", elections$runnerup) # same process for runner-ups
elections$runnerup <- gsub("\n.*", "", elections$runnerup)
elections$runnerup <- gsub("[A-Z]. ", "", elections$runnerup)
elections[which(elections$runnerup==" Gore"),"runnerup"] <- "Al Gore" # putting the right name
elections[which(elections$runnerup==" Smith"),"runnerup"] <- "Al Smith" 

# merge data and create "new.data"
new.data <- merge(elections, election.data, by = "year")
 
# save the r file 
save(new.data, file = "new.data.RData")


