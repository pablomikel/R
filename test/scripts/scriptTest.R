library(tidyverse) #load tidyverse package that helps with visualizing tables through "tibbles"

#----- loading and plotting data

  afl <- read_csv("./data/afl.csv")     #make sure file path is the same
  tibble(afl)

  attendance <- afl %>%
    group_by(year, game_type) %>%
    summarise(attendance = mean(attendance))
  tibble(attendance)

  pic <- attendance %>%
    ggplot(aes(x = year, y = attendance)) +  # year on x-axis, attendance on y-axis
    facet_wrap(~game_type) +                 # separate "facets" for each game_type
    geom_point() +                           # add the data as "points"
    geom_smooth()                            # overlay a smooth regression line
  plot(pic)

#-----  explanation of pipes (%>%)

  response_time <- c(420, 619, 550, 521, 1003, 486, 512, 560, 495, 610)

  #process to log-transform the RT, calculate mean, exponentiate result, and finally round to two digits
    #variable based
    a <- log(response_time)
    b <- mean(a)
    c <- exp(b)
    d <- round(c,digits=2)
    print(d)

    #pipe based
    response_time %>%
      log %>%
      mean %>%
      exp %>%
      round(digits=2) %>%
      print

    ##pipes are used as a way to combine multiple operations into a coherent flow

#----- binomial distribution using sample() function

  die <- c("blue","blue","blue","blue","red","red")    #create a die with six sides (4 blue and 2)
  rolls <- sample(die, size = 20, replace = TRUE)  #roll the die 20 times with replacement
  rolls  #calls function

  n_blue <- sum(rolls == "blue")  #count the number of 'blue' rolls
  n_blue

  #repeat this experiment 5 times
  for (i in 1:5){
    rolls <- die %>% sample(size = 20, replace = TRUE)
    n_blue <- sum(rolls == "blue")
    print(n_blue)

  #repeat this experiment 100000 times with a frequency table output
  n_replications <- 100000
  n_blue <- numeric(length = n_replications)
  for(r in 1:n_replications){
    rolls <- die %>% sample(size = 20, replace = TRUE)
    n_blue[r] <- sum(rolls == "blue")
  }
  n_blue <- factor(n_blue, levels = 0:20, ordered = TRUE)
  frequencies <- table(n_blue)
  frequencies

  #plot table output as histogram
  as_tibble(frequencies, n = "count") %>%
  mutate(n_blue = as.numeric(n_blue)) %>%
  ggplot(aes(x=n_blue, y = count)) +
  geom_col(width = .5) +
  theme_bw()

#----- Confidence Intervals
  library(lsr)
  ciMean(afl) #defaults to 95% conf interval

  ciMean(x=afl, conf=.8) #this is how you change to a different conf interval

  ciMean(afl$away_score)  #to look at only one variable/column

#----- Paired Samples T-Test
  # tried to get this function in library(lsr) to work but it didn't so this is using the standard t.test function in the core R stats package

  t.test(afl$home_score,afl$away_score,alternative =c("two.sided","less","greater"),paired=TRUE,var.equal=FALSE,conf.level=0.95 )

#----- Independent Samples T-Test
  # same problem with this one

  afl$total_score <- afl$home_score + afl$away_score
  t.test(afl$total_score ~ afl$game_type,alternative =c("two.sided","less","greater"),paired=FALSE,var.equal=FALSE,conf.level=0.95 )

#----- Chi Square Test
  # at this point I've given up on the functions in library(lsr)
  chisq.test(afl$home_team, afl$weekday,correct = TRUE)

  # however this may be incorrect because of low counts for individual weekdays

  afl$weekday_small <- as.character(afl$weekday)
  weekgames <- afl$weekday_small %in% c("Mon","Tue","Wed","Thu","Fri")
  afl$weekday_small[weekgames] <- "M-F"
  afl$weekday_small <- as.factor(afl$weekday_small)

  chisq.test(afl$home_team, afl$weekday_small,correct = TRUE)

#----- Comparing Multiple Means
  # looking for high-scoring venues
  # only looking at venues with more than 100 games played

  venue_use <- table(afl$venue)
  majors <- venue_use[venue_use >= 100]

  # restrict the data to these games
  afl_majors <- afl[ afl$venue %in% names(majors), ]

  # creating a var that uses the built in ANOVA function in R
  mod <- aov(total_score ~ venue, afl_majors)
  anova(mod)

#----- Assessing Relationships
  # Do games become lower scoring over time?
  mod1 <- lm(total_score ~ year, afl)
  summary(mod1)

  # To plot this relationship
  yearly_score <- aggregate(
    formula = total_score ~ year,
    data = afl,
    FUN = mean
  )
  plot(
    x = yearly_score$year,
    y = yearly_score$total_score,
    type = "p",
    pch = 19,
    xlab = "Year",
    ylab = "Average Points per Game"
  )
  abline(coef = mod1$coef)

  # Now to do a Pearson's Correlation test
  cor.test(x = afl$total_score,
  y = afl$year)

  # to get R^2
  r <- -0.1318585
  print(r^2)

#----- End of Document
