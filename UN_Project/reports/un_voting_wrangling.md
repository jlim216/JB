Progress report 1
================
JB Lim
2020-02-18

``` r
library(tidyverse)
library(lubridate)

# Parameters
un_voting_file <-
  here::here("c01-own/data/un_voting.rds")

country_border_data_democracy_file <-
  here::here("c01-own/data/country_border_data_democracy.rds")

path_name <-
  here::here("c01-own/data/un_voting_wrangled.rds")

#===============================================================================
# Reading in the data files.
un_voting <-  
  un_voting_file %>%
  read_rds() %>%
  mutate_at(vars(country), ~ str_to_lower(.))

country_info <-
  country_border_data_democracy_file %>%
  read_rds()
```

**Wrangling the UN voting data**

  - First, let’s explore the data quality of the un voting dataset
    provided by Professor Erik Voten.

<!-- end list -->

``` r
un_voting %>%
  summarize_all(~ sum(is.na(.))) %>%
  glimpse()
```

    ## Observations: 1
    ## Variables: 21
    ## $ rcid          <int> 0
    ## $ ccode         <int> 0
    ## $ member        <int> 331385
    ## $ vote          <int> 0
    ## $ country       <int> 7190
    ## $ countryname   <int> 0
    ## $ year          <int> 0
    ## $ session       <int> 0
    ## $ abstain       <int> 2997
    ## $ yes           <int> 200
    ## $ no            <int> 17374
    ## $ importantvote <int> 102050
    ## $ date          <int> 0
    ## $ unres         <int> 31345
    ## $ descr         <int> 200
    ## $ me            <int> 0
    ## $ nu            <int> 0
    ## $ di            <int> 0
    ## $ hr            <int> 0
    ## $ co            <int> 0
    ## $ ec            <int> 0

  - We notice that “member” has the most number of NAs. As nations
    joined the UN in different times since its establishment in 1945,
    any observation with NA values for “member” should be dropped.

<!-- end list -->

``` r
un_voting <-
  un_voting %>%
  drop_na(member)
```

  - Second, we notice that Czech Republic and Czechoslovakia constitute
    the nations with member code “9” (not a member). In particular,
    Czech Republic joined the United Nations in 1993 with the
    dissolution of Czechoslovakia into Czech and Slovakia. Thus, the
    data for Czech Republic until 1992 has the member code “9” and that
    of Czechoslovakia for 1993 onward has member code “9”. We would be
    dropping such data.

<!-- end list -->

``` r
un_voting %>%
  filter(member == 9) %>%
  glimpse() %>%
  count(countryname, year)
```

    ## Observations: 4,704
    ## Variables: 21
    ## $ rcid          <int> 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18…
    ## $ ccode         <int> 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, …
    ## $ member        <int> 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9…
    ## $ vote          <int> 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9…
    ## $ country       <chr> "cze", "cze", "cze", "cze", "cze", "cze", "cze", "cze",…
    ## $ countryname   <chr> "Czech Republic", "Czech Republic", "Czech Republic", "…
    ## $ year          <int> 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1…
    ## $ session       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ abstain       <int> 4, 8, 1, 10, 0, 2, 2, 0, 0, 3, 2, 5, 6, 7, 2, 9, 11, 1,…
    ## $ yes           <int> 29, 9, 28, 12, 25, 38, 45, 46, 41, 21, 10, 7, 10, 7, 27…
    ## $ no            <int> 18, 34, 22, 27, 18, 1, 0, 2, 0, 22, 31, 30, 26, 34, 19,…
    ## $ importantvote <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    ## $ date          <date> 1946-01-01, 1946-01-02, 1946-01-04, 1946-01-04, 1946-0…
    ## $ unres         <chr> "R/1/66", "R/1/79", "R/1/98", "R/1/107", "R/1/295", "R/…
    ## $ descr         <chr> "TO ADOPT A CUBAN AMENDMENT TO THE UK PROPOSAL REFERRIN…
    ## $ me            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    ## $ nu            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    ## $ di            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    ## $ hr            <lgl> FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, …
    ## $ co            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    ## $ ec            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, …

    ## # A tibble: 60 x 3
    ##    countryname     year     n
    ##    <chr>          <int> <int>
    ##  1 Czech Republic  1946    43
    ##  2 Czech Republic  1947    38
    ##  3 Czech Republic  1948    64
    ##  4 Czech Republic  1949   104
    ##  5 Czech Republic  1950    50
    ##  6 Czech Republic  1951     7
    ##  7 Czech Republic  1952    70
    ##  8 Czech Republic  1953    26
    ##  9 Czech Republic  1954    31
    ## 10 Czech Republic  1955    37
    ## # … with 50 more rows

``` r
un_voting <-
  un_voting %>%
  filter(member != 9)
```

  - Let’s also access whether there are any problems with Slovakia.

<!-- end list -->

``` r
un_voting %>%
  filter(countryname == "Slovakia") %>%
  count(vote)
```

    ## # A tibble: 5 x 2
    ##    vote     n
    ##   <int> <int>
    ## 1     1  1191
    ## 2     2   410
    ## 3     3   290
    ## 4     8    15
    ## 5     9    74

``` r
un_voting %>%
  filter(countryname == "Slovakia", vote == 9) %>%
  distinct(year)
```

    ## # A tibble: 1 x 1
    ##    year
    ##   <int>
    ## 1  1992

  - We see that Slovakia had 74 votes under “not a member.” Slovakia
    became a member of the United Nations in 1993. Thus, we see how the
    votes from 1992 for Slovakia have been counted as “not a member” and
    instead registered under Czechoslovakia. We drop this value as well.

<!-- end list -->

``` r
un_voting <-
  un_voting %>%
  filter(countryname != "Slovakia" | vote != 9)
```

  - Let’s also access in, greater depth, whether other nations face
    similar issues to Slovakia in terms of their vote choice record.

<!-- end list -->

``` r
vote_nine <-
  un_voting %>%
  left_join(
    country_info %>% 
      select(un_member_date, iso),
    by = c("country" = "iso")
  ) %>%
  filter(vote == 9) %>%
  count(countryname, year, un_member_date)

vote_nine %>%
  count(year == un_member_date | year == (un_member_date - 1))
```

    ## # A tibble: 3 x 2
    ##   `year == un_member_date | year == (un_member_date - 1)`     n
    ##   <lgl>                                                   <int>
    ## 1 FALSE                                                       3
    ## 2 TRUE                                                       70
    ## 3 NA                                                          8

``` r
vote_nine %>%
  filter(is.na(un_member_date))
```

    ## # A tibble: 8 x 4
    ##   countryname                year un_member_date     n
    ##   <chr>                     <int>          <dbl> <int>
    ## 1 Taiwan, Province of China  1971             NA   112
    ## 2 Yemen Arab Republic        1947             NA     6
    ## 3 Yemen People's Republic    1966             NA     1
    ## 4 Yemen People's Republic    1967             NA    26
    ## 5 Yugoslavia                 1992             NA    73
    ## 6 Yugoslavia                 1993             NA     1
    ## 7 Yugoslavia                 2000             NA     3
    ## 8 Zanzibar                   1963             NA    15

``` r
vote_nine %>%
  filter(year != un_member_date, year != (un_member_date - 1))
```

    ## # A tibble: 3 x 4
    ##   countryname  year un_member_date     n
    ##   <chr>       <int>          <dbl> <int>
    ## 1 China        1971           1945     5
    ## 2 Mauritius    1966           1968     1
    ## 3 Yemen        2010           1947     1

  - We notice that most “9” voting codes for most nations come from one
    or two specific years. Out of 81 such cases 70 (86.4197531 %), the
    year with “9” voting code matched the membership year (or the
    preceding year). For instance, in the case of Sweden, the nation was
    admitted in November 19, 1946. Hence, the nation was recorded as “9”
    for voting in resolutions prior to that date in the same year. We
    should hence be dropping these values.

  - The remaining 11 cases mainly come from nations that dissolved,
    merged or transferred their membership status since the
    establishment of the 1945. Indeed, they include China (previously
    held by Taiwan), Yemen (North Yemen, South Yemen), Zanzibar and
    Yugoslavia. These “edge” cases will be covered below in greater
    depth. For now, we drop these value for the similar logic as above.

<!-- end list -->

``` r
un_voting <-
  un_voting %>%
  filter(vote != 9)
```

  - Finally, aside from Czechoslovakia, we notice that seven countries
    have been members of the United Nations and currently are not part
    of the United Nations. The cases can be divided up as below and we
    check if there are any discrepencies in country names resulting from
    changing membership status or dissolution of states.

<!-- end list -->

``` r
# Full list of countries
names <-
  un_voting %>%
  distinct(countryname, country)

names %>%
  anti_join(country_info, by = c("country" = "iso")) %>%
  pull(countryname)
```

    ## [1] "Czechoslovakia"              "Yugoslavia"                 
    ## [3] "Taiwan, Province of China"   "Yemen Arab Republic"        
    ## [5] "Zanzibar"                    "Yemen People's Republic"    
    ## [7] "Federal Republic of Germany" "German Democratic Republic"

1.  Taiwan lost its status in the United Nations in 1971. The People’s
    Republic of China replaced Taiwan’s seat.

2.  Yugoslavia was a member of the United Nations from 1945 to 1992 when
    it disintegrated into Croatia, Slovenia, Bosnia and Herzegovina, and
    Serbia and Montenegro (called Federal Republic of Yugoslavia until
    2003). Yugoslavia “re-joined” the United Nations in 2000 with the
    entry of “Serbia and Montenegro” as one federation of state.
    Starting 2006, the seat was transferred to Serbia after Montenegro’s
    independence from Serbia.

3.  Yemen Arab Republic (North Yemen) and Yemen People’s Republic (South
    Yemen) united in 1990 to form the current Republic of Yemen.

4.  Similarly, the German Democratic Republic (East Germany) acceded to
    the Federal Republic of Germany (West Germany) in 1990.

5.  Zanzibar, as a semi-autonomous region of Tanzania, joined the UN in
    1963, but soon after merged with the United Republic of Tanganyika
    to form the United Republic of Tanzania on 1964.

Given these information, let’s check to see if all the data (including
country name corresponding to correct years) are updated and accurate.

``` r
# Taiwan
un_voting %>%
  filter(countryname == "Taiwan, Province of China", year > 1971) %>% 
  count() %>%
  pull()
```

    ## [1] 0

``` r
# Zanzibar
un_voting %>%
  filter(countryname == "Zanzibar", year != 1963) %>%
  count() %>%
  pull()
```

    ## [1] 0

  - As shown above, the cases for Taiwan (\#1) and Zanzibar(\#5) are
    relatively straightforward and the dataset has accurate information
    for the two states.

<!-- end list -->

``` r
# Yugoslavia
un_voting %>%
  filter(countryname == "Yugoslavia", year > 1991) %>% 
  count(year)
```

    ## # A tibble: 19 x 2
    ##     year     n
    ##    <int> <int>
    ##  1  1992     2
    ##  2  2000    63
    ##  3  2001    67
    ##  4  2002    73
    ##  5  2003    74
    ##  6  2004    72
    ##  7  2005    74
    ##  8  2006    87
    ##  9  2007    77
    ## 10  2008    76
    ## 11  2009    65
    ## 12  2010    66
    ## 13  2011    68
    ## 14  2012    70
    ## 15  2013    60
    ## 16  2014    80
    ## 17  2015    78
    ## 18  2016    75
    ## 19  2017    94

``` r
# Serbia
un_voting %>%
  filter(countryname == "Serbia", year > 1999) %>% 
  count(year, country)
```

    ## # A tibble: 1 x 3
    ##    year country     n
    ##   <int> <chr>   <int>
    ## 1  2018 srb        95

``` r
# Montenegro
un_voting %>%
  filter(countryname == "Montenegro", year > 1999) %>% 
  count(year)
```

    ## # A tibble: 13 x 2
    ##     year     n
    ##    <int> <int>
    ##  1  2006    85
    ##  2  2007    77
    ##  3  2008    76
    ##  4  2009    65
    ##  5  2010    66
    ##  6  2011    68
    ##  7  2012    70
    ##  8  2013    60
    ##  9  2014    80
    ## 10  2015    78
    ## 11  2016    75
    ## 12  2017    94
    ## 13  2018    95

  - For Yugoslavia (\#2), we notice how the dataset did not change its
    name to “Serbia” starting 2000. Specifically, while Montenegro was
    accurately named starting the year “2006” when it declared
    independence from Serbia, the data for the name “Serbia” only begins
    in 2018. Hence, we should rename Yugoslavia starting the year 2000
    to “Serbia” to reflect these changes.

<!-- end list -->

``` r
# rename
un_voting <-
  un_voting %>%
  mutate(
    country = if_else(country == "yug" & year > 1999, "srb", country),
    countryname = 
      if_else(countryname == "Yugoslavia" & year > 1999, "Serbia", countryname)
  )
```

  - Let’s finally check the data for the Yemens (\#3) and Germanys
    (\#4).

<!-- end list -->

``` r
# Yemen
un_voting %>%
  filter(countryname == "Yemen") %>%
  count(countryname, year)
```

    ## # A tibble: 28 x 3
    ##    countryname  year     n
    ##    <chr>       <int> <int>
    ##  1 Yemen        1991    74
    ##  2 Yemen        1992    75
    ##  3 Yemen        1993    65
    ##  4 Yemen        1994    68
    ##  5 Yemen        1995    79
    ##  6 Yemen        1996    76
    ##  7 Yemen        1997    70
    ##  8 Yemen        1998    62
    ##  9 Yemen        1999    68
    ## 10 Yemen        2000    67
    ## # … with 18 more rows

``` r
# Germany
un_voting %>%
  filter(countryname == "Germany") %>%
  count(countryname, year)
```

    ## # A tibble: 29 x 3
    ##    countryname  year     n
    ##    <chr>       <int> <int>
    ##  1 Germany      1990    86
    ##  2 Germany      1991    74
    ##  3 Germany      1992    75
    ##  4 Germany      1993    65
    ##  5 Germany      1994    68
    ##  6 Germany      1995    79
    ##  7 Germany      1996    76
    ##  8 Germany      1997    70
    ##  9 Germany      1998    62
    ## 10 Germany      1999    68
    ## # … with 19 more rows

  - We can confirm that the names are merged to their current names
    staring the 1991 and 1990 respectively.

  - We should be aware of these “edge” cases when working with the data
    in the next part.

**Save Answer to rds file**

``` r
un_voting %>%
  write_rds(path_name)
```
