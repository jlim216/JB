# join democracy data with the border and country profile data
# entering in data for the Congo Republic (missing geography data)

# Author: JB Lim
# Version: 2020-02-17

# Libraries
library(tidyverse)
library(lubridate)

# Parameters

country_border_data_file <-
  here::here("c01-own/data/country_border_data.rds")

democracy_file <-
  here::here("c01-own/data/country_democracy.rds")

vars_recode <-
  c(
    "Bahamas" = "The Bahamas",
    "Cape Verde" = "Cabo Verde",
    "Serbia" = "Republic of Serbia",
    "Cote d'Ivoire" = "Ivory Coast",
    "Czech Republic" = "Czechia",
    "Sao Tome and Principe" = "São Tomé and Principe",
    "Timor-Leste" = "East Timor",
    "St. Lucia" = "Saint Lucia",
    "St. Kitts and Nevis" = "Saint Kitts and Nevis",
    "St. Vincent and the Grenadines" = "Saint lls[?2004l
README.md				country_border_data.R			country_data.R				un_voting.R
country_border.R			country_border_data_democracy.R		country_democracy.R
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 scripts % [K[?2004hlls[?2004l
README.md				country_border_data.R			country_data.R				un_voting.R
country_border.R			country_border_data_democracy.R		country_democracy.R
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 scripts % [K[?2004hlls[?2004l
README.md				country_border_data.R			country_data.R				un_voting.R
country_border.R			country_border_data_democracy.R		country_democracy.R
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 scripts % [K[?2004hRRScript country_border_data_democracy.R[?2004l
── [1mAttaching packages[22m ────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
[32m✓[39m [34mggplot2[39m 3.2.1     [32m✓[39m [34mpurrr  [39m 0.3.3
[32m✓[39m [34mtibble [39m 2.1.3     [32m✓[39m [34mdplyr  [39m 0.8.4
[32m✓[39m [34mtidyr  [39m 1.0.0     [32m✓[39m [34mstringr[39m 1.4.0
[32m✓[39m [34mreadr  [39m 1.3.1     [32m✓[39m [34mforcats[39m 0.4.0
── [1mConflicts[22m ───────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
[31mx[39m [34mdplyr[39m::[32mfilter()[39m masks [34mstats[39m::filter()
[31mx[39m [34mdplyr[39m::[32mlag()[39m    masks [34mstats[39m::lag()

Attaching package: ‘lubridate’

The following object is masked from ‘package:base’:

    date

[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 scripts % [K[?2004hccd ~[?2004l
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 ~ % [K[?2004hlls[?2004l
Desktop/		Downloads/		Library/		Music/			Public/			aggregate.rds
Documents/		GitHub/			Movies/			Pictures/		Stanford University/	aggregate_2.rds
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 ~ % [K[?2004hs ccd GitHub[?2004l
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 GitHub % [K[?2004hccdc  lls[?2004l
README.md	dcl-2020-01/	sc-evictions/
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 GitHub % [K[?2004hccd G dcl= -2020-01[?2004l
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 dcl-2020-01 % [K[?2004hccdc  lls[?2004l
data/			dcl-2020-01.Rproj	jb/			old tasks/		tasks/
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 dcl-2020-01 % [K[?2004hccd jb[?2004l
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 jb % [K[?2004hlls[?2004l
README.md			c13-election-2016-2/		e02-data-basics/		e16-vis-scales/			e30-spatial-basics/
_progress/			c14-medicare-wsj-2/		e03-tidy-data/			e17-function-basics/		e31-vis-continuous-continuous/
c01-own/			c15-opioids-1/			e04-vis-basics/			e18-string-basics/		e32-manip-scoped-2/
c02-michelson-1880/		c16-napoleon/			e05-documentation/		e19-parse-basics/		e33-list-cols/
c03-diamonds-1/			c17-election-2016-3/		e06-code-style/			e20-factor-basics/		e34-regexps/
c04-titanic/			c18-voter-survey-1/		e07-manip-basics/		e21-purrr-basics/		e35-vis-time-series/
c05-old-faithful/		c19-opioids-2/			e08-rmarkdown-basics/		e22-eda-2d/			e36-purrr-parallel/
c06-diamonds-2/			c20-cholera-map/		e09-vis-discrete-continuous/	e23-manip-scoped/		e37-timespans/
c07-co2/			c21-voter-survey-2/		e10-pivot-1/			e24-pivot-2/			e38-spatial-vis/
c08-health-inequality-map/	c22-oldest-persons/		e11-manip-one-table/		e25-vis-distributions/		e39-parse-details/
c09-election-2016-1/		c23-blood-pressure-1/		e12-data-structure-basics/	e26-tidy-eval/			e40-model-eda/
c10-movie-ratings/		c24-health-inequality/		e13-eda-1d/			e27-datetime-basics/		e41-string-details/
c11-medicare-wsj-1/		c25-hockey-stick/		e14-getting-help/		e28-function-vector/		jb.Rproj
c12-project-workflow/		e01-setup/			e15-relational-basics/		e29-purrr-mutate/
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 jb % [K[?2004hccd c01-own[?2004l
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 c01-own % [K[?2004hc lls[?2004l
Makefile	challenge.Rmd	data/		docs/		reports/	team-challenge/
README.md	challenge.md	data-raw/	eda/		scripts/
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 c01-own % [K[?2004hccd team-challenge[?2004l
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004hc lls[?2004l
Makefile	README.md	master.Rmd
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004h%%m%  mmake  m  %% make[?2004l
fg: no current job
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004hccd master.Rmd[?2004l
cd: not a directory: master.Rmd
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004hmmake[?2004l
sed -E \
	-e '/<!-- task-begin -->/,/<!-- task-end -->/d' \
	-e '/# task-begin/,/# task-end/d' \
	-e '/# solution-(begin|end)/d' \
	-e '/<!-- solution-(begin|end)/d' \
	< master.Rmd > solution.Rmd
sed -E \
	-e 's/^author:.*|.*# yaml-author.*/author: \"Your Name\"/' \
	-e 's/^date:.*/date: 2019-/' \
	-e '/<!-- solution-begin -->/,/<!-- solution-end -->/d' \
	-e '/# solution-begin/,/# solution-end/d' \
	-e '/# task-(begin|end)/d' \
	-e '/<!-- task-(begin|end)/d' \
	< master.Rmd > challenge.Rmd
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004h%%ma %  mmake[?2004l
make: Nothing to be done for `all'.
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004hmmake[?2004l
sed -E \
	-e '/<!-- task-begin -->/,/<!-- task-end -->/d' \
	-e '/# task-begin/,/# task-end/d' \
	-e '/# solution-(begin|end)/d' \
	-e '/<!-- solution-(begin|end)/d' \
	< master.Rmd > solution.Rmd
sed -E \
	-e 's/^author:.*|.*# yaml-author.*/author: \"Your Name\"/' \
	-e 's/^date:.*/date: 2019-/' \
	-e '/<!-- solution-begin -->/,/<!-- solution-end -->/d' \
	-e '/# solution-begin/,/# solution-end/d' \
	-e '/# task-(begin|end)/d' \
	-e '/<!-- task-(begin|end)/d' \
	< master.Rmd > challenge.Rmd
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004hmmake[?2004l
sed -E \
	-e '/<!-- task-begin -->/,/<!-- task-end -->/d' \
	-e '/# task-begin/,/# task-end/d' \
	-e '/# solution-(begin|end)/d' \
	-e '/<!-- solution-(begin|end)/d' \
	< master.Rmd > solution.Rmd
sed -E \
	-e 's/^author:.*|.*# yaml-author.*/author: \"Your Name\"/' \
	-e 's/^date:.*/date: 2019-/' \
	-e '/<!-- solution-begin -->/,/<!-- solution-end -->/d' \
	-e '/# solution-begin/,/# solution-end/d' \
	-e '/# task-(begin|end)/d' \
	-e '/<!-- task-(begin|end)/d' \
	< master.Rmd > challenge.Rmd
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004hmmake[?2004l
make: Nothing to be done for `all'.
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004ha mmake[?2004l
sed -E \
	-e '/<!-- task-begin -->/,/<!-- task-end -->/d' \
	-e '/# task-begin/,/# task-end/d' \
	-e '/# solution-(begin|end)/d' \
	-e '/<!-- solution-(begin|end)/d' \
	< master.Rmd > solution.Rmd
sed -E \
	-e 's/^author:.*|.*# yaml-author.*/author: \"Your Name\"/' \
	-e 's/^date:.*/date: 2019-/' \
	-e '/<!-- solution-begin -->/,/<!-- solution-end -->/d' \
	-e '/# solution-begin/,/# solution-end/d' \
	-e '/# task-(begin|end)/d' \
	-e '/<!-- task-(begin|end)/d' \
	< master.Rmd > challenge.Rmd
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004hmmake[?2004l
sed -E \
	-e '/<!-- task-begin -->/,/<!-- task-end -->/d' \
	-e '/# task-begin/,/# task-end/d' \
	-e '/# solution-(begin|end)/d' \
	-e '/<!-- solution-(begin|end)/d' \
	< master.Rmd > solution.Rmd
sed -E \
	-e 's/^author:.*|.*# yaml-author.*/author: \"Your Name\"/' \
	-e 's/^date:.*/date: 2019-/' \
	-e '/<!-- solution-begin -->/,/<!-- solution-end -->/d' \
	-e '/# solution-begin/,/# solution-end/d' \
	-e '/# task-(begin|end)/d' \
	-e '/<!-- task-(begin|end)/d' \
	< master.Rmd > challenge.Rmd
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004hmmake[?2004l
sed -E \
	-e '/<!-- task-begin -->/,/<!-- task-end -->/d' \
	-e '/# task-begin/,/# task-end/d' \
	-e '/# solution-(begin|end)/d' \
	-e '/<!-- solution-(begin|end)/d' \
	< master.Rmd > solution.Rmd
sed -E \
	-e 's/^author:.*|.*# yaml-author.*/author: \"Your Name\"/' \
	-e 's/^date:.*/date: 2019-/' \
	-e '/<!-- solution-begin -->/,/<!-- solution-end -->/d' \
	-e '/# solution-begin/,/# solution-end/d' \
	-e '/# task-(begin|end)/d' \
	-e '/<!-- task-(begin|end)/d' \
	< master.Rmd > challenge.Rmd
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004h[?2004l
[1m[7m%[27m[1m[0m                                                                                                                                                                   [0m[27m[24m[Jjblim@DN0a0ffb89 team-challenge % [K[?2004h