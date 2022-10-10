UN Voting Patterns
================
Your Name
2019-

``` r
# Libraries
library(tidyverse)
library(sf)
library(dcl)

# If you will not be providing answers, simply delete the following lines.
# Parameters
  # File for downloaded answers
file_answers <- ""




#===============================================================================

# Read in answers
if (str_length(file_answers) > 0) {
  answers <- read_rds(file_answers) 
}
```

## Challenge

**Background Context**

  - This challenge deals with the **“United Nations General Assembly
    Voting Data”** posted on the *Harvard Dataverse.* The dataset
    examines all the “roll-call votes in the UN General Assembly” from
    1946 to 2018.

  - The code book
    <https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/LEJUQZ/IPRJ61&version=21.0>
    of the dataset articulates the variables of the raw UNGA dataset.
    Notable variables include Vote Choice (with 1 as Yes, 2 as Abstain,
    3 as No, 8 as Absent, 9 as Not a member), Country Code, Year of
    Vote, Votes identified as important by U.S. State Department report
    and Issue Codes.
    
      - Issue Codes include: Votes relating to the Palestinian Conflict,
        Nuclear weapons and nuclear material, Arms control and
        disarmanent, Colonialism, Human Rights, Economic Development.

  - The UN voting dataset has already been partially wrangled to account
    for transitions and dissolutions in certain states such as
    Yugoslavia or Czechoslovakia. This challenge focuses more on the
    exploratory data analysis and visualization aspect.

  - The country information file contains the boundary and relevant
    information (from Natural Earth) for 192 UN member states. The
    information dataset also includes While the United Nations has 193
    member states, the information dataset provided excludes Tuvalu due
    to its absence from the original “Natural Earth” dataset.

  - Both datasets can be found in the Data Box
    <https://stanford.app.box.com/folder/104837039459>.

  - Finally, the information dataset includes additional facts on the
    status of democracy in 2019 (imported from “Freedom in the World”)
    and UN Membership status from the Gapminder dataset covered in the
    Stanford Data Challenge Lab course.

**Objective**

In this challenge, we will be examining the status of the United States’
hegemonic leadership in the post Cold-War era (1992-2018) by assessing
the voting behaviors of other nations in the United Nations General
Assembly.

**Reading Data + Wrangling**

**q1** After reading in both files, filter for UN General Assembly Votes
from 1992 to 2018. Then, combine the United Nations voting dataset with
the country information dataset. In doing so, note the following:

  - We will be dropping “Czechoslovakia” and “Yugoslavia” which ceased
    to exist in 1992 and 1993 respectively (only represents 77 out of
    862969 observations) from the UN General Assembly Dataset.

  - Rename: “country” to “iso”, “me” to “palestine”, “nu” to
    “nuclear\_weapons”, “di” to “disarmament”, “hr” to
    “human\_rights”, “co” to “colonialism”, “ec” to “economic\_dev”

  - Select the following variables: rcid, vote, iso, countryname, year,
    important vote, me, nu, di, hr, co, ec + all the variables in
    country information.

<!-- end list -->

``` r
# Print results
if (exists("q1")) q1

# Compare result with answer
if (exists("q1")) compare(answers$q1, q1)
```

**Calculating Alignment**

**q2** Let’s try to calculate the “alignment” of votes between the
United States and other UN member states for all the unique resolutions
since 1992. The rcid code uniquely identifies each distinct resolution
from 1992 to 2018. The variable vote indicates the countries vote for
the specific resolution. For now, let’s focus on the votes “Yes”, “No”
and “Abstain.” Refer to the documentation on the dataset to understand
the numeric codes for each corresponding vote.

To do this, we would need the following pieces of information: 1.
Retrieve information on the United States’ (or any desired nations’)
voting record for every resolution (rcid). 2. Compare that with other
nations through grouping by each rcid. 3. Calculating the difference in
votes using absolute values and providing appropriate labels.

**q2.1** Build a function called “target\_nation” that takes in a string
of iso code and returns a tibble with two variables: each unique rcid
and the target nations’ vote.

**q2.2** Build another function called “Alignment” that takes in a
tibble of target nation votes and returns a tibble with a new variable
“difference.” The “difference” variable should be factorized from
numeric values into categorical values(“Agreed”, “Abstained”,
“Disagreed”).

Hint: The variable for the “target nation votes” should have a “general
name,” so that that the user does not have to manually change the name
everytime the alignment is calculated. This may require an additional
line of code for the function in quesiton 2.

Hint 2: The target nation being examined should be filtered out before
calculating the alignment.

**q3** Use the two functions above to create a tibble with the voting
alignment trend for the United States from 1992 to 2018. Store the
answers in q3.

Note: Exclude the column listing the target nation’s votes.

``` r
# Print results
if (exists("q3")) q3

# Compare result with answer
if (exists("q3")) compare(answers$q3, q3)
```

**q4** Perform the same task as q3 for China. Store the answers in q4.

``` r
# Print results
if (exists("q4")) q4

# Compare result with answer
if (exists("q4")) compare(answers$q4, q4)
```

**EDA 1D**

**q5** Plot the distribution of “difference” from q3 for the United
States. Do the same for China using the information from q4. What
conclusions can you draw?

**q6** How much percent of the votes in q1 are considered to be
“important”? What is the distribution of “difference” for “important”
votes in the case of the United States? What conclusions can be drawn?

**Visualization**

**q7** Visualize the “percentage of votes in agreement with the U.S.” on
a map. Do the same for China. What conclusions can you draw?

Hint: Making a function would simplify the process\!

**q8** Visualize the “percentage of votes in agreement with the U.S.”
for votes classified as important by the State Department on a map. Do
the same for China. What conclusions can you draw?

**U.S. and China Differs** (Optional)

**q9** Consider the resolutions where the U.S. and China had opposing
votes.

Which voting option did each nation “lean most favorably towards” over
1992-2018? Was there any difference for votes classified as “important”?

**q10** Finally, the democracy and income variables on to the map. Are
there any possible correlations?
