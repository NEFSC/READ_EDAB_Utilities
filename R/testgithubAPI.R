# test access to GitHub API through R

# install packages from CRAN, this should work
# if it asks to install dependencies, say yes to all

install.packages(c("remotes", "devtools", "jsonlite"))

# test API access

# can you install packages from github?

remotes::install_github('tidyverse/dplyr')

devtools::install_github('tidyverse/dplyr')

# can you retrieve issues from a github repo?

jsonlite::fromJSON('https://api.github.com/repos/tidyverse/dplyr/issues')

