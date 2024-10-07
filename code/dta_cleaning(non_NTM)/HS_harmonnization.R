


# http://www.justinrpierce.com/index_files/Pierce_Schott_JOS_2012.pdf


if(!require('concordance')) {
  install.packages('concordance')
  library('concordance')
}

# can use a function to concord
# https://rdocumentation.org/packages/concordance/versions/2.0.0/topics/concord_hs
concord_hs(sourcevar, origin, destination, dest.digit = 4, all = FALSE)

concord(sourcevar = c("12"),
        origin = "HS5", destination = "HS4",
        dest.digit = 2, all = TRUE)

concord(sourcevar = c("010111", "382390"),
        origin = "HS0", destination = "HS5",
        dest.digit = 6, all = TRUE)



