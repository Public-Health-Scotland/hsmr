# Code to produce the publication's report and summary word documents

###############################################.
## Packages ----
###############################################.
library(phstemplates)

###############################################.
## Knitting documents ----
###############################################.

# creating publication's report
compile_report(rmd_filename = "markdown/PHS-NATIONAL-STATS-REPORT.Rmd", 
               cover_filename = "markdown/phs-natstats-cover.docx", 
               filename_out = "markdown/PHS-NATIONAL-STATS-REPORT.docx",
               title = "Hospital Standardised Mortality Ratio",
               subtitle = "July 2022 to June 2023",
               date = "14 November 2023")

# Creating publication's summary
rmarkdown::render("markdown/PHS-NATIONAL-STATS-SUMMARY.Rmd")


## END
