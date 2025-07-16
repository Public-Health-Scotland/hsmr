# Code to produce the publication's report and summary word documents

###############################################.
## Packages ----
###############################################.
library(phstemplates)

###############################################.
## Knitting documents ----
###############################################.

# creating publication's report
compile_report(rmd_filename = "markdown/PHS-ACCREDITED-STATS-REPORT.Rmd", 
               cover_filename = "markdown/phs-accrstats-cover.docx", 
               filename_out = "markdown/PHS-ACCREDITED-STATS-REPORT.docx",
               title = "Hospital Standardised Mortality Ratio",
               subtitle = "April 2024 to March 2025",
               date = "12 August 2025")

# Creating publication's summary
rmarkdown::render("markdown/PHS-ACCREDITED-STATS-SUMMARY.Rmd")


## END
