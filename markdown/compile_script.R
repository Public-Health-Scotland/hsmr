library(phstemplates)
library(here)
here()

compile_report(rmd_filename = "markdown/PHS-NATIONAL-STATS-REPORT.Rmd", 
               cover_filename = "markdown/phs-natstats-cover.docx", 
               filename_out = "markdown/PHS-NATIONAL-STATS-REPORT.docx",
               title = "Hospital Standardised Mortality Ratio",
               subtitle = "October 2020 to September 2021",
               date = "08 February 2022")

