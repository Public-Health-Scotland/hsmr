#########################################################################
# Name of file - sql_queries_smr.R
# Data release - Quarterly HSMR publication
# Original Authors - David Caldwell and Anna Price
# Orginal Date - July 2018
#
# Type - SQL queries
# Written/run on - RStudio server
# Version of R - 3.2.3
#
# Description - Defines SQL queries sourced and used in create_smr_data
#########################################################################

z_query_smr01 <- paste("select LINK_NO, ADMISSION_DATE, DISCHARGE_DATE,",
                       "CIS_MARKER, SEX, LOCATION, POSTCODE, DISCHARGE_TYPE,",
                       "MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2,",
                       "OTHER_CONDITION_3, OTHER_CONDITION_4,",
                       "OTHER_CONDITION_5,",
                       "CASE WHEN ADMISSION_TYPE BETWEEN 20 AND 48 OR",
                       "ADMISSION_TYPE = 18",
                       "THEN '2'",
                       "WHEN ADMISSION_TYPE BETWEEN 10 AND 12 OR",
                       "ADMISSION_TYPE = 19",
                       "THEN '1'",
                       "ELSE 'NULL'",
                       "END admgrp,",
                       "CASE WHEN ADMISSION_TRANSFER_FROM IN",
                       "('20','21','22','23','24','25','26','27','28','29')",
                       "THEN '1'",
                       "WHEN ADMISSION_TRANSFER_FROM IN",
                       "('60','61','62','63','64','65','66','67','68','69',",
                       "'70')",
                       "THEN '2'",
                       "WHEN ADMISSION_TRANSFER_FROM IN",
                       "('10','11','12','13','14','15','16','17','18','19')",
                       "THEN '3'",
                       "WHEN ADMISSION_TRANSFER_FROM IN",
                       "('30','31','32','33','34','35','36','37','38','39')",
                       "THEN '4'",
                       "WHEN ADMISSION_TRANSFER_FROM IN",
                       "('50','51','52','53','54','55','56','57','58','59',",
                       "'5A','5B','5C', '5D', '5E', '5F', '5G', '5H')",
                       "THEN '5'",
                       "WHEN ADMISSION_TRANSFER_FROM IN",
                       "('40','41','42','43','44','45','46','47','48','49',",
                       "'4A','4B','4C', '4D', '4E', '4F', '4G', '4H')",
                       "THEN '6'",
                       "ELSE 'NULL'",
                       "END admfgrp,",
                       "SPECIALTY,",
                       "CASE WHEN INPATIENT_DAYCASE_IDENTIFIER = 'I'",
                       "THEN '1'",
                       "WHEN INPATIENT_DAYCASE_IDENTIFIER = 'D'",
                       "THEN '2'",
                       "ELSE 'NULL'",
                       "END ipdc,",
                       "CASE WHEN age_in_years BETWEEN 0 AND 9 THEN '1'",
                       "WHEN age_in_years BETWEEN 10 AND 19 THEN '2'",
                       "WHEN age_in_years BETWEEN 20 AND 29 THEN '3'",
                       "WHEN age_in_years BETWEEN 30 AND 39 THEN '4'",
                       "WHEN age_in_years BETWEEN 40 AND 49 THEN '5'",
                       "WHEN age_in_years BETWEEN 50 AND 59 THEN '6'",
                       "WHEN age_in_years BETWEEN 60 AND 69 THEN '7'",
                       "WHEN age_in_years BETWEEN 70 AND 79 THEN '8'",
                       "WHEN age_in_years BETWEEN 80 AND 89 THEN '9'",
                       "WHEN age_in_years >= 90 THEN '10'",
                       "ELSE 'NULL'",
                       "END age_grp,",
                       "to_char(admission_date,'Q') AS quarter,",
                       "extract(year from admission_date) AS year,",
                       "HBTREAT_CURRENTDATE, AGE_IN_YEARS from SMR01_PI",
                       "where ADMISSION_DATE BETWEEN",
                       "TO_DATE(", shQuote(z_start_date, type = "sh"),",'yyyy-mm-dd') AND",
                       "TO_DATE(", shQuote(z_end_date, type = "sh"),",'yyyy-mm-dd')",
                       "ORDER BY LINK_NO, ADMISSION_DATE, RECORD_TYPE,",
                       "DISCHARGE_DATE, ADMISSION, DISCHARGE, URI")

z_query_smr01_minus5 <- paste("select LINK_NO, ADMISSION_DATE, DISCHARGE_DATE,",
                              "OLD_SMR1_TADM_CODE, CIS_MARKER, SPECIALTY,",
                              "MAIN_CONDITION from SMR01_PI",
                              "where ADMISSION_DATE BETWEEN",
                              "TO_DATE(", shQuote(z_start_date_5, type = "sh"),",'yyyy-mm-dd')",
                              "AND TO_DATE(", shQuote(z_end_date, type = "sh"),",'yyyy-mm-dd')",
                              "ORDER BY LINK_NO, ADMISSION_DATE, RECORD_TYPE,",
                              "DISCHARGE_DATE, ADMISSION, DISCHARGE, URI")

z_query_gro <- paste("select LINK_NO, DATE_OF_DEATH",
                     "from ANALYSIS.GRO_DEATHS_C",
                     "where DATE_OF_DEATH >=",
                     "TO_DATE(", shQuote(z_start_date, type = "sh"),",'yyyy-mm-dd')",
                     "ORDER BY LINK_NO")
