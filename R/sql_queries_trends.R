z_query_smr01_ltt <- paste("select LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, CIS_MARKER, POSTCODE, SPECIALTY,",
                           "ADMISSION_TYPE, DISCHARGE_TYPE, HBTREAT_CURRENTDATE,",
                           "CASE WHEN age_in_years BETWEEN 0 AND 19 THEN '1'",
                           "WHEN age_in_years BETWEEN 20 AND 39 THEN '2'",
                           "WHEN age_in_years BETWEEN 40 AND 59 THEN '3'",
                           "WHEN age_in_years BETWEEN 60 AND 79 THEN '4'",
                           "WHEN age_in_years >= 80 THEN '5'",
                           "ELSE 'NULL' END age_grp from SMR01_PI",
                           "where ADMISSION_DATE >= to_date(",z_start_date,", 'yyyy-MM-dd') AND ADMISSION_DATE <= to_date(",z_end_date,",'yyyy-MM-dd')",
                           "ORDER BY LINK_NO, ADMISSION_DATE, RECORD_TYPE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI")

z_query_gro <- paste("select LINK_NO, DATE_OF_DEATH",
                     "from ANALYSIS.GRO_DEATHS_C",
                     "where DATE_OF_DEATH >= to_date(",z_start_date,",'yyyy-MM-dd')",
                     "ORDER BY LINK_NO")
