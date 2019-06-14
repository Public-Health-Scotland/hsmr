# Hospital Standardised Mortality Ratios 

The Hospital Standardised Mortality Ratios publication is a quarterly publication that has been fully RAP'd. The entire process is contained within this R package and git repository. 

## Resources

* [A Quick Guide to git & GitHub](https://nhs-nss-transforming-publications.github.io/git-guide/index.html)
* 

## Folder Structure

All the publication files and folders are stored in the following directory:

[filepath]

This directory should contain:

* A "master" folder
* A folder named after each analyst who has worked on the publication e.g. a folder called "David"

### The "master" folder

The **master** folder is the **master copy** of the publication repository. This is the "production-ready" version that is used for the publication process each quarter. Within it there will be a folder called "data" which contains the output files/basefiles for all previous publications. The master copy should **never be edited** and should only be updated from approved changes pulled from GitHub.

### The individual analyst folders

These folders also contain up-to-date copies of the repository and these are the versions which are edited each time the publication is updated or each time a change to the process has to be made. Analysts should only work in their own folders on their own development branches. Once they are content that their changes are ready for the master branch, they must create a pull request on GitHub and have other analysts from the team review their changes and, if satisfied, merge them back into the master branch. It is then that the **master folder is updated.**

## The Repository

### Files and Folders

The hsmr publication process has been wrapped up inside an R package and so a number of files/folders within the repository are there to facilitate this. 

#### Folders
* **.git:** This is the folder containing the version control history of the repository. It can be safely ignored.
* **.Rpoj.user:** Where project-specific temporary filse are saved. This can be safely ignored. 
* **data:** This is where the output is saved. This folder is not tracked by git and so it is safe to have data stored here. 
* **man:** This is where the documentation files for the R package are stored. E.g. the help files for the `completeness` function.
* **markdown:** Where the markdown files and output are stored. 
* **R:** Where the functions are stored. 
* **reference_files:** Reference files/lookups specific to the package and not generic files are stored here e.g. the primary diagnosis groupings lookup.
* **tests:** Testing files for the package functions are saved here. This can be safely ignored. 

#### Files
* **.gitignore:** Any files that should not be tracked by git should be added to this file. 
* **.Rbuildignore::** This can be safely ignored. 
* **create_smr_data.R:** This is the script which uses the package to produce the SMR data for the publication.
* **create_trends_data.R:** This is the script which uses the package to produce the long term trends data for the publication.
* **create_excel_tables.R:** This is the script which uses the package to produce the excel tables. 
* **DESCRIPTION:** This is metadata for the R package. If the package is ever updated, the version number should be updated here.


## Running the publciation 

### Updating the code

The package is designed to require as little human intervention as possible. To update the publication each quarter, the analyst responsible for updating the scripts/running the publication should complete the following steps:

* Pull the most recent version of the master branch into their own folder 
* Create a fresh branch to make necessary changes
* Update the dates in the `setup_environment.R` file
* Check filepaths for lookups in `create_smr_data.R` and `create_trends_data.R` are still correct 
* Push new branch to GitHub
* Create pull request for another analyst to review changes
* Once changes have been approved, merge the branch into the master and delete
* If no more changes are required, pull the updated master branch into the **master folder**

### Running the code

* In the master folder, open up `create_smr_data.R`, highlight the entire script and run
* Check for any errors and investigate if necessary
* check the output in [folder] looks as it should
* In the master folder, open up `create_trends_data.R`, highlight the entire script and run
* As above, check for any errors and look at the output to see if it looks as it should do
* Open `create_excel_tables.R`, highglight the entire script and run. This script pulls in Excel templates which can be found in the reference_files folder (without data). The output files are saved in the data/output folder. 

Once this step is done, the raw data files and Excel tables for the publication have been produced. The final step is knitting the markdown documents, but that can't be done until the completeness figures are available. Once that is done:

* In the master/markdown folder, open both `.Rmd` scripts and click "knit"
* Check output
* A couple of manual steps are required to finish off the markdown documents (adding cover page, table of contents and formatting tables correctly). They are outlined in the readme in the [National Statistics Publication Templates repository.](https://github.com/NHS-NSS-transforming-publications/National-Stats-Template)

The raw output files from this process also feed into the Tableau dashboard, so once they are ready, they should be moved to the appropriate folder.

The raw output files (csv datafiles, basefiles) all have the publication date in the name, so there is no need to archive as each time the process is re-run, new files will be created. The only files which do get overwritten are the publication document files, but these are copied over to the publication folder as part of the normal publication process so are already archived.
