# Hospital Standardised Mortality Ratios 

The Hospital Standardised Mortality Ratios publication is a quarterly publication that has been fully RAP'd. The entire process is contained within this R package and git repository. 

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

## Updating the Repository 

The package is designed to require as little human intervention as possible. To update the publication each quarter, the analyst responsible for updating the scripts/running the publication should complete the following steps:

* Pull the most recent version of the master branch into their own folder 
* Create a fresh branch to make necessary changes
* Update the dates in the `setup_environment.R` file
* Check filepaths for lookups in `create_smr_data.R` and `create_trends_data.R` are still correct 
* Push new branch to GitHub
* Create pull request for another analyst to review changes
* Once changes have been approved, merge the branch into the master and delete
* If no more changes are required, pull the updated master branch into the **master folder**
