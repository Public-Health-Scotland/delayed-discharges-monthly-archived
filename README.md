The [Monthly Delayed Discharges publication](https://www.isdscotland.org/Health-Topics/Health-and-Social-Community-Care/Delayed-Discharges/) has been converted to RAP level 4a (version controlled and peer reviewed).


## Folder Structure

All the publication files and folders are stored in the following directory:

/.../delayed_discharges/...

This directory should contain:

* A "master" folder
* A folder named after each analyst who has worked on the publication e.g. a folder called "David", "Lucinda", "Robyn" etc.

### The "master" folder

The **master** folder is the **master copy** of the publication repository. This is the "production-ready" version that is used for the publication process each quarter. Within it there will be a folder called "data" which contains the output files/basefiles for all previous publications. The master copy should **never be edited** and should only be updated from approved changes pulled from GitHub.

### The individual analyst folders

These folders also contain up-to-date copies of the repository and these are the versions which are edited each time the publication is updated or each time a change to the process has to be made. Analysts should only work in their own folders on their own development branches. Once they are content that their changes are ready for the master branch, they must create a pull request on GitHub and have other analysts from the team review their changes and, if satisfied, merge them back into the master branch. It is then that the **master folder is updated.**

## The Repository

#### Files
* **.gitignore:** Any files that should not be tracked by git should be added to this file. 
* **00_setup_environment.R:** This script is edited each month to update dates.
* **01_validation.R:** This script checks each health board data submission and recodes and formats date variables, removes non-hospital locations and induvuduals aged under 18. Variables such as reason grouping, census flag and occupied bed days are derived. It requires the specialty lookup file to match on specialty description. The script outputs data query files which are fed back to health boards. The script also produces provisional census and bed day figures. This script is intended to be run more than once following resolution of queries by health boards.
* **02_read_in_and_combine_files.R:** This script reads in all health board validated data submissions and combines them to a single file.
* **03_scotland_file_processing.R:** This script recodes, formats and derives data variables (similar to the validation script). It required the national reference file location lookup file to match on location description, the specialty lookup file to match on specialty description and postcode directory lookup file to match on geography data. The script outputs a validated Scotland file.
* **04_census_and_bed_days.R:** This script creates all outputs required for the monthly publication spreadsheets for census and bed days. Data files are output for each of the different tabs required within the spreadsheets.
* **05_trend_file.R:** This script appends the latest monthly validated Scotland file to the previous trend file. The script outputs an updated trend file.

## Running the publication

### Running the code

*	In the master folder, open up x.R, highlight the entire script and run
*	Check for any errors and investigate if necessary
*	Check the output in data/output looks as it should
*	In the master folder, open up y.R, highlight the entire script and run
*	As above, check for any errors and look at the output to see if it looks as it should do

The raw output files all have the publication date in the name, so there is no need to archive as each time the process is re-run, new files will be created. The only files which do get overwritten are the publication document files, but these are copied over to the publication folder as part of the normal publication process so are already archived. 

### Updating the code

To update the publication each month, the analyst responsible for updating the scripts/running the publication should complete the following steps:

* Pull the most recent version of the master branch into their own folder 
* Create a fresh branch to make necessary changes
* Update the dates in the `00_setup_environment.R` file
* Check filepaths for lookups in `01_validation.R` and `03_scotland_file_processing.R` are still correct 
* Push new branch to GitHub
* Create pull request for another analyst to review changes
* Once changes have been approved, merge the branch into the master and delete
* If no more changes are required, pull the updated master branch into the **master folder**