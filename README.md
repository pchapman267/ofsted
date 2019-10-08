# Ofsted MI All Data

## Background

This project combines all Ofsted Management Information publications that are posted [here](https://www.gov.uk/government/statistical-data-sets/monthly-management-information-ofsteds-school-inspections-outcomes) on Gov.UK.

The aim of this is to support analysis where we are interested in Ofsted at a point in, or over, time.

## Summary

Running the run.R will do the following:

1. Scan the Gov.UK page and extract links to all files
2. Download all of the files to a temporary directory
3. Read in and clean the datasets based on a predefined set of naming conventions (names have changed over time).
4. Stack all of the datasets together
5. Subset the dataset so that we have one line per URN and Inspection ID.
6. Output the clean dataset ofsted_all.csv to the outputs folder

If there are no changes to the naming conventions for fields used in the files then running this script will work over time. If there are changes it will fail and the names should be added to the code for recoding.

## Travis + GitHub

This project is set up with Travis CI such that the code can routinely be run on open source machines at speed and ouptuts automatically published to GitHub. You can find published data files under the releases tab. 

The code is run daily, and if new data is found, a new release is created. 
