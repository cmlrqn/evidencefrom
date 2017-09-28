# evidencefrom

## Purpose
1. To create a dataset of AER, ECTA, JPE, QJE, REStud articles using publicly available RePEc metadata.
2. To use the dataset to explore the economics bibliography.

## Inputs and Output
The metadata come from [RePEc](ftp://ftp.repec.org/opt/ReDIF/RePEc/), where they are held in [ReDIF](http://openlib.org/acmes/root/docu/redif_1.html) files. The code extracts information from these text files by first scraping their content through rvest and then making use of regularities in the text to separate article information. 

For each article, the generated dataset should contain (where available):
    ..* A list of authors
    * Title
    * Abstract
    * Volume, Issue, Pages
    * Year
    * Journal
    * Handle URL link 

## Limitations
The limitations of the resulting dataset are mainly associated to limitations of the metadata. 
For the American Economic Review, metadata are only available for article from 1969 onwards. 
More generally, the files can contain orthographic errors that translate into errors in the dataset. 

## Motivation 
This is a small project I worked on to teach myself a bit of R, get into web scraping and try to provide an alternative to the odd person who might be interested in economics article data but doesn't have the opportunity to bulk download EconLit metadata.
