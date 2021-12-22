# Haupert & al Methods in Ecology and Evolution (2022)

This repository contains all the functions in 'R' that were used to process the data and create figures of the publication 
[(Haupert et. al. 2022, MEE)](https://www.to.be.defined.fr)

The code is available for reproducability.

This repository is archived on Zenodo:

[![DOI](https://zenodo.org/badge/DOI/XXXX/zenodo.XXXXXX.svg)](https://doi.org/10.5281/zenodo.3530203)

If the code, even partially, is used for other purpose please cite the paper `Haupert S., Sèbe F., Sueur J. How to determine the acoustic detection 
distance of autonomous recorder units in terrestrial environments? Methods in Ecology and Evolution, 2022`

## Setup and usage

Download the `.zip` from Github (click on `code` then `Download Zip`) and extract all folders without changing the name of the folders neither rearrange the folder
and sub-folders.

Then, use your favorite R environment (e.g. RStudio). The scripts are ready to be used. They will install the required libraries if they are not already installed
in your environment. Here is the list of library that are requested :
* seewave and tuneR
* plotly
* robutslrm
* tidyverse, ggplot2, dplyr

The R scripts to reproduce figures/analyses from our paper are:
 
* `figure_1a_compute_Leq.R` generates a figure with the equivalent sound level (Leq) of the white noise and the ambient sound measured at different distances. The white noise is propagated in a neotropical rainforest (French guiana). The receptor is a SongMeter 4 (SM4) from Wildlife acoustics.   
*
*

## Google Colab

Google Colab is a very convenient way to test a code written in Python or R without having to install anything on your computer. Everything is run iin the cloud, directly on computers in Google. The required libraries are installed on the fly on the distant computers.

For an interactive way to test our code, we provide most of the R scripts from this repository as Notebook ready to be used on Google Colab. Some adjustment had 
to be done to be able to run the scripts as some R libraries such as Plotly for R don't work properly on Google Colab. In such a case, we mixed R and Python. R 
is used for data processing while Python is used to build and display the figures.

## Integration of the propagation functions into libraries

For convenience, we added the propagation functions into the library [seewave]https://rug.mnhn.fr/seewave/) (R language) and into the package [scikit-maad](https://scikit-maad.github.io/) (Python language)

