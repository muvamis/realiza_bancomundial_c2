# Version 1

## Comments made by Andres

It is too slow because the app runs all the code since the begining 

## Changes made by Andres

### Architecture
I created a folder for each module in R/. The idea is that making changes is easier for everyone. And that a change in a specific module, does not affect any other part of the app.

### Data

The data is loaded only once at the begining of the app. Instead of loading it every time that a tab is active. 

# Description of the architecture

The dashboard has been coded using a modular approach. Which means that every page/tab of the dashboard is coded in its own module.

To simplify the coding, all the code is divided in sub-directories:

## 0.Utils-clean-data

All the scripts that perform a data cleaning task. Within this folder is the **vectors-actividades** where the vector of the activities of FNM and SGR are defined.

## 2. Themes

All the scripts to define themes of the charts

## 3. Plots

All the scripts to produce plots.

# Modules

Each module is coded separately to avoid replication and to ease the process of making changes

## PARTICIPANTES

The code to create PARTICIPANTES SGR, FNM, etc. All the tabs that show the number of participantes to a given activity.







