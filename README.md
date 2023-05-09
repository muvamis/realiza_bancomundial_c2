# Version 2.0

This app analyses the data collected for the Realiza program and creates a dashboard to disseminate the attendance rates of the participants. The main audience of the dashboard is the World Bank.


# Architecture

# data

it contains all the data used in this dashboard. The data is created by the [Realiza dashboard](https://dashboard.muvamoz.org/dashboard/realiza). The code to create the data is stored in this [Repo](https://github.com/araupontones/realiza)


# R

Contains all the code and functions that are used for the dashboard. The different sections of the dashboard are contained within the **4.modules folder**. The idea is that making changes is easier for everyone. And that a change in a specific module, does not affect any other part of the app.

The dashboard has been coded using a modular approach. Which means that every page/tab of the dashboard is coded in its own module.

To simplify the coding, all the code is divided in sub-directories:

## 0.Utils-clean-data

All the scripts that perform a data cleaning task. Within this folder is the **vectors-actividades** where the vector of the activities of FNM and SGR are defined.

## 1.Utils-app

Functions to perform repetitive tasks in the app that are independent from data cleaning

## 2. Themes

All the scripts to define themes of the charts.

## 3. Plots

All the scripts to produce plots.

## Modules

Each module is coded separately to avoid replication and to ease the process of making changes

### PARTICIPANTES

The code to create PARTICIPANTES SGR, FNM, etc. All the tabs that show the number of participantes to a given activity.

### SESSOES OBRIGATORIAS 

Code to create sessoes obrigatorias.



# Preguntas de Andres

## Sessoes obrigatorias

### Filtro periodo

Quales sao as metas de cada perido?






