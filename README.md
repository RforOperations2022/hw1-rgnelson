# Homework 1

## Title: US Chronic Disease Interactive Summary
### Author: Bobby Nelson
### Date: 2/4/2022

## About

This tool is an interactive Shiny application that displays crude mortality rates in the United States split across wider disease indicator categories like asthma and cardiovascular disease. The data for this application is the CDC's published data on United States Chronic Disease Indicators (CDI) found [here](https://catalog.data.gov/dataset/u-s-chronic-disease-indicators-cdi).

This dataset provides a detailed breakdown of U.S. chronic disease indicators 
by state. There is a lot of extra detail in the data that was simplified
before loading into the Shiny application. You can read more about the data
[here](./data/rr6401.pdf).

The user can select the disease category of interest and view an updated heat map and bar chart of mortality rates in the United States (at the state level). The user can select any year from 2010 to 2017 to view the data. The user can optionally choose a specific state on which to get additional statistics. When a state is selected, a bar plot for the given year shows mortality rates of the sub-categories of the overall disease category, and a line-plot shows the time trend of the category's total mortality from 2010-2017. 

In addition to the charts, the user can also interact with tabular displays of both the state and United States data. These tables can be downloaded to the user's local machine.

## Access

View the application at: https://rgnelson.shinyapps.io/ChronicDisease/.
