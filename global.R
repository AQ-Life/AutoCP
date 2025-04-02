library(shiny)
library(DT)
library(haven)
library(tidyverse)
library(shinyWidgets)
library(readxl)
library(rlang)
library(rtables)
library(tern)
library(bslib)
library(showtext)
library(sqldf)
library(Hmisc)

rm(list = ls())

source(file = "01_Settings/Path.R", local = T, encoding = "UTF-8")


# font_add(family = "KT", regular = "02_Settings/simkai.ttf")
# font_add(family = "arialbd", regular = "01_Settings/arialbd.ttf")
showtext_auto()
showtext::showtext_opts(dpi = 300)

font.families()
