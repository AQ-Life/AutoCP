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
library(sysfonts)
library(Hmisc)

rm(list = ls())

source(file = "01_Settings/Path.R", local = T, encoding = "UTF-8")


sysfonts::font_add(family = "KT", regular = "simkai.ttf")
sysfonts::font_add(family = "arialbd", regular = "arialbd.ttf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)
