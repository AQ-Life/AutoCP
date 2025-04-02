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


# sysfonts::font_add(family = "KT", regular = "simkai.ttf")
# sysfonts::font_add(family = "arialbd", regular = "arialbd.ttf")
# font_add_google("Yuji Syuku", "KT")
# showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

font_add(family = "KT", regular = "01_Settings/simkai.ttf")
font_add(family = "arialbd", regular = "01_Settings/arialbd.ttf")

