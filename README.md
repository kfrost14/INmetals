INmetals
========
INmetals is a repository for the "Indiana Metals" Shiny web application that displays heavy metals data from the monitors around the s
state.  The app also includes emissions data from 2005, 2008 and 2011 NEI emissions sources for heavy metals.

It requires the installation of rCharts from GitHub using the devtools package

require(devtools)
install_github('rCharts', 'ramnathv')

The app can be run from your desktop using the runGitHub() function from the Shiny package

require(shiny)
runGitHub('INmetals', 'kfrost14')