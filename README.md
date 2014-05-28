INmetals
========
INmetals is a repository for the "Metals Data Explorer" Shiny web application that displays Indiana heavy metals data from monitors around the state.  The app also includes emissions data from 2005, 2008 and 2011 NEI emissions sources for heavy metals.  Right now this app primarily serves as a proof of concept for a web application that could serve metals monitoring data.

It requires the installation of rCharts from GitHub using the devtools package

    require(devtools)
    install_github('rCharts', 'ramnathv')

The app can be run from your desktop using the runGitHub() function from the Shiny package

    require(shiny)
    runGitHub('INmetals', 'kfrost14')
