# viadee Datenvisualisierung (R)

Idee: Wir generalisieren typische Visualisierungen, aus den Data-Mining-POCs und stellen diese als R-Package zur Verfügung.

Zunächst sind nur ggplot-Hintergründe im viadee-CI enthalten.

# Installation
Eine Installation von GitHub ist, wie folgt möglich:

	devtools::install_github("viadee/viadeeDataViz")

Alternativ kann sie hier als bspw. ![viadeedataviz_0.1.0.tar.gz](viadeedataviz_0.1.0.tar.gz)  heruntergeladen und per RStudio aus der Datei installiert werden.

# Build
roxygen2::roxygenise()
RStudio > Build > Build Source Package
