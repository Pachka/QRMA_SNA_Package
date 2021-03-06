% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphSNA.R
\name{graphSNA}
\alias{graphSNA}
\title{Visualization of SNA parameters}
\usage{
graphSNA(x, indegree, outdegree, betweeness, NOM)
}
\arguments{
\item{x}{the data.frame containing the names of localities, mobility parameters and GPS coordinates. \cr
FR: data.frame contenant les noms des localités, les paramètres de mobilité et les coordonnées GPS.}

\item{indegree}{the column containing the "indegree" parameters, will be plotted as the x -coordinate.}

\item{outdegree}{the column containing the "outdegree" parameters, will be plotted as the x -coordinate.}

\item{betweeness}{the column containing the "betweeness" parameters, will define the size of the dots.}

\item{NOM}{the column containing the names of the localities.}
}
\description{
This function is used to produce dynamic graphs
illustrating the role of different localities in the network
based on mobility parameters generated by the Network Analysis (SNA).
}
\details{
Cette fonction sert à produire des graphiques dynamiques
illustrant le rôle des différentes localités dans le réseau en fonction des
paramètres de mobilité issus de l'analyse de réseau (SNA)
}
\keyword{ggplot}
\keyword{network}
\keyword{plot}
\keyword{sna}
