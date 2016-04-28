\name{ParallelTree}
\alias{ParallelTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ParallelTree
}
\description{
 Provides two functions: Group_function() and Parallel_Tree(). Group_function() applies a given function (e.g.,mean()) to input variable(s) by group across levels. Has additional data management options.
    Parallel_Tree() uses 'ggplot2' to create a parallel coordinate plots (technically a facsimile of parallel coordinate plots in a Cartesian coordinate system).
    Used in combination these functions can create parallel tree plots, a variant of parallel coordinate plots, which are useful for visualizing multilevel data.
}

\details{
This package is primarily designed to produce Parallel Tree plots. Plots of multilevel data showing grand and group means, as well as raw scores. The Parallel_Tree() function is flexible enough to produce typical Parallel Coordinate Plots as well. Parallel Tree plots allow for Parallel Coordinate plots to be nested within each level of the Parallel Tree plot (i.e., between the means of two levels). With more than 2000 level 1 observations users should be aware that overplotting will likely be an issue, and users may consider selecting a random subsample, setting the alpha for geom_path() to less than 1, or use facets to prevent overplotting.
}

\references{

Wegman, E. J., & Luo, Q. (1997). High dimensional clustering using parallel coordinates and the grand tour (pp. 93-101). Springer Berlin Heidelberg.

Wegman, E. J. (1990). Hyperdimensional data analysis using parallel coordinates. Journal of the American Statistical Association, 85(411), 664-675.

}
\author{
Patrick O'Keefe
}
