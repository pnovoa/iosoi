
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iosoi (*I*dentifier *O*f *S*olution *O*f *I*nterest)

<!-- badges: start -->
<!-- badges: end -->

The goal of *iosoi* is to provide an [R](https://www.r-project.org)
implementation of the multi-criteria decision making approach proposed
in:

- Torres, M., Pelta, D. A., Lamata, M. T., & Yager, R. R. (2021). An
  approach to identify solutions of interest from multi and
  many-objective optimization problems. *Neural Computing &
  Applications, 33*(7), 2471–2481.
  [doi:10.1007/s00521-020-05140-x](https://doi.org/10.1007/s00521-020-05140-x)

It also includes alternative approaches for assessing the solutions from
a geometric perspective. Formally, iosoi helps to solve the following
MCDM problem:

Given a set of m solutions (alternatives) with evaluations on n
criteria, and an order of preference among these criteria, **order the
solutions according to their overall scores**. These overall scores are
obtained through the linear combination of the evaluations and the
weights of the criteria.

## Installation

You can install the development version of *iosoi* from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pnovoa/iosoi")
```

## Example

Consider a decision problem in which the objective is to identify a set
of solutions of interest based on their overall performance over a given
set of criteria. As input data we have a set of $m$ solutions
(alternatives) and their individual evaluations on each of the $n$
criteria. Intuitively, this information can be arranged in the form of a
matrix $E$ whose entries $e_{ij}$ correspond to the numerical evaluation
that solution $i$ has in criterion $j$. Additionally, we have an order
of preference of the criteria by the decision maker. Without loss of
generality, it will be assumed that this order is decreasing as a
function of the order of the criteria in the matrix $E$. That is, let
$\lambda_j$ the $j$-th criterion, then
$\lambda_1 \succeq \lambda_2 ... \succeq \lambda_n$. Numerically, this
information can be translated into variables representing non-negative
weights associated with the criteria and with the additional condition
of adding up to $1$.

The following example is based on a problem of $5$ solutions, $3$
criteria and evaluation matrix:

$$ E_{5 \times 3} =
\left(\begin{array}{cc} 
5 & 2 & 3 \\
4 & 4 & 3 \\
1 & 1 & 5 \\
4 & 2 & 3 \\
4 & 3 & 2 \\
\end{array}\right)
$$

The preference order provided by the decision-maker is (as we mentioned
before) in decreasing order regarding the order followed by the criteria
in matrix $E_{ij}$.

Using the `iosoi` package, one can perform the steps of the Torres et
al. (2021) approach to obtain global evaluations of the solutions and be
able to select those of greatest interest to the decision maker. In what
follows we will perform this task from both, the possibility approach by
Torres et al. (2021) and a geometric approach based on the notion of the
volume. In the case of possibility approach we will rely on the
*neutral* attitude for computing the superiority degree of the solutions
against the reference one. For both approaches we set a threshold of
$0$, which means that only those solutions with their corresponding
assessments greater than $0$ will appear in the output.

``` r
library(iosoi)

# Evaluation matrix
E <- matrix(c(5, 2, 3, 4, 4, 3, 1, 1, 5, 4, 2, 3, 4, 3, 2), byrow = TRUE, nrow = 5)
E
#>      [,1] [,2] [,3]
#> [1,]    5    2    3
#> [2,]    4    4    3
#> [3,]    1    1    5
#> [4,]    4    2    3
#> [5,]    4    3    2

# Identifying SOIs from the possibility approach of Torres et al (2021).
E %>% poss_identify_sois(by = "neutral", threshold = 0.0)
#>    C1 C2 C3 VE_C1 VE_C2    VE_C3       LB UB REF   neutral
#> S1  5  2  3     5   3.5 3.333333 3.333333  5   0 0.6666667
#> S2  4  4  3     4   4.0 3.666667 3.666667  4   1 0.5000000
#> S4  4  2  3     4   3.0 3.000000 3.000000  4   0 0.2500000
#> S5  4  3  2     4   3.5 3.000000 3.000000  4   0 0.2500000

# Identifying SOIs from the geometric approach based on the volume
E %>% geom_identify_sois(by = "poss_volume", threshold = 0.0)
#>    C1 C2 C3 VE_C1 VE_C2    VE_C3       LB       UB REF poss_volume
#> S1  5  2  3     5   3.5 3.333333 3.333333 5.000000   0   0.5035461
#> S2  4  4  3     4   4.0 3.666667 3.666667 4.000000   1   0.5000000
#> S3  1  1  5     1   1.0 2.333333 1.000000 2.333333   0   0.2708333
#> S4  4  2  3     4   3.0 3.000000 3.000000 4.000000   0   0.4615385
#> S5  4  3  2     4   3.5 3.000000 3.000000 4.000000   0   0.4736842
```

As noted, both outputs contain not only the original evaluation matrix,
but also other columns related to intermediate steps of the method of
Torres et al (2021). For instance, those columns with prefix `VE_`
corresponds to the linear scoring of each solution at the extreme points
of the feasible region induced by the preference order of the criteria.
Columns `LB` and `UB` contain the lower and upper bounds of the values
obtained in the `VE_` columns. The Boolean column `REF` identify
(with 1) the reference solution, that is, the one used to assess the
rest of solutions. Finally, the last column (ie. `neutral` or
`poss_volume`) contains the overall assessment of the solutions. Note
that only those with values greater than $0$ were included. In the first
case, only solutions `S1`, `S2`, `S4` and `S5` were selected, while in
the second case all were included.

To understand why solution S3 is left out in the approach of Torres et
al.  (2021) `iosoi` allows to plot the intervals (ranges of possible
scores) of each solution. That is, in order to visually check the degree
of overlapping of the solutions in relation to the reference one. The
code then performs the analysis again but relaxes the threshold to $-1$
to include all solutions in the final output. In this way the intervals
of all the solutions will be plotted.

``` r
E %>% 
  poss_identify_sois(
    by = "neutral", 
    threshold = -1.0) %>%
  plot_intervals()
```

<img src="man/figures/README-plots-1.png" width="70%" style="display: block; margin: auto;" />

As expected, `S3` is excluded because its interval is clearly far to the
left relative to `S2` (the reference solution highlighted with interval
in red). The rest, and especially `S1` have certain levels of overlap
with `S2`.

We can also plot any computed assessment indicator through a bar plot.
For example, if we consider `poss_volume`, the following code show how
to plot this indicator.

``` r
E %>% 
  geom_identify_sois(by = "poss_volume", threshold = 0.0) %>%
  plot_assessment(by = "poss_volume",
                  plot_title = "Normalized volume",
                  ylabel = "Normalized volume"
                  )
```

<img src="man/figures/README-plots2-1.png" width="70%" style="display: block; margin: auto;" />
