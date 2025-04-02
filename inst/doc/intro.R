## ----include = FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(maxEff)
library(groupedHyperframe)
library(survival)
library(rpart)

## ----echo = FALSE, results = 'asis'-------------------------------------------
c(
  '', 'Forward pipe operator', '`?base::pipeOp` introduced in `R` 4.1.0', 
  '`abs`', 'Absolute value', '`base::abs`',
  '`coxph`', 'Cox model', '`survival::coxph`',
  '`CRAN`, `R`', 'The Comprehensive R Archive Network', 'https://cran.r-project.org',
  '`factor`', 'Factor, or categorical variable', '`base::factor`',
  '`function`', '`R` function', '``base::`function` ``',
  '`groupedHyperframe`', 'Grouped hyper data frame', ' `groupedHyperframe::as.groupedHyperframe`',
  '`head`', 'First parts of an object', '`utils::head`; `utils:::head.default`',
  '`hypercolumns`, `hyperframe`', '(Hyper columns of) hyper data frame', '`spatstat.geom::hyperframe`',
  '`labels`', 'Labels from object', '`base::labels`; `maxEff::labels.node1`',
  '`levels`', 'Levels of a `factor`', '`base::levels`',
  '`listof`', 'List of objects', '`stats::listof`',
  '`logistic`', 'Logistic regression model', '`stats::glm(., family = binomial(\'logit\'))`',
  '`matrix`', 'Matrix', '`base::matrix`',
  '`partition`', 'Stratified partition', '`maxEff::statusPartition`, `caret::createDataPartition`', 
  '`PFS`', 'Progression/recurrence free survival', 'https://en.wikipedia.org/wiki/Progression-free_survival',
  '`predict`', 'Model prediction', '`stats::predict`; `maxEff::predict.add_num`; `maxEff::predict.add_dummy`',
  '`quantile`', 'Quantile', '`stats::quantile`',
  '`rpart`', 'Recursive partitioning and regression trees', '`rpart::rpart`',
  '`S3`, `generic`, `methods`', '`S3` object oriented system',  '`base::UseMethod`; `utils::methods`; `utils::getS3method`; https://adv-r.hadley.nz/s3.html',
  '`sort_by`', 'Sort an object by some criterion', '`base::sort_by`; `maxEff::sort_by.add_`',
  '`subset`', 'Subsets of object by conditions', '`base::subset`; `maxEff::subset.add_dummy`',
  '`Surv`', 'Survival object', '`survival::Surv`',
  '`update`', 'Update and re-fit a model call', '`stats::update`'
) |>
  matrix(nrow = 3L, dimnames = list(c('Term / Abbreviation', 'Description', 'Reference'), NULL)) |>
  t.default() |>
  as.data.frame.matrix() |> 
  kable()

## -----------------------------------------------------------------------------
data(cu.summary, package = 'rpart')
(r = rpart(Price ~ Mileage, data = cu.summary, cp = .Machine$double.eps, maxdepth = 1L))

## -----------------------------------------------------------------------------
(foo = r |> node1())

## -----------------------------------------------------------------------------
set.seed(125); rnorm(6, mean = 24.5) |> foo()

## -----------------------------------------------------------------------------
foo |> get_cutoff()
foo |> labels()

## -----------------------------------------------------------------------------
y = (survival::capacitor) |> 
  with(expr = Surv(time, status))
set.seed(15); id = y |>
  statusPartition(times = 1L, p = .5)
table(y[id[[1L]], 2L]) / table(y[,2L]) # balanced by status
set.seed(15); id0 = y |>
  caret::createDataPartition(times = 1L, p = .5)
table(y[id0[[1L]], 2L]) / table(y[,2L]) # *not* balanced by status

## -----------------------------------------------------------------------------
data(Ki67, package = 'groupedHyperframe')
Ki67

## -----------------------------------------------------------------------------
s = Ki67 |>
  aggregate_quantile(by = ~ patientID, probs = seq.int(from = .01, to = .99, by = .01))

## -----------------------------------------------------------------------------
set.seed(234); id = sample.int(n = nrow(s), size = nrow(s)*.8) |> sort.int()
s0 = s[id, , drop = FALSE] # training set
s1 = s[-id, , drop = FALSE] # test set

## -----------------------------------------------------------------------------
summary(m <- coxph(PFS ~ Tstage, data = s0))

## -----------------------------------------------------------------------------
set.seed(14837); m1 = m |>
  add_num(x = ~ logKi67.quantile, mc.cores = 1L) |>
  sort_by(y = abs(effsize)) |>
  head(n = 2L)
m1

## -----------------------------------------------------------------------------
m1[1L] |> predict(newdata = s1)

## -----------------------------------------------------------------------------
set.seed(14837); m2 = m |>
  add_dummy(x = ~ logKi67.quantile, mc.cores = 1L) |>
  subset(subset = p1 > .05 & p1 < .95) |> 
  sort_by(y = abs(effsize)) |>
  head(n = 2L)
m2

## -----------------------------------------------------------------------------
m2[1L] |> predict(newdata = s1)

## -----------------------------------------------------------------------------
set.seed(14837); m3 = m |> 
  add_dummy_partition(~ logKi67.quantile, times = 20L, mc.cores = 1L) |>
  subset(subset = p1 > .15 & p1 < .85) |>
  sort_by(y = abs(effsize), decreasing = TRUE) |>
  head(n = 2L)
m3

## -----------------------------------------------------------------------------
m3[1L] |> predict(newdata = s1)

