# `REtage`

<div id="badges">
  <a href="https://www.insee.fr/fr/statistiques/6793990">
    <img src="https://img.shields.io/badge/See the Working Paper-red?style=for-the-badge&logo=firefox&logoColor=white" alt="Twitter Badge"/>
  </a>
</div>

Lino Galiana

Experimental <img height="18" width="18" src="https://cdn.simpleicons.org/r/00ccff99" /> package
to model inheritance using survey data. 

See [Galiana and Wilner (2023)](https://www.insee.fr/fr/statistiques/6793990) and [Galiana, Guichaoua and Wilner (2022)](https://www.cairn.info/revue-retraite-et-societe-2022-2-page-17.htm) for more details on the framework and the dataset used. 

## Installation

```r
remotes::install_github("linogaliana/retage")
```

## Ordered Multinomial Models

Most of the materials presented here comes from `oglmx` vignette that is used behind the stage

Available implementations of ordered discrete models : 

* `MASS::polr`
* `oglmx::oglmx`

We denote $`y`$ an observed random variable that takes $`J`$ possible values ($`\{0,...,J-1\}`$). Responses are ordered. We assume there exists a latent model such that
```math
y^* = x\beta + \sigma \epsilon
```
$x$ represents observad covariates (sometimes excluding constant term, see below), $`\beta`$ is a $`K \times 1`$ vector of parameters, $`\epsilon`$ a zero mean error term with scaled variance $`\sigma`$. We denote $`\alpha_1<alpha__{J-1}`$ thresholds. By setting $`\alpha_0 = -\inf`$ and $`\alpha_J = -\inf`$ ,

```math
\mathbb{P}(y = j) = \mathbb{P}\left( \epsilon \leq \frac{\alpha_{j+1}-x\beta}{\sigma} \right) -  \mathbb{P}\left( \epsilon \leq \frac{\alpha_{j}-x\beta}{\sigma} \right) = F\left( \epsilon \leq \frac{\alpha_{j+1}-x\beta}{\sigma} \right) -  F\left( \epsilon \leq \frac{\alpha_{j}-x\beta}{\sigma} \right)
```

with $F$ cumulative function for the error term $`\epsilon`$. Noise variance can be modelled as a process

```math
\mathbb{P}(y = j) = g(z \delta)
```

where $`g`$ should be a positive definite function of some covariates $`z`$ and $`\delta`$ a scale location parameter. This approach allows for both homoskedasticity and heteroskedasticity. Likelihood writes down

```math
\mathcal{l}(\beta, \delta, \alpha) = \sum_{i=1}^n \sum_{j=0}^{J-1} \mathbf{1}_{y_i = i} \log \bigg( F\left( \epsilon \leq \frac{\alpha_{j+1}-x_i\beta}{g(z_i \delta)}\right) -  F\left( \epsilon \leq \frac{\alpha_{j}-x_i\beta}{g(z_i \delta)} \right) \bigg)
```


Parameter Assumptions for Popular Ordered Dependent Variable Models (from `oglmx` vignette):

| Model  | No. Outcomes  |  Error distribution   | Constant in mean equation   | Error variance ($\sigma$)   | Threshold parameters  |
|--------|---------------|-----------------------|-----------------------------|-----------------------------|-----------------------|
|Probit  | 2             | Standard Normal       | Included                    | Set = 1                     | Set = 0               |
|Logit   | 2             | Standard Logistic     | Included                    | Set = 1                     | Set = 0               |
|Ordered Probit  | >2    | Standard Normal       | Excluded                    | Set = 1                     | Estimated             |
|Ordered Logit   | >2    | Standard Logistic     | Excluded                    | Set = 1                     | Estimated             |
|Interval regression | >2| User choice           | Included                    | Estimated                   | Specified             |

