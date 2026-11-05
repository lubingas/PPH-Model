# Misoprostol for postpartum haemorrhage in Uganda: cost-effectiveness model

The decision model, written in R, and an interactive Shiny app for:

> Lubinga SJ, Atukunda EC, Wasswa-Ssalongo G, Babigumira JB. Potential cost-effectiveness of prenatal distribution of misoprostol for prevention of postpartum hemorrhage in Uganda. *PLOS ONE*. 2015;10(11):e0142550. https://doi.org/10.1371/journal.pone.0142550

The model compares prenatal distribution of misoprostol with no misoprostol, by wealth quintile, from governmental and societal perspectives, for three outcomes: PPH cases, PPH deaths and DALYs.

**Live app:** https://theejonslo.shinyapps.io/pph-misoprostol/

## What you can do in the app

- **Parameters:** browse every model input with its source, edit values with checks and a preview of the probabilistic draws, see which inputs you have changed, reset them, and download the current inputs as CSV.
- **Base case:** outcomes averted and incremental costs by wealth quintile, with ICERs for each perspective.
- **One-way sensitivity analysis:** tornado diagrams showing which parameters move each result most.
- **Probabilistic sensitivity analysis:** cost-effectiveness plane and acceptability curves from Monte Carlo draws.

Every chart has a menu for downloading it as PNG or SVG, or its data as CSV.

## How the model calculates results

The model is a decision tree for one birth. A woman in wealth quintile $q$ (a share $w_q$ of births) delivers in one of five places $d$: hospital (H), health centre (C), at home with a friend or relative (F), with a traditional birth attendant (T), or unassisted (U). What happens next depends on the uterotonic she receives, whether she has postpartum haemorrhage (PPH), and whether she reaches emergency obstetric care (EmOC). Each result is worked out for both arms $a$: prenatal distribution of misoprostol (M) or none (N).

### Place of delivery

The survey gives, for each quintile, the share of births in a health facility $h_q$, the share of facility births in hospital $s_q$, and the shares of all births that are unassisted $u_q$ or with a friend or relative $f_q$. These become branch probabilities, and the programme can change the odds of a facility birth by $\text{OR}$ (1 in the base case):

$$\tilde u_q = \frac{u_q}{1 - h_q} \qquad \tilde f_q = \frac{f_q}{(1 - h_q)(1 - \tilde u_q)} \qquad h^{\text{M}}_q = \frac{\text{OR}\, h_q}{1 - h_q + \text{OR}\, h_q}$$

$$\pi_{q,\text{H}} = h_q s_q \quad \pi_{q,\text{C}} = h_q (1 - s_q) \quad \pi_{q,\text{F}} = (1 - h_q)(1 - \tilde u_q)\, \tilde f_q \quad \pi_{q,\text{T}} = (1 - h_q)(1 - \tilde u_q)(1 - \tilde f_q) \quad \pi_{q,\text{U}} = (1 - h_q)\, \tilde u_q$$

### PPH, deaths and DALYs at each place

At place $d$ a woman receives oxytocin with probability $o_d$; if not, misoprostol with probability $m^a_d$. With the programme she may misuse it (take it before the birth) with probability $\mu$ (zero without the programme); misuse leads to an adverse event with probability $\varepsilon$, which is a uterine rupture with probability $\kappa$ and otherwise a stillbirth. The risk of PPH is the setting's baseline $\rho_d$ (facility or home) times the relative risk of the uterotonic received:

$$I^a_d = o_d\, \rho_d\, \text{RR}_{\text{oxy}} + (1 - o_d)\, m^a_d \Big[(1 - \mu)\, \rho_d\, \text{RR}_{\text{miso},d} + \mu\, (1 - \varepsilon)\, \rho_d\Big] + (1 - o_d)(1 - m^a_d)\, \rho_d$$

$I^a_d$ is the probability of PPH. Women with PPH reach EmOC with probability $e_d$ and die with probability $\delta_{\text{E}}$ if they do and $\delta_{\text{N}}$ if they don't. With $A^a_d = (1 - o_d)\, m^a_d\, \mu\, \varepsilon$, the probability of an adverse event from misuse, deaths and DALYs are:

$$\text{Deaths}^a_d = I^a_d \big[e_d\, \delta_{\text{E}} + (1 - e_d)\, \delta_{\text{N}}\big] + A^a_d \big[\kappa\, \delta_{\text{R}} + (1 - \kappa)\, \delta_{\text{S}}\big] + (1 - I^a_d - A^a_d)\, \delta_0$$

$$\text{DALYs}^a_d = I^a_d \Big[e_d \big(\delta_{\text{E}}\, \text{YLL}_{\text{PPH}} + \alpha_{\text{E}}\, \text{YLD}_{\text{PPH}}\big) + (1 - e_d) \big(\delta_{\text{N}}\, \text{YLL}_{\text{PPH}} + \alpha_{\text{N}}\, \text{YLD}_{\text{PPH}}\big)\Big] + A^a_d \Big[\kappa \big(\delta_{\text{R}}\, \text{YLL}_{\text{R}} + \text{YLD}_{\text{R}}\big) + (1 - \kappa) \big(\delta_{\text{S}}\, \text{YLL}_{\text{S}} + \text{YLD}_{\text{S}}\big)\Big]$$

where $\alpha_{\text{E}}$ and $\alpha_{\text{N}}$ are the probabilities of severe anaemia after PPH with and without EmOC. Deaths without PPH or misuse ($\delta_0$, 0 in the base case) add no DALYs. For each condition $c$ (PPH, uterine rupture R, stillbirth S), a death at age $x$ loses the $L_x = \text{LE} - x$ years left to life expectancy $\text{LE}$, and a survivor lives with the condition's disability weight for its duration. The years lost are counted one at a time (whole years, then the part-year left at the end) and discounted at rate $r$ from the start of each year; the first year leaves out the time $t_c$ already counted as disability:

$$\text{YLL}_c(x) = \sum_{k=0}^{\lceil L_x \rceil - 1} \frac{\omega_k}{(1 + r)^k}, \quad \omega_0 = 1 - t_c,\ \ \omega_k = \min(1,\ L_x - k) \text{ for } k \ge 1 \qquad \text{YLD}_c = \text{DW}_c\, t_c$$

With $\text{LE} = 62.5$ the last year counts as half a year. $\text{YLL}_c$ and $\text{YLD}_c$ are averaged over three maternal ages (18, 27 and 42), weighted by the share of births in each age group.

### Costs

The cost of each birth is the cost of the delivery at place $d$ plus the expected cost of what follows. From the governmental perspective these are the health system's costs; the societal perspective adds travel, upkeep and time. Time is valued at GDP per capita $y$ over 264 working days a year (and 8 hours a day):

$$O_{\text{ND}} = \frac{L_{\text{ND}}\, y}{264} + \frac{t_{\text{travel}}\, y}{264 \times 8} \qquad O_{\text{PPH}} = \frac{L_{\text{PPH}}\, y}{264} + \frac{t_{\text{travel}}\, y}{264 \times 8} \qquad O_{\text{R}} = \frac{L_{\text{R}}\, y}{264} + \frac{t_{\text{travel}}\, y}{264 \times 8} \qquad O_{\text{F}} = \frac{L_{\text{F}}\, y}{264} \qquad O_{\text{home}} = \frac{L_{\text{home}}\, y}{264}$$

$O_{\text{F}}$ is the time of the friend or relative who looks after the woman at a home birth, and $O_{\text{home}}$ is the woman's own time recovering at home (with no travel).

**Delivery.** With $c_{\text{ND},d}$ the cost of a normal delivery, $\tau_d$ the travel cost and $k_d$ the upkeep cost of a stay:

| Place | Governmental | Societal |
|---|---|---|
| Hospital | $c_{\text{ND,H}}$ | $c_{\text{ND,H}} + \tau_{\text{H}} + k_{\text{H}} + O_{\text{ND}}$ |
| Health centre | $c_{\text{ND,C}}$ | $c_{\text{ND,C}} + \tau_{\text{C}} + k_{\text{C}} + O_{\text{ND}}$ |
| Traditional birth attendant | 0 | $c_{\text{TBA}} + \tau_{\text{T}} + O_{\text{F}} + O_{\text{home}}$ |
| Friend or relative | 0 | $O_{\text{F}} + O_{\text{home}}$ |
| Unassisted | 0 | $O_{\text{home}}$ |

With OR = 1 women deliver in the same places in both arms, so the delivery costs are the same in both arms and drop out of the incremental costs.

In the misoprostol arm every woman also carries the programme's cost per woman, for $N$ women: the tablets, training $N_{\text{HW}}$ health workers for $d_{\text{train}}$ days at $c_{\text{train}}$ a day, nurses teaching mothers, and packaging (a share $p_{\text{pack}}$ of the tablet price):

$$C_{\text{prog}} = \frac{c_{\text{miso}} N + N_{\text{HW}}\, c_{\text{train}}\, d_{\text{train}} + c_{\text{nurse}} \times 8 \times 5 \times N_{\text{nurse}} \times F_{\text{nurse}} + c_{\text{miso}}\, p_{\text{pack}} N}{N}$$

**After delivery.** Oxytocin costs $c_{\text{oxy}}$, plus $c_{\text{destroy}}$ in the misoprostol arm to collect the unused tablets. Without the programme, misoprostol given by staff costs $c_{\text{miso}}$ (with the programme it is in $C_{\text{prog}}$). Treating PPH at EmOC, a uterine rupture or a stillbirth (all in hospital) costs $c^p_{\text{PPH}}$, $c^p_{\text{R}}$ and $c^p_{\text{S}}$ from perspective $p$; the societal versions add the hospital's travel and upkeep costs and the woman's time ($O_{\text{PPH}}$, $O_{\text{R}}$ and $O_{\text{ND}}$). A death costs $c_{\text{death}}$ (0 in the base case):

$$C^{a,p}_d = \text{delivery}^{a,p}_d + o_d\, c^a_{\text{oxy}} + (1 - o_d)\, m^a_d\, c^a_{\text{miso}} + I^a_d\, e_d\, c^p_{\text{PPH}} + A^a_d \big[\kappa\, c^p_{\text{R}} + (1 - \kappa)\, c^p_{\text{S}}\big] + \text{Deaths}^a_d\, c_{\text{death}}$$

The PPH-cases analysis counts only the delivery and the uterotonics, since it stops at whether PPH occurs.

### Averages and ICERs

Every result $X$ (PPH cases, deaths, DALYs, costs) is averaged over places of delivery within each quintile, and then over quintiles:

$$X^a_q = \sum_d \pi^a_{q,d}\, X^a_d \qquad \bar X^a = \sum_q w_q\, X^a_q \qquad \text{ICER} = \frac{\bar C^{\text{M}} - \bar C^{\text{N}}}{\bar E^{\text{N}} - \bar E^{\text{M}}}$$

The ICER for the average woman is the difference in average costs over the difference in average outcomes ($E$ is PPH cases, deaths or DALYs, so the denominator is what the programme averts), not the average of the quintile ICERs.

### Sensitivity analyses

The quintile shares and the maternal age groups are each drawn from a Dirichlet distribution whose parameters are the survey counts $\alpha$. In the one-way analysis each share $x_i$ is set to the 2.5% and 97.5% points of its own distribution, $\text{Beta}(\alpha_i, \sum_j \alpha_j - \alpha_i)$, and the others are rescaled so the set still adds up to 1: $x_j' = x_j\, (1 - x_i') / (1 - x_i)$. In the probabilistic analysis, probabilities and disability weights are drawn from beta distributions, relative risks from log-normal distributions, costs, travel time and life expectancy from normal distributions truncated at the low and high values, lengths of stay and durations from uniform distributions between the low and high values, and the shares from the Dirichlet distributions. The discount rate varies only in the one-way analysis (0 to 5%).

| Symbol | Input in `pph_data_input.xlsx` (or `global.R`) | Base case |
|---|---|---|
| $h_q$, $s_q$, $u_q$, $f_q$ | `pHfDel…Q`, `pHospDel…Q`, `pUnassDel…Q`, `pFriendRelDel…Q` for each quintile | Uganda DHS 2011 |
| $w_q$; OR | `pLowQuintile` … `pHighQuintile`; `orHFDelivery` | survey counts; 1 |
| $o_d$; $m^a_d$; $e_d$ | `pOxy…`; `pMisoAccess…MisoPolicy` (M), `pMisoAccess…OxyPolicy` (N); `pAccessEmOC…` | by place |
| $\rho_d$ | `pHospPPH` (facility), `pHomePPH` (home) | 0.078, 0.122 |
| $\text{RR}_{\text{oxy}}$; $\text{RR}_{\text{miso},d}$ | `rPPHOxyPlacebo`; `rPPHMisoHF` (facility), `rPPHMisoOxy` (home) | 0.53; 0.84, 0.53 |
| $\mu$, $\varepsilon$, $\kappa$ | `pMisuseMiso`, `pNoMisoAE`, `pUterineRuptureMiso` | 0.0029, 0.014, 0.7 |
| $\delta_{\text{E}}, \delta_{\text{N}}, \delta_{\text{R}}, \delta_{\text{S}}, \delta_0$ | `pDieEmOC`, `pDieNoEmOC`, `pDieUterineRupture`, `pDieStillBirth`, `pDieNoPPH` | 0.062, 0.124, 0.118, 0, 0 |
| $\alpha_{\text{E}}$, $\alpha_{\text{N}}$ | `pSevereAnemiaEmOC`, `pSevereAnemianoEmOC` | 0.17, 0.34 |
| $\text{DW}_c$, $t_c$ | `disabilitywtPPH`, `durationPPH`; `…Rupture`, `durationUterineRupture`; `…StillBirth`, `durationStillBirth` | 0.149, 1 month; 0.326, 3 months; 0.123, 1 month |
| LE; $r$ | `lifeexpectancy`; `cDiscountCosts` | 62.5 years (59.8 to 65.1); 3% (0 to 5%) |
| $y$; $t_{\text{travel}}$ | `cProductivity`; `averageTravelTime` | $547; 0.75 hours |
| $L_{\text{ND}}, L_{\text{PPH}}, L_{\text{R}}, L_{\text{F}}, L_{\text{home}}$ | `averageLOSNormDel`, `averageLOSPPH`, `averageLOSUterineRupture`, `avgFriendTime`, `averageLOSHome` | 3, 7, 14, 3 and 3 days |
| $c_{\text{ND},d}$; $c_{\text{TBA}}$ | `cNormalDeliveryHosp`, `cNormalDeliveryHC`; `cNormDelHomeTBA` | $15.47, $15.23; $11.77 |
| $\tau_d$, $k_d$ | `cTravelHospital`, `cTravelHC`, `cTravelTBA`; `cUpkeepHospital`, `cUpkeepHC` | $3.83, $1.39, $1.39; $8.08, $2.78 |
| $c_{\text{PPH}}$, $c_{\text{R}}$, $c_{\text{S}}$ | `cPPHManagementHosp`, `cUterineRupture`, `cVaginalStillBirth` | $64.70, $118.88, $15.47 |
| $c_{\text{oxy}}$, $c_{\text{destroy}}$, $c_{\text{miso}}$; $c_{\text{death}}$ | `cOxytocin`, `cDestruction`, `cMisoprostol`; `cMortality` | $0.044, $0.05, $0.36; 0 |
| $N$, $N_{\text{HW}}$, $c_{\text{train}}$, $d_{\text{train}}$ | `nWomen`, `nHW`, `cTrain`, `daysTraining` in `global.R` | 10,000 women; 200 health workers at $10 a day for 5 days |
| $c_{\text{nurse}}$, $N_{\text{nurse}}$, $F_{\text{nurse}}$; $p_{\text{pack}}$ | `cNurse` (hourly); `nNurse`, `nurseFTE` in `global.R`; `cPackage` | $1.08; 50, 20; 0.5 |

With these inputs the programme costs $5.84 per woman. The functions `getCarePath()`, `getTxPath()`, `getIncidencePath()`, `getDALYS()`, `getAccessCost()`, `getCareCost()`, `getIncidenceCost()` and `getMisoProgCosts()` in `sub_routines.R` follow these equations; the tree itself is in `analysis/PPHTree.json`.

### Changes from the published model

| Change | Effect on the base case |
|---|---|
| Life expectancy and the discount rate are read from the input spreadsheet, so both now vary in the sensitivity analyses. The published model counted years of life lost to age 63½ and did not use the spreadsheet's life expectancy of 62.5 years. | DALYs averted per woman 0.0327 → 0.0322; US$ per DALY averted $139.75 → $142.08 (governmental) and $123.95 → $126.02 (societal) |
| Home births count the woman's own time at home: added to births with a friend, relative or traditional birth attendant (which counted only the helper's time), and replacing a hospital stay and travel time for unassisted births. | Societal cost per woman up $2.15 in both arms; incremental costs and ICERs unchanged, because the programme doesn't change where women deliver (OR = 1) |

## Run it locally

In R (4.3 or later):

```r
install.packages(c("MCMCpack", "data.table", "magrittr", "tidyverse", "readxl",
                   "truncnorm", "gt", "pbapply", "highcharter", "shiny", "shinyjs",
                   "progressr", "waiter", "reactable", "bs4Dash"))
shiny::runApp()
```

Run `shiny::runApp()` from this folder, or open `PPH-Model.Rproj` in RStudio and click **Run App**.

## Files

| File | Contents |
|------|----------|
| `global.R` | Packages, chart styling, model constants; reads the input spreadsheet |
| `sub_routines.R` | The decision model, sensitivity analyses and chart functions |
| `equations.R` | The equations on the Home tab (the same as in this README) |
| `server.R`, `ui.R` | The Shiny app |
| `pph_data_input.xlsx` | Model inputs: base-case values, ranges, distributions and sources |
| `analysis/meta-analysis.R` | Random-effects meta-analyses of PPH incidence and of the relative risk of PPH with misoprostol versus placebo, from the trial data in `analysis/` (needs the `metafor` package) |
| `analysis/PPHTree.json` | The structure of the decision tree |

The 2022 version of the model and app, before this update, is tagged [`v2022`](../../tree/v2022).

## Licence

The code is released under the MIT License (see `LICENSE`). The published article, and the decision-tree figure the app shows from it, are © the authors under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
