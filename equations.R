# equations.R
# ---------------------------
# How the model calculates results, for the Home tab, typeset with MathJax.
# The same equations, with a table of the inputs behind each symbol, are in the README;
# getCarePath(), getTxPath(), getIncidencePath(), getDALYS(), getAccessCost(), getCareCost(),
# getIncidenceCost() and getMisoProgCosts() in sub_routines.R follow them.
# ---------------------------

eq <- function(tex) div(class = "eq", style = "overflow-x:auto; overflow-y:hidden;", paste0("\\[", tex, "\\]"))

model_equations <- function() {
  withMathJax(
    p("The model is a decision tree for one birth. A woman in wealth quintile \\(q\\) (a share \\(w_q\\) of births) delivers in one of five ",
      "places \\(d\\): hospital (H), health centre (C), at home with a friend or relative (F), with a traditional birth attendant (T), ",
      "or unassisted (U). Each result is worked out for both arms \\(a\\): prenatal distribution of misoprostol (M) or none (N)."),

    h5("Place of delivery"),
    p("The survey gives, for each quintile, the share of births in a health facility \\(h_q\\), the share of facility births in hospital ",
      "\\(s_q\\), and the shares of all births that are unassisted \\(u_q\\) or with a friend or relative \\(f_q\\). These become branch ",
      "probabilities, and the programme can change the odds of a facility birth by OR (1 in the base case):"),
    eq(r"(\tilde u_q = \frac{u_q}{1 - h_q} \qquad \tilde f_q = \frac{f_q}{(1 - h_q)(1 - \tilde u_q)} \qquad h^{\text{M}}_q = \frac{\text{OR}\, h_q}{1 - h_q + \text{OR}\, h_q})"),
    eq(r"(\pi_{q,\text{H}} = h_q s_q \quad \pi_{q,\text{C}} = h_q (1 - s_q) \quad \pi_{q,\text{F}} = (1 - h_q)(1 - \tilde u_q)\, \tilde f_q \quad \pi_{q,\text{T}} = (1 - h_q)(1 - \tilde u_q)(1 - \tilde f_q) \quad \pi_{q,\text{U}} = (1 - h_q)\, \tilde u_q)"),

    h5("PPH, deaths and DALYs at each place"),
    p("At place \\(d\\) a woman receives oxytocin with probability \\(o_d\\); if not, misoprostol with probability \\(m^a_d\\). With the programme ",
      "she may take it before the birth (misuse, \\(\\mu\\)); misuse leads to an adverse event with probability \\(\\varepsilon\\), which is a uterine ",
      "rupture with probability \\(\\kappa\\) and otherwise a stillbirth. The risk of PPH is the setting's baseline \\(\\rho_d\\) times the relative risk of the uterotonic received:"),
    eq(r"(I^a_d = o_d\, \rho_d\, \text{RR}_{\text{oxy}} + (1 - o_d)\, m^a_d \Big[(1 - \mu)\, \rho_d\, \text{RR}_{\text{miso},d} + \mu\, (1 - \varepsilon)\, \rho_d\Big] + (1 - o_d)(1 - m^a_d)\, \rho_d)"),
    p("Women with PPH reach emergency obstetric care with probability \\(e_d\\) and die with probability \\(\\delta_{\\text{E}}\\) if they do and ",
      "\\(\\delta_{\\text{N}}\\) if they don't. With \\(A^a_d = (1 - o_d)\\, m^a_d\\, \\mu\\, \\varepsilon\\), the probability of an adverse event from misuse:"),
    eq(r"(\text{Deaths}^a_d = I^a_d \big[e_d\, \delta_{\text{E}} + (1 - e_d)\, \delta_{\text{N}}\big] + A^a_d \big[\kappa\, \delta_{\text{R}} + (1 - \kappa)\, \delta_{\text{S}}\big] + (1 - I^a_d - A^a_d)\, \delta_0)"),
    eq(r"(\text{DALYs}^a_d = I^a_d \Big[e_d \big(\delta_{\text{E}}\, \text{YLL}_{\text{PPH}} + \alpha_{\text{E}}\, \text{YLD}_{\text{PPH}}\big) + (1 - e_d) \big(\delta_{\text{N}}\, \text{YLL}_{\text{PPH}} + \alpha_{\text{N}}\, \text{YLD}_{\text{PPH}}\big)\Big] + A^a_d \Big[\kappa \big(\delta_{\text{R}}\, \text{YLL}_{\text{R}} + \text{YLD}_{\text{R}}\big) + (1 - \kappa) \big(\delta_{\text{S}}\, \text{YLL}_{\text{S}} + \text{YLD}_{\text{S}}\big)\Big])"),
    p("\\(\\alpha_{\\text{E}}\\) and \\(\\alpha_{\\text{N}}\\) are the probabilities of severe anaemia after PPH with and without emergency care. For each condition \\(c\\) ",
      "(PPH, uterine rupture R, stillbirth S), a death at age \\(x\\) loses the \\(L_x = \\text{LE} - x\\) years left to life expectancy LE; a survivor lives with ",
      "the condition's disability weight for its duration. The years lost are counted one at a time (whole years, then the part-year left at the end, half a year ",
      "when LE is 62.5) and discounted at rate \\(r\\) from the start of each year; the first year leaves out the time \\(t_c\\) already counted as disability. ",
      "Both are averaged over maternal ages 18, 27 and 42, weighted by the share of births at each age."),
    eq(r"(\text{YLL}_c(x) = \sum_{k=0}^{\lceil L_x \rceil - 1} \frac{\omega_k}{(1 + r)^k}, \quad \omega_0 = 1 - t_c,\ \ \omega_k = \min(1,\ L_x - k) \text{ for } k \ge 1 \qquad \text{YLD}_c = \text{DW}_c\, t_c)"),

    h5("Costs"),
    p("The cost of each birth is the cost of the delivery plus the expected cost of what follows. The governmental perspective counts the health ",
      "system's costs; the societal perspective adds travel \\(\\tau_d\\), upkeep \\(k_d\\) and time. Time is valued at GDP per capita \\(y\\) over 264 working days ",
      "(and 8 hours a day), for a normal delivery (ND), PPH treatment, a uterine rupture (R), a friend or relative's days of care at a home birth (F), ",
      "and the woman's own days recovering after a home birth (home, with no travel):"),
    eq(r"(O_{\text{ND}} = \frac{L_{\text{ND}}\, y}{264} + \frac{t_{\text{travel}}\, y}{264 \times 8} \qquad O_{\text{PPH}} = \frac{L_{\text{PPH}}\, y}{264} + \frac{t_{\text{travel}}\, y}{264 \times 8} \qquad O_{\text{R}} = \frac{L_{\text{R}}\, y}{264} + \frac{t_{\text{travel}}\, y}{264 \times 8} \qquad O_{\text{F}} = \frac{L_{\text{F}}\, y}{264} \qquad O_{\text{home}} = \frac{L_{\text{home}}\, y}{264})"),
    p("Delivery costs \\(c_{\\text{ND},d}\\) in a hospital or health centre (societal: \\(c_{\\text{ND},d} + \\tau_d + k_d + O_{\\text{ND}}\\)). Home births cost the government nothing; ",
      "societally, a birth with a traditional birth attendant costs \\(c_{\\text{TBA}} + \\tau_{\\text{T}} + O_{\\text{F}} + O_{\\text{home}}\\), with a friend or relative ",
      "\\(O_{\\text{F}} + O_{\\text{home}}\\), and unassisted \\(O_{\\text{home}}\\). With OR = 1 women deliver in the same places in both arms, so delivery costs drop out of the ",
      "incremental costs. In the misoprostol arm every woman also carries the programme's cost per woman:"),
    eq(r"(C_{\text{prog}} = \frac{c_{\text{miso}} N + N_{\text{HW}}\, c_{\text{train}}\, d_{\text{train}} + c_{\text{nurse}} \times 8 \times 5 \times N_{\text{nurse}} \times F_{\text{nurse}} + c_{\text{miso}}\, p_{\text{pack}} N}{N})"),
    p("After delivery: oxytocin \\(c_{\\text{oxy}}\\) (plus \\(c_{\\text{destroy}}\\) in the misoprostol arm, to collect unused tablets); misoprostol given by staff \\(c_{\\text{miso}}\\) without ",
      "the programme; treating PPH, a uterine rupture or a stillbirth in hospital, \\(c^p_{\\text{PPH}}\\), \\(c^p_{\\text{R}}\\), \\(c^p_{\\text{S}}\\) (societal versions add hospital travel, ",
      "upkeep and the woman's time); and \\(c_{\\text{death}}\\) for a death (0 in the base case):"),
    eq(r"(C^{a,p}_d = \text{delivery}^{a,p}_d + o_d\, c^a_{\text{oxy}} + (1 - o_d)\, m^a_d\, c^a_{\text{miso}} + I^a_d\, e_d\, c^p_{\text{PPH}} + A^a_d \big[\kappa\, c^p_{\text{R}} + (1 - \kappa)\, c^p_{\text{S}}\big] + \text{Deaths}^a_d\, c_{\text{death}})"),
    p("The PPH-cases analysis counts only the delivery and the uterotonics, since it stops at whether PPH occurs."),

    h5("Averages and ICERs"),
    eq(r"(X^a_q = \sum_d \pi^a_{q,d}\, X^a_d \qquad \bar X^a = \sum_q w_q\, X^a_q \qquad \text{ICER} = \frac{\bar C^{\text{M}} - \bar C^{\text{N}}}{\bar E^{\text{N}} - \bar E^{\text{M}}})"),
    p("Every result \\(X\\) is averaged over places of delivery within each quintile, then over quintiles. The ICER for the average woman is the difference ",
      "in average costs over the difference in average outcomes \\(E\\) (PPH cases, deaths or DALYs averted), not the average of the quintile ICERs."),

    h5("Sensitivity analyses"),
    p("The quintile shares and the maternal age groups are each drawn from a Dirichlet distribution whose parameters are the survey counts \\(\\alpha\\). ",
      "In the one-way analysis each share \\(x_i\\) is set to the 2.5% and 97.5% points of \\(\\text{Beta}(\\alpha_i, \\sum_j \\alpha_j - \\alpha_i)\\), and the others are rescaled so the ",
      "set still adds up to 1, \\(x_j' = x_j\\,(1 - x_i')/(1 - x_i)\\). In the probabilistic analysis, probabilities and disability weights are drawn from beta distributions, ",
      "relative risks from log-normal distributions, costs, travel time and life expectancy from normal distributions truncated at the low and high values, lengths of stay and ",
      "durations from uniform distributions, and the shares from the Dirichlet distributions. The discount rate varies only in the one-way analysis (0 to 5%)."),
    p(class = "info-note", "Every symbol is an input on the Parameters tab; the README on GitHub lists which input each one is.")
  )
}
