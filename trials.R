# What probability 20 sites, that 6 enroll ...
# without having a single screen fail?


##################################################
# SMALL, EASY EXAMPLE PROBLEM WHERE WE CAN VERIFY CORRECT ANSWERS
##################################################
# if we have:
#   3 trial sites
#   2 patients per site
#   0.50 probability of screen failure
#   what is probability that >= 2 sites will each fail both patients?


# patient possibilities per site:
# site 1    site 2    site 3
# 00        00        00
# 00        00        01
# 00        00        10
# 00        00        11


# across sites, 64 choose possibilities
# first 16, with site 1 at 00
# these 16 possibilities below include 7 cases with at least two 00
# the other 48 possibilities include 3 cases total with at least two 00
# in total, 7 + 3 = 10 cases with at least two 00
# therefore, probability that >= 2 sites will each fail both patients = 10/64 = 0.15625
# this answer also calculated with 'pbinom'
# I think ">= 2" translates to "size = 3 minus x = 1 equals 2"

patient_n <- 2
screen_fail_prob <- 0.50
px0 <- pbinom(0, size = patient_n, prob = screen_fail_prob)
px0

site_n <- 3
atleast <- 2
x <- site_n - atleast
1 - pbinom(x, size = site_n, prob = px0)
pbinom(x, size = site_n, prob = px0, lower.tail = FALSE)
# > 1 - pbinom(x, size = site_n, prob = px0)
# [1] 0.15625
# > pbinom(x, size = site_n, prob = px0, lower.tail = FALSE)
# [1] 0.15625



# site 1    site 2    site 3

# 00        00        00
# 00        00        01
# 00        00        10
# 00        00        11

# 00        01        00
# 00        01        01
# 00        01        10
# 00        01        11

# 00        10        00
# 00        10        01
# 00        10        10
# 00        10        11

# 00        11        00
# 00        11        01
# 00        11        10
# 00        11        11


##################################################
# WRITE FUNCTION TO HANDLE PROBLEM AND CHECK INTUITIONS
##################################################

prob_no_failures_across_sites <- function(
    site_n, patient_n, screen_fail_prob, atleast) {
  px0 <- pbinom(0, size = patient_n, prob = screen_fail_prob)
  x <- atleast - 1
  prob <- pbinom(x, size = site_n, prob = px0, lower.tail = FALSE)
  return(prob)
}

# same problem as above, but changing the "at least" criterion
# if we have:
#   3 trial sites
#   2 patients per site
#   0.50 probability of screen failure

site_n <- 3
patient_n <- 2
screen_fail_prob <- 0.50

# what is probability that >= 1 site will fail both patients?
# correct answer is 37/64 = 0.578125
atleast <- 1
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.578125

# what is probability that >= 2 sites will each fail both patients?
# correct answer is 10/64 = 0.15625
atleast <- 2
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.15625

# what is probability that >= 3 sites will each fail both patients?
# correct answer is 1/64 = 0.015625
atleast <- 3
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.015625


##################################################
# check intuition:
# what if 'screen_fail_prob' decreases?
# probability that at least given number of sites each fails both patients should increase
site_n <- 3
patient_n <- 2

atleast <- 1
screen_fail_prob <- 0.50
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.578125
screen_fail_prob <- 0.25
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.9162598
# INCREASE, CORRECT


atleast <- 2
screen_fail_prob <- 0.50
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.15625
screen_fail_prob <- 0.25
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.5932617
# INCREASE, CORRECT


atleast <- 3
screen_fail_prob <- 0.50
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.015625
screen_fail_prob <- 0.25
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.1779785
# INCREASE, CORRECT


##################################################
# check intuition:
# what if 'patient_n' increases?
# that will decrease probability that each site will have no failures
site_n <- 3
screen_fail_prob <- 0.50

atleast <- 1
patient_n <- 2
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.578125
patient_n <- 4
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.1760254
# DECREASE, CORRECT


atleast <- 2
patient_n <- 2
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.15625
patient_n <- 4
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.01123047
# DECREASE, CORRECT


atleast <- 3
patient_n <- 2
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.015625
patient_n <- 4
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.0002441406
# DECREASE, CORRECT


##################################################
# check intuition:
# what if 'site_n' increases?
# that will increase probability that at least a given number of sites will have no screening failures
patient_n <- 2
screen_fail_prob <- 0.50

atleast <- 1
site_n <- 3
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.578125
site_n <- 6
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.8220215
# INCREASE, CORRECT


atleast <- 2
site_n <- 3
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.15625
site_n <- 6
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.4660645
# INCREASE, CORRECT


atleast <- 3
site_n <- 3
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.015625
site_n <- 6
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.1694336
# INCREASE, CORRECT


##################################################
# for the same problem as above, but for
#   20 trial sites
#   n patients per site
#   0.50 probability of screen failure
#   what is probability that >= 6 sites will each fail all its patients?

site_n <- 20
screen_fail_prob <- 0.50
atleast <- 6


patient_n <- 2
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.9087396

patient_n <- 4
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 0.1259243

patient_n <- 8
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 6.464835e-05

patient_n <- 16
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 4.049306e-12

patient_n <- 32
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 1.438882e-26

patient_n <- 48
prob_no_failures_across_sites(site_n, patient_n, screen_fail_prob, atleast)
# [1] 5.111937e-41
