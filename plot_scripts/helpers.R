
# Converts annualised costs to total benefits
fcn_normalise_benefits <- function(cost_annuity, area = 750000, perha = F, 
                                   annualise = F, discount_rate = 0.035, years = 30,
                                   billions = T) {
  if (annualise) {
    # Data arrives in annualised format
    benefits <- -cost_annuity
  } else {
    benefits <- -cost_annuity * (1-(1/(1+discount_rate)^years))/discount_rate
    if (billions) {
      benefits <- benefits / 1e9
    }
  }
  if (perha) {
    benefits <- benefits/area
  }
  benefits
}