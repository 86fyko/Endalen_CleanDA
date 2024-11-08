library(lme4)

# Fit the mixed-effects model
m_mixed_i <- lmer(plot_biomass ~ TREATMENT+ SUBSITE + (1 | YEAR), data = dryCAS)
summary(m_mixed_i)

m_mixed_3way <- lmer(plot_biomass ~ YEAR*SUBSITE*TREATMENT + (SUBSITE|YEAR),data = dryCAS)#annes suggestion of 3 way interaction
summary(m_mixed_3way)
m_mixed_3way_dry <- lmer(plot_biomass ~ as.Date(YEAR)*TREATMENT + (1|YEAR),data = subset(dryCAS, SUBSITE=="DRY-L"))#annes suggestion of 3 way interaction
summary(m_mixed_3way_dry)
m_mixed_3way_cas <- lmer(plot_biomass ~ as.Date(YEAR)*TREATMENT + (1|YEAR),data = subset(dryCAS, SUBSITE=="CAS-L"))#annes suggestion of 3 way interaction
summary(m_mixed_3way_cas)

m_mixed_2way <- lmer(plot_biomass ~ YEAR*SUBSITE +YEAR*TREATMENT + (SUBSITE|YEAR),data = dryCAS)#annes suggestion of simpler model
summary(m_mixed_2way)
m_mixed_t0 <- lmer(plot_biomass ~ YEAR + SUBSITE+ (SUBSITE|YEAR),data = dryCAS)
AIC(m_mixed_3way, m_mixed_2way,m_mixed_t0)

anova(m_mixed_3way,m_mixed, m_mixed_t,m_mixed_t0,m_mixed_null, m_mixed_i )
anova(m_mixed_t,m_mixed_t0)



# Summarize the model
summary(m_mixed)

####mixed effect plotting####
# Predictions and confidence intervals for the mixed-effects model
newdat <- expand.grid(
  YEAR = unique(dryCAS$YEAR),       # Specify the levels for YEAR
  SUBSITE = unique(dryCAS$SUBSITE), # Specify the levels for SUBSITE
  TREATMENT = c("CTL", "OTC")       # Specify the levels for TREATMENT
)

# Generate predictions
newdat$fit <- predict(m_mixed_3way, newdata = newdat, re.form = NA)

# Calculate standard errors for predictions
pvar1 <- diag(mm %*% tcrossprod(vcov(m_mixed_3way), mm))
se <- sqrt(pvar1)

# Adjust for random effects variance
tvar1 <- pvar1 + VarCorr(m_mixed_3way)$YEAR[1]

# Set the multiplier for confidence intervals
cmult <- 1.96  # 95% confidence intervals

# Calculate confidence intervals
newdat$plo <- newdat$fit - cmult * se
newdat$phi <- newdat$fit + cmult * se
newdat$tlo <- newdat$fit - cmult * sqrt(tvar1)
newdat$thi <- newdat$fit + cmult * sqrt(tvar1)

# Plot
g0 <- ggplot(newdat, aes(x = YEAR, y = fit, color = SUBSITE, shape=TREATMENT)) +
  geom_point() +
  geom_line()+
#  geom_ribbon(aes(ymin = plo, ymax = phi), alpha=0.1) +
  labs(title = "CI based on fixed-effects uncertainty ONLY")+
  theme_bw()

g0
g0 + geom_ribbon(aes(ymin = tlo, ymax = thi),alpha=0.1)+
  labs(title="CI based on FE uncertainty + RE variance")
