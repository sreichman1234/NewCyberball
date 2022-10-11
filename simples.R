# Code to analyse simple effects

# first, deviate the status variables to test effects within levels of status

excluder_all.df <- mutate(excluder_all.df, 
                      d2_x1statushi = x1status - 1, #high status
                      d2_x2statushi = x2status - 1,
                      d2_x1statuslo = x1status + 1, #low status
                      d2_x2statuslo = x2status - 1,
                      d2_x1statuscon = x1status, #control
                      d2_x2statuscon = x2status + 2
                    )

#second, deviate the context variables to test effects within levels of context

excluder_all.df <- mutate(excluder_all.df, 
                      d2_x3context_t = x3context - 1, #task
                      d2_x3context_s = x3context + 1  #social
                    )

#original model
excluder_modelexclusion <- lmer(prop ~ xtime * (x1status + x2status) * x3context + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_modelexclusion)
anova(excluder_modelexclusion)

#test effects within status - this will be like looking within panels of high vs medium vs low at the 2-way interactions in the panels
excluder_simples_hs <- lmer(prop ~ xtime * (d2_x1statushi + d2_x2statushi) * x3context + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_hs)
anova(excluder_simples_hs)

excluder_simples_ls <- lmer(prop ~ xtime * (d2_x1statuslo + d2_x2statuslo) * x3context + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_ls)
anova(excluder_simples_ls)

excluder_simples_con <- lmer(prop ~ xtime * (d2_x1statuscon + d2_x2statuscon) * x3context + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_con)
anova(excluder_simples_con)


#test effects within context - this will tell us useful stuff about how the status-exclusion patterns show up within task vs social separately
#equivalent to testing one-way ANOVA of status with only solid lines or with only the dotted lines
excluder_simples_social <- lmer(prop ~ xtime * (x1status + x2status) * d2_x3context_s + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_social)
anova(excluder_simples_social)

excluder_simples_task <- lmer(prop ~ xtime * (x1status + x2status) * d2_x3context_t + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_task)
anova(excluder_simples_task)

#test individual slopes - within intersections of power and context levels

#high status - social (dotted line in RH panel)
excluder_simples_highsoc <- lmer(prop ~ xtime * (d2_x1statushi + d2_x2statushi) * d2_x3context_s + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_highsoc)
anova(excluder_simples_highsoc)

#high status - task (solid line in RH panel)
excluder_simples_hightask <- lmer(prop ~ xtime * (d2_x1statushi + d2_x2statushi) * d2_x3context_t + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_hightask)
anova(excluder_simples_hightask)

#low status - social (dotted line in LH panel)
excluder_simples_lowsoc <- lmer(prop ~ xtime * (d2_x1statuslo + d2_x2statuslo) * d2_x3context_s + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_lowsoc)
anova(excluder_simples_lowsoc)

#low status - task (solid line in LH panel)
excluder_simples_lowtask <- lmer(prop ~ xtime * (d2_x1statuslo + d2_x2statuslo) * d2_x3context_t + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_lowtask)
anova(excluder_simples_lowtask)

#control status - social (dotted line in centre panel)
excluder_simples_consoc <- lmer(prop ~ xtime * (d2_x1statuscon + d2_x2statuscon) * d2_x3context_s + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_consoc)
anova(excluder_simples_consoc)

#control status - task (solid line in centre panel)
excluder_simples_contask <- lmer(prop ~ xtime * (d2_x1statuscon + d2_x2statuscon) * d2_x3context_t + (xtime | prolific_id), data = excluder_all.df)
summary(excluder_simples_contask)
anova(excluder_simples_contask)


######Revised to read in Sarah's comprehensive dataframe and work with that

full.df <- read.csv("qualtricsMCQ.csv")
qualtricsMCQ <- full.df


