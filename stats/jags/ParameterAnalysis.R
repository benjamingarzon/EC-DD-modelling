


allIndPars.good = allIndPars %>% filter(subjID %in% include_subjects)
allIndPars.melt = melt(
  allIndPars.good,
  id.vars = c(
    "subjID",
    "Context_short",
    "Context",
    "context",
    "context_order",
    "Group"
  ),
  variable.name = "parameter",
  value.name = "value"
) %>%
  filter(parameter %in% mypars) %>% mutate(value = as.numeric(value), parameter_label = par_labels[parameter])

myplot.reliab = ggplot(allIndPars.melt, aes(x = Context, y = value, group = subjID)) + geom_line(size = 0.4) + geom_point(size = 0.6) +
  ylab('Parameter value') +
  xlab('Volatility') +
  facet_wrap(. ~ parameter_label, scales = "free", nrow = 1)

print(myplot.reliab)
#myplot = ggplot(allIndPars.melt, aes(x = context, y = value, group = context )) + geom_violin() + facet_grid( context_order ~ parameter, scales = "free" )
#print(myplot)

allIndPars.diff = merge(
  subset(allIndPars.melt, context == 'c1'),
  subset(allIndPars.melt, context == 'c2'),
  by = c('subjID', 'parameter'),
  suffixes = c('.c1', '.c2')
) %>% mutate(value = value.c2 - value.c1, c1_first = context_order.c1 == 1)

myplot = ggplot(allIndPars.diff, aes(x = 0, y = value, fill = c1_first)) + geom_violin() + facet_wrap(. ~ parameter, scales = "free")
print(myplot)

myplot = ggplot(allIndPars.diff, aes(x = 0, y = value)) + geom_violin() + geom_point(position = "jitter") + facet_wrap(. ~ parameter, scales = "free")
print(myplot)

allIndPars.sum <-
  summarySE(
    allIndPars.melt,
    measurevar = "value",
    groupvars = c(
      "Context",
      "Context_short",
      "parameter_label",
      "context_order",
      "Group"
    )
  )

# Standard error of the mean

myplot.differences = ggplot(
  allIndPars.sum,
  aes(
    x = Group,
    y = value,
    ymin = value - se,
    ymax = value + se,
    col = Context,
    group = Context
  )
) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(position = position_dodge(0.5)) +
  geom_line(position = position_dodge(0.5)) +
  ylab('Parameter value') +
  xlab('Context order')  +
  facet_wrap(. ~ parameter_label, scales = 'free', nrow = 1)
#    ggh4x::facet_grid2(context_order ~ parameter_label, scales = 'free_y', independent = 'y')

print(myplot.differences)
allIndPars.melt.vis = allIndPars.melt %>% filter((value < 2 &
                                                    parameter == 'nondectime.p') | parameter != 'nondectime.p')
myplot.differences.all = ggplot() +
  geom_line(
    data = allIndPars.melt.vis,
    aes(
      x = Context_short,
      y = value,
      group = subjID,
      col = Group
    ),
    size = 0.2,
    alpha = 0.4
  ) +
  geom_point(
    data = allIndPars.melt.vis,
    aes(x = Context_short, y = value, col = Group),
    size = 0.5,
    alpha = 0.4
  ) +
  geom_line(
    data = allIndPars.sum,
    aes(
      x = Context_short,
      y = value,
      col = Group,
      group = Group
    ),
    size = 1
  ) +
  geom_point(data = allIndPars.sum,
             aes(x = Context_short, y = value, col = Group),
             size = 1) +
  geom_errorbar(
    data = allIndPars.sum,
    aes(
      ymin = value - se,
      ymax = value + se,
      x = Context_short,
      y = value,
      col = Group
    ),
    width = 0.2,
    size = 1
  ) +
  ylab('Parameter value') +
  xlab('Volatility') +
  facet_wrap(. ~ parameter_label, scales = 'free', nrow = 1)
print(myplot.differences.all)

allIndPars.sum <-
  summarySE(allIndPars.diff,
            measurevar = "value",
            groupvars = c("parameter"))

#allIndPars.good = allIndPars %>% filter(subjID %in% include_subjects)
parameters = c("nondectime.p",
               "b.drift.intercept.p",
               "b.drift.amount.p",
               "noise.p",
               "bias.p")

tests = c("ContextLow volatility",
          "GroupLow vol. first:ContextLow volatility")

for (parameter in parameters) {
  names(tests) = c(sprintf("_%s_context_", parameter),
                   sprintf("_%s_groupxcontext_", parameter))
  model = fitmodel(sprintf("%s ~ Group * Context + (1|subjID)", parameter),
                   allIndPars.good,
                   tests)
  
  cc.main = fixef(model)["GroupLow vol. first:ContextLow volatility"]
  mm = model.matrix(model)
  parameter_pred = paste(parameter, 'pred', sep = '.')
  allIndPars.good[parameter] = allIndPars.good[parameter] - cc.main *
    mm[, "GroupLow vol. first:ContextLow volatility"]
}

print(summary(model))

allIndPars.melt = melt(
  allIndPars.good,
  id.vars = c("subjID", "Context", "Context_short"),
  variable.name = "parameter",
  value.name = "value"
) %>%
  filter(parameter %in% mypars) %>% mutate(value = as.numeric(value),
                                           parameter_label = par_labels[parameter])

allIndPars.sum <-
  summarySE(
    allIndPars.melt,
    measurevar = "value",
    groupvars = c("Context",
                  "Context_short",
                  "parameter_label")
  )

myplot.differences.agg = ggplot(data = allIndPars.sum,
                                aes(
                                  x = Context_short,
                                  y = value,
                                  ymin = value - se,
                                  ymax = value + se
                                )) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  geom_errorbar(width = 0.2,
                size = 1) +
  ylab('Parameter value') +
  xlab('Volatility') +
  facet_wrap(. ~ parameter_label, scales = 'free', nrow = 1)
print(myplot.differences.agg)
