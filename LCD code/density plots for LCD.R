ggplot(data=hia, aes(x=delta_mortality, group=sub.region, fill=sub.region)) +
  geom_density(alpha=.4)

ggplot(data=hia, aes(x=delta_mortality, group=who_region, fill=who_region)) +
  geom_density(alpha=.4)
