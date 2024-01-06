library(ggplot2)
library(patchwork)
library(RColorBrewer)



label_parsed <- function(x) {
  parsed_param <- gsub("alpha", "\U03B1", x)
  parsed_param <- gsub("rho", "\U03C1", parsed_param)
  parsed_param <- gsub("omega", "\U03C9", parsed_param)
  
  parsed_param
  
}


label_parsed_mean <- function(x) {
  parsed_param <- gsub("alpha", "\U03BC\U03B1", x)
  parsed_param <- gsub("rho", "\U03BC\U03C1", parsed_param)
  parsed_param <- gsub("omega", "\U03BC\U03C9", parsed_param)
  
  parsed_param
  
}


label_parsed_delta <- function(x) {
  parsed_param <- gsub("alpha", "\U0394\U03B1", x)
  parsed_param <- gsub("rho", "\U0394\U03C1", parsed_param)
  parsed_param <- gsub("omega", "\U0394\U03C9", parsed_param)

  parsed_param

}



MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}



sim_sub_plot_rtpois <- function(c, filename="sim_sub_plot_rtpois.png"){
  
  c_avg <- colMeans(c)
  
  c_sd <- apply(c, 2, sd)
  
  plot <- ggplot() +
    geom_errorbar(aes(x = 1:12, ymin = c_avg - c_sd, ymax = c_avg + c_sd), width = 0.2) +
    geom_point(aes(x = 1:12, y = c_avg), size=2, shape="square") +
    geom_line(aes(x = 1:12, y = c_avg), lwd=0.3) +
    theme_bw() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text = element_text(size = 12, face = "bold")) +
    ylab("Average contribution") +
    xlab("Trial") +
    ylim(-1, 3) +
    scale_x_continuous(breaks = 1:12)

  ggsave(paste0("plots/", filename), width = 6, height = 4)
  print(paste0("[INFO]: Saved plots/", filename))
}



sim_sub_plot <- function(c, filename="sim_sub_plot.png"){
  
  c_avg <- colMeans(c)
  
  plot <- ggplot() +
    geom_point(aes(x = 1:12, y = c_avg), size=2, shape="square") +
    geom_line(aes(x = 1:12, y = c_avg), lwd=0.3) +
    theme_bw() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text = element_text(size = 12, face = "bold")) +
    ylab("Average contribution") +
    xlab("Trial") +
    ylim(-2, 6) +
    scale_x_continuous(breaks = 1:12)
  
  ggsave(paste0("plots/", filename), width = 6, height = 4)
  print(paste0("[INFO]: Saved plots/", filename))
}



sub_recov_plot <- function(df, filename="sub_recov.png"){
  
  ggplot(df, aes(true, recov)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "black", lwd = 0.2) +
    theme_bw() +
    facet_wrap(~ label_parsed(parameter), scales = "free") +
    theme(strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "True", y = "Recovered")
  
  # save
  ggsave(paste0("plots/", filename), width = 12, height = 4.3)
  print(paste0("[INFO]: Saved plots/",filename))
}



mean_recov_plot <- function(df, filename="mean_recov.png"){
  
  ggplot(df, aes(true, recov)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "black", lwd = 0.2) +
    theme_bw() +
    facet_wrap(~ label_parsed_mean(parameter), scales = "free") +
    theme(strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "True", y = "Recovered")
  
  # save
  ggsave(paste0("plots/", filename), width = 12, height = 4.3)
  print(paste0("[INFO]: Saved plots/",filename))
}



diff_recov_plot <- function(dfs, filename="diff_recov_plot.png") {
  
  gg_l = lapply(dfs, function(x) {
    
    ggplot(x, aes(true_diff, recov_diff)) +
      geom_point(aes(color = true_mid)) +
      geom_abline(intercept = 0, slope = 1, color = "black", lwd = 0.2) +
      theme_bw() +
      facet_wrap(~ label_parsed_delta(parameter), ncol = 1) +
      theme(strip.background = element_rect(fill = "transparent", colour = NA),
            strip.text = element_text(size = 18, face = "bold"),
            axis.title = element_text(size = 8),
            legend.position = c(0.78, 0.11),
            legend.direction = "horizontal",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(color = "True mid-value", x = "True", y = "Recovered") +
      scale_color_continuous(guide = guide_colorbar(title.position = "top",
                                                    title.hjust = 0.5,
                                                    title.vjust = 0.5,
                                                    title.theme = element_text(size = 8),
                                                    barwidth = 5,
                                                    barheight = 0.5,
                                                    label.theme = element_text(size = 7, angle = -45, hjust = 0, vjust = 1)))
    
  })
  
  plot <- wrap_plots(gg_l[c(1,3,2)], ncol = 3, nrow = 1)
  
  ggsave(paste0("plots/", filename), width = 12, height = 4.5)
  print(paste0("[INFO]: Saved plots/",filename))
}



group_diffs_plot <- function(df, filename="sim_group_diffs.png"){
  ggplot() +
  geom_linerange(data = group_df_for_plot,
                 aes(xmin = first, xmax = second, 
                     y = comparison,
                     color=sign)) +
  ylab('') +
  xlab('') +
  theme_bw() +
  facet_wrap(~label_parsed_delta(parameter), ncol=3, scales="free_x") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(size = 12, face = "bold"),
        ) +
  scale_y_discrete(breaks = NULL) +
  labs(color="Sign")
  
  ggsave(paste0("plots/", filename), width = 7, height = 3)
  print(paste0("[INFO]: Saved plots/",filename))
}



post_diff_plot <- function(df, filename = "posterior_dens_diffs.png"){
  
  ggplot(df, aes(x=samples)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~label_parsed_delta(parameter), nrow=3, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(x = "", y = "Density")
  
  # save
  ggsave(paste0("plots/", filename), width = 7, height = 6)
  print(paste0("[INFO]: Saved plots/",filename))
}



post_mean_plot <- function(df, filename = "posterior_dens_means.png"){
  
  my_palette <- brewer.pal(3, "Paired") # 3 to avoid warning, only 2 are needed
  my_palette <- my_palette[c(2,1)]
  
  ggplot(df, aes(x=samples, color=group)) +
  geom_density() +
  facet_wrap(~label_parsed_mean(parameter), nrow=3, scales = "free") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(x = "", y = "Density", color="Group") +
  scale_color_manual(values = my_palette)
  
  ggsave(paste0("plots/", filename), width = 7, height = 6)
  print(paste0("[INFO]: Saved plots/", filename))
}