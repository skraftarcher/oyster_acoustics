# explore long-duration acoustic indices -----

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
lp("ggsidekick")

# run if not done before
# source("scripts/organize-acoustic-indices.R") 

# set these for a specific machine
# where to find the compiled dataframes?

output_parent_directory <- "odata/"
figure_directory <- "figures/"

# choose which data set/sample to compile
site_file_name <- "AcousticIndices_Calc_Sum_22_5674"
site_file_namec5678 <- "AcousticIndices_Calc_Sum_22_5678"
site_file_namec5680 <- "AcousticIndices_Calc_Sum_22_5680"
site_file_namel5674 <- "AcousticIndices_LUM_Sum_22_5674"
site_file_namel5678 <- "AcousticIndices_LUM_Sum_22_5678"
site_file_namel5680 <- "AcousticIndices_LUM_Sum_22_5680"


### bring in data

ld <- readRDS(paste0(output_parent_directory, "towsey-indices-", site_file_name, ".rds"))%>%
  mutate(kHz = round(freq_bin_num * 11025 / 256) / 1000)
ldc5678 <- readRDS(paste0(output_parent_directory, "towsey-indices-", site_file_namec5678, ".rds"))%>%
  mutate(kHz = round(freq_bin_num * 11025 / 256) / 1000)
ldc5680 <- readRDS(paste0(output_parent_directory, "towsey-indices-", site_file_namec5680, ".rds"))%>%
  mutate(kHz = round(freq_bin_num * 11025 / 256) / 1000)
ldl5674 <- readRDS(paste0(output_parent_directory, "towsey-indices-", site_file_namel5674, ".rds"))%>%
  mutate(kHz = round(freq_bin_num * 11025 / 256) / 1000)
ldl5678 <- readRDS(paste0(output_parent_directory, "towsey-indices-", site_file_namel5678, ".rds"))%>%
  mutate(kHz = round(freq_bin_num * 11025 / 256) / 1000)
ldl5680 <- readRDS(paste0(output_parent_directory, "towsey-indices-", site_file_namel5680, ".rds"))%>%
  mutate(kHz = round(freq_bin_num * 11025 / 256) / 1000)

plot_single_index <- function(data,
                              index) {
  data %>%
    filter(index_type == {{ index }}) %>%
    # filter pulse at start of each file and high and low bands that distract
    filter(minintofile != 0 & kHz > 0.1 & kHz < 10.5) %>%
    ggplot(aes(plot_time, kHz, colour = score, fill = score)) +
    geom_tile() +
    #geom_raster() +
    scale_colour_viridis_c(option = "turbo") +
    scale_fill_viridis_c(option = "turbo") +
    coord_cartesian(expand = FALSE) +
    theme_sleek() +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "black")
    ) +
    ggtitle(paste0(data$site[1]), subtitle = index)
}


# explore individual indices
ggsave(plot=plot_single_index(ld, "ACI"),
       "figures/calc5674_aci.png",
       width=7,height=5)
ggsave(plot=plot_single_index(ld, "BGN"),"figures/calc5674_bgn.png")
ggsave(plot=plot_single_index(ld, "ENT"),"figures/calc5674_ent.png")
ggsave(plot=plot_single_index(ld, "EVN"),"figures/calc5674_evn.png")
ggsave(plot=plot_single_index(ld, "PMN"),"figures/calc5674_pmn.png")
ggsave(plot=plot_single_index(ld, "SPT"),"figures/calc5674_spt.png")



false_colour_plot <- function(indices,
                              data) {
  .d <- data %>%
    filter(minintofile != 0)%>%
    #filter(file_dt %in% unique(file_dt)[c(9)])%>%
    #filter(minintofile != 0 & kHz > 0.1 & kHz < 10.5)%>%
    filter(index_type %in% indices) %>%
    group_by(index_type) %>% mutate(score=abs(score),score = (score-min(score))/(max(score)-min(score))) %>%
    ungroup() %>%
    pivot_wider(names_from = "index_type", values_from = "score")
  # browser()
  # .d[!complete.cases(.d),] #Returns zero rows, no pixel is lacking any data

  r1 <- range(.d[[indices[1]]])
  r2 <- range(.d[[indices[2]]])
  r3 <- range(.d[[indices[3]]])
  # # Values must be btw 0-1
  
  .d$r <- .d[[indices[1]]] 
  .d$g <- .d[[indices[2]]] 
  .d$b <- .d[[indices[3]]] 

  ggplot(
    data = .d,#filter(.d, minintofile != 0 & kHz > 0.1 & kHz < 10.5),
    aes(x=datetime, y=kHz,fill = rgb(r, g, b, maxColorValue = 1))
  ) +
    geom_tile() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_x_datetime(breaks = scales::pretty_breaks(n = 12)) +
    coord_cartesian(expand = FALSE) +
    theme_sleek() +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "black")
    ) 
}

ggsave(plot=false_colour_plot(indices = c("ACI","EVN","ENT"),
                              data=ld),
       "figures/calc_5674_aci_env_ent.png",
       width=7,height=5)

ggsave(plot=false_colour_plot(indices = c("ACI","EVN","ENT"),
                              data=ldc5678),
       "figures/calc_5678_aci_env_ent.png",
       width=7,height=5)

ggsave(plot=false_colour_plot(indices = c("ACI","EVN","ENT"),
                              data=ldc5680),
       "figures/calc_5680_aci_env_ent.png",
       width=7,height=5)

ggsave(plot=false_colour_plot(indices = c("ACI","EVN","ENT"),
                              data=ldl5674),
       "figures/lum_5674_aci_env_ent.png",
       width=7,height=5)

ggsave(plot=false_colour_plot(indices = c("ACI","EVN","ENT"),
                              data=ldl5678),
       "figures/lum_5678_aci_env_ent.png",
       width=7,height=5)

ggsave(plot=false_colour_plot(indices = c("ACI","EVN","ENT"),
                              data=ldl5680),
       "figures/lum_5680_aci_env_ent.png",
       width=7,height=5)

