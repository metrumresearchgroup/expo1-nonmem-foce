# Purpose: Plots for poster


library(pmplots)
library(pmtables)
library(tidyverse)
library(patchwork)
library(yspec)
library(mrggsave)
library(magrittr)

options(mrg.script = "poster-graphics.R", 
        mrggsave.dir = here::here("deliv/poster-acop-2022/images"),
        "mrggsave.dev" = "png")


data <- pmplots::pmplots_data_obs()


## DV - PRED
p1 <- dv_pred(data)
p2 <- dv_ipred(data)

dvp = p1+p2

mrggsave(dvp, stem = "dv-pred-ipred-mrg", height=3, width=5)
ggsave(dvp, filename = "dv-pred-ipred.png", 
       path = here::here("deliv/poster-acop-2022/images"),
       device = "png", height=2, width=5)



## Residuals

p3 <- cwres_time(data)
p4 <- res_pred(data)
p5 <- npde_cat(data, x = "STUDYc//Study")
p6 <- wres_hist(data)

resP = (p3 + p4) / (p5 + p6)
mrggsave(resP, stem = "res-mrg", height=5, width=5)
ggsave(resP, filename = "res.png", 
       path = here::here("deliv/poster-acop-2022/images"),
       device = "png", height=5, width=5)


## Data disposition table


tab <- data %>%
  filter(EVID==0) %>% 
  pt_data_inventory(by = c("Study" = "STUDYc")) %>%
  stable()

# If recreated, need to manually trim table - better options? 
st2doc(tab,
       preview = FALSE,
       output_dir = here::here("deliv/poster-acop-2022/images"),
       output_file = "pk-data-sum.pdf",)
dat# st_as_image(tab)

