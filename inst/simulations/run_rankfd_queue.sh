#!/bin/sh
# 1. H0F power arm (streams 450-599), 2. H0p Type I arm (streams 600-749).
while pgrep -f rankfd_route1_power.R > /dev/null; do sleep 60; done
[ -f rankfd_route1_power_B50000.csv ] || Rscript rankfd_route1_power.R 50000 8 > rankfd_route1_power.log 2>&1
Rscript rankfd_route1_typeI_h0p.R 50000 8 > rankfd_route1_typeI_h0p.log 2>&1
