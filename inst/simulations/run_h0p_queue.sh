#!/bin/sh
while pgrep -f rankfd_route1_typeI_h0p.R > /dev/null; do sleep 60; done
[ -f rankfd_route1_typeI_h0p_B50000.csv ] || exit 1
Rscript rankfd_route1_power_h0p.R 50000 8 > rankfd_route1_power_h0p.log 2>&1
