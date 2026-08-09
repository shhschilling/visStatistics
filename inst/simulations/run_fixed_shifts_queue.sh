#!/bin/sh
# Wait for current rankfd queue to finish, then run fixed-shifts power simulation
while pgrep -f "rankfd_route1_typeI_h0p.R\|rankfd_route1_power_h0p.R" > /dev/null; do sleep 60; done
Rscript rankfd_route1_power_fixed_shifts.R 50000 8 > rankfd_route1_power_fixed_shifts.log 2>&1
