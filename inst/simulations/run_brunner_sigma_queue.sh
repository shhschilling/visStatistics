#!/bin/sh
# 1. Type I grid on Brunner's sigma (150 cells)
# 2. One-point power grid, delta = 1, on Brunner's sigma (125 cells)
# The gradient power grid already exists and is NOT rerun.
# Both scripts checkpoint every cell and skip finished cells on restart.
cd "$(dirname "$0")"
B=50000; CORES=8; SET=brunner

run_until_done() {
  script="$1"; csv="$2"; total="$3"; log="$4"; shift 4
  while true; do
    Rscript "$script" "$B" "$CORES" "$SET" "$@" >> "$log" 2>&1
    if [ -f "$csv" ]; then rows=$(($(wc -l < "$csv") - 1)); else rows=0; fi
    [ "$rows" -ge "$total" ] && { echo "COMPLETE: $csv ($rows cells)" >> "$log"; break; }
    echo "restarting $script at $rows/$total" >> "$log"
    sleep 5
  done
}

run_until_done route1_typeI_design_variants.R \
  "route1_typeI_design_${SET}_B${B}.csv" 150 route1_typeI_design_variants.log

run_until_done route1_power_design_variants.R \
  "fleishman_4groups_power_design_${SET}_onepoint_d100_B${B}.csv" 125 \
  route1_power_onepoint.log TRUE onepoint 1
