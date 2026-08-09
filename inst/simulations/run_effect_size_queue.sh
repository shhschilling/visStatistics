#!/bin/sh
# Runs the two effect-size-scaling power grids, one after the other, after the
# currently running jobs have finished. Both scripts checkpoint every completed
# cell to their CSV and skip finished cells on restart, so each is wrapped in a
# retry loop: if the parent loses its session (the SIGPIPE/sendMaster failure
# seen twice already), it resumes instead of starting over.
#
#   1. route1_power_omega_fixed.R  -- omega^2 held constant across all designs
#   2. route1_power_etaH_fixed.R   -- eta_H^2 held constant instead
#
# 150 cells each (5 designs x 6 sizes x 5 panels).
cd "$(dirname "$0")"
TOTAL=150

wait_for_free() {
  while pgrep -f "rankfd_route1_power_fixed_shifts.R|rankfd_route1_power_h0p.R" > /dev/null; do
    sleep 60
  done
}

run_until_done() {
  script="$1"; csv="$2"; log="$3"
  while true; do
    Rscript "$script" 50000 8 >> "$log" 2>&1
    if [ -f "$csv" ]; then
      rows=$(($(wc -l < "$csv") - 1))
    else
      rows=0
    fi
    [ "$rows" -ge "$TOTAL" ] && { echo "COMPLETE: $csv ($rows cells)" >> "$log"; break; }
    echo "restarting $script at $rows/$TOTAL cells" >> "$log"
    sleep 5
  done
}

wait_for_free
run_until_done route1_power_omega_fixed.R \
               fleishman_4groups_power_omega_fixed_B50000.csv \
               route1_power_omega_fixed.log

run_until_done route1_power_etaH_fixed.R \
               fleishman_4groups_power_etaH_fixed_B50000.csv \
               route1_power_etaH_fixed.log
