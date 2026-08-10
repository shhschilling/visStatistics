#!/bin/sh
# Brunner-SD design variant: parametric arm first, then the rank arm (RK, ATS,
# ATSp). Both checkpoint every completed cell and skip finished cells on
# restart, so each is wrapped in a retry loop: if the parent loses its session
# (the SIGPIPE/sendMaster failure seen before), it resumes instead of starting
# over. 90 cells each -- the two homoscedastic designs are not rerun.
cd "$(dirname "$0")"
TOTAL=90
SET=brunner
B=50000
CORES=8

run_until_done() {
  script="$1"; csv="$2"; log="$3"
  while true; do
    Rscript "$script" "$B" "$CORES" "$SET" >> "$log" 2>&1
    if [ -f "$csv" ]; then rows=$(($(wc -l < "$csv") - 1)); else rows=0; fi
    [ "$rows" -ge "$TOTAL" ] && { echo "COMPLETE: $csv ($rows cells)" >> "$log"; break; }
    echo "restarting $script at $rows/$TOTAL cells" >> "$log"
    sleep 5
  done
}

run_until_done route1_power_design_variants.R \
               "fleishman_4groups_power_design_${SET}_B${B}.csv" \
               route1_power_design_variants.log

run_until_done rankfd_route1_power_design_variants.R \
               "rankfd_route1_power_design_${SET}_B${B}.csv" \
               rankfd_route1_power_design_variants.log
