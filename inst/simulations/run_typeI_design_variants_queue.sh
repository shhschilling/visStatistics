#!/bin/sh
# Type I grid on Brunner's scaling vectors. Checkpoints every cell and skips
# finished cells on restart, so a lost session resumes rather than restarts.
cd "$(dirname "$0")"
TOTAL=150
SET=brunner
B=50000
CORES=8
LOG=route1_typeI_design_variants.log

while true; do
  Rscript route1_typeI_design_variants.R "$B" "$CORES" "$SET" >> "$LOG" 2>&1
  csv="route1_typeI_design_${SET}_B${B}.csv"
  if [ -f "$csv" ]; then rows=$(($(wc -l < "$csv") - 1)); else rows=0; fi
  [ "$rows" -ge "$TOTAL" ] && { echo "COMPLETE: $csv ($rows cells)" >> "$LOG"; break; }
  echo "restarting at $rows/$TOTAL cells" >> "$LOG"
  sleep 5
done
