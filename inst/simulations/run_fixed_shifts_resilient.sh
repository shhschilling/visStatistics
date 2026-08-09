#!/bin/sh
# Auto-restart wrapper: rankfd_route1_power_fixed_shifts.R now checkpoints
# each completed cell to the CSV, so if the background job is killed
# (twice observed: all 8 mclapply workers fail with SIGPIPE simultaneously,
# consistent with the parent process losing its session), this loop just
# relaunches it and it resumes from the last completed cell instead of
# starting over.
TOTAL_CELLS=90
cd "$(dirname "$0")"
while true; do
  Rscript rankfd_route1_power_fixed_shifts.R 50000 8 >> rankfd_route1_power_fixed_shifts.log 2>&1
  DONE=$(grep -c "^done:" rankfd_route1_power_fixed_shifts.log 2>/dev/null || echo 0)
  if [ -f rankfd_route1_power_fixed_shifts_B50000.csv ]; then
    ROWS=$(($(wc -l < rankfd_route1_power_fixed_shifts_B50000.csv) - 1))
  else
    ROWS=0
  fi
  if [ "$ROWS" -ge "$TOTAL_CELLS" ]; then
    echo "All $TOTAL_CELLS cells complete." >> rankfd_route1_power_fixed_shifts.log
    break
  fi
  echo "Restarting: $ROWS/$TOTAL_CELLS cells done so far." >> rankfd_route1_power_fixed_shifts.log
  sleep 5
done
