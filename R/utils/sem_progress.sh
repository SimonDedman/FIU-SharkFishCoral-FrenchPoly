#!/bin/sh
# Progress display for R/05b_SEM_piecewise_brms.R. Run from the project root:
#   watch -n 60 -t -c 'cd "<project root>" && sh R/utils/sem_progress.sh [variant]'
# variant: omitted or "original" -> Results/SEM ; otherwise Results/SEM/variants/<variant>
# Liveness comes from the systemd unit the run was started under (never pgrep):
#   systemd-run --user --unit=claude-frenchpoly-sem-refit ...
V="${1:-original}"
if [ "$V" = "original" ]; then D=Results/SEM; else D="Results/SEM/variants/$V"; fi
UNIT=claude-frenchpoly-sem-refit
P="$D/progress.txt"
L=$(ls -t "$D"/run_05b_*.log 2>/dev/null | grep -v stageB | head -n 1)
# denominator measured from the worker's own tally ("brms 12/51 fits"), not hard-coded
TOTAL=$(grep -o 'brms [0-9]*/[0-9]* fits' "$P" 2>/dev/null | tail -n 1 | sed 's/.*\/\([0-9]*\) fits/\1/')
[ -z "$TOTAL" ] && TOTAL="?"
done_fits=$(ls "$D"/brms/*.Rds 2>/dev/null | wc -l)
active=$(systemctl --user is-active "$UNIT" 2>/dev/null)
if grep -q "DONE 05b" "$P" 2>/dev/null && grep -q "^R EXIT 0" "$L" 2>/dev/null; then state=DONE
elif grep -q "^R EXIT [1-9]" "$L" 2>/dev/null; then state="FAILED ($(grep '^R EXIT' "$L" | tail -n 1))"
elif [ "$active" = "active" ] || [ "$active" = "activating" ]; then
  age=$(( $(date +%s) - $(stat -c %Y "$P" 2>/dev/null || echo 0) ))
  if [ "$age" -gt 900 ]; then state="STALLED? (no progress line for ${age}s)"; else state=RUNNING; fi
else state="STOPPED EARLY (unit $UNIT is '$active', no DONE marker; killed or memory cap hit?)"; fi
echo "05b SEM [$V]   state: $state   $(date '+%H:%M:%S %Z')"
echo "brms fits cached on disk: $done_fits / $TOTAL   (independent count of $D/brms/*.Rds)"
echo "--- last progress lines (worker's own tally; its ETA is only valid while RUNNING):"
tail -n 4 "$P" 2>/dev/null
echo "--- errors in log ($L): $(grep -c -i -E '^Error|Execution halted' "$L" 2>/dev/null)"
grep -i -E '^Error|Execution halted' "$L" 2>/dev/null | tail -n 3
