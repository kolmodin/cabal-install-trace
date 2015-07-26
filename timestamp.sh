#!/bin/sh

getdate() {
  (date +%N | grep --invert-match N > /dev/null) && which date || which gdate || echo /usr/local/Cellar/coreutils/*/bin/gdate
}

DATECMD=`getdate`

init=`$DATECMD +%s%6N`
while read line
do
  # Using date adds about 5ms per line on my machine.
  next=`$DATECMD +%s%6N`
  echo $((next-init)): $line
done
