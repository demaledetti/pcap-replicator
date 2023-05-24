#!/usr/bin/env bash

set -o errexit
set -o pipefail

bench=$1
shift
len=$1
shift
quiet=$1

if [ "$bench" = "" -o "$len" = "" ]
then
  echo "usage: $0"
  exit 1
fi
if [ "$quiet" = "-q" ]
then
  exec > /tmp/debug.log
  exec 2>&1
  shift
fi

app_args="$@"

the_pcap1=~/opt/haskell/rws/the1.pcap
packets=1000000
the_pcap10=~/opt/haskell/rws/the10.pcap
packets10=10000000
the_pcap100=~/opt/haskell/rws/the100.pcap
packets100=100000000

the_pcap=~/opt/haskell/rws/the${len}.pcap
packets=$(( $len * 1000 * 1000))

echo "runnning benchmark $bench with $packets packets"


title() {
  echo
  echo ======= $1
}
# https://unix.stackexchange.com/questions/472117/how-can-i-run-multiple-pv-commands-in-parallel
par() {
  title "$@"
  xargs -P 2 -I {} bash -c "{}"
}


#title "cat"
#time cat $the_pcap100 | pv -rt > /dev/null

#title "pv"
#time pv -rt $the_pcap100 > /dev/null

b_tcpdump() {
title "tcpdump"
pv -rt $the_pcap | /usr/bin/time -v tcpdump -nnr - --count >> /tmp/debug.log 2>&1
}

b_app() {
local app=$1
title "$app"
/usr/bin/time -v cabal run pcap-replicator-$app -- -1 $app_args "pv -rt $the_pcap"
}

b_tcpdump_tcpdump() {
par "tcpdump + tcpdump" <<EOF
pv -rtc -N server $the_pcap | tcpdump -nnr - -w - | nc -Nl 8091
sleep 1; time nc -d localhost 8091 | pv -rtc -N client | tcpdump -nnr - --count
EOF
}

b_app_tcpdump() {
local app=$1
par "$app + tcpdump" <<EOF
cabal run pcap-replicator-$app -- -1 "$app_args" \"sleep 1; time pv -rtc -N server $the_pcap\"
sleep .9; time nc -d localhost 8091 | pv -rtc -N client | tcpdump -nnr - --count
EOF
}

case $bench in
  all)
    b_tcpdump
    b_app ioref
    b_app statet
    b_tcpdump_tcpdump
    b_app_tcpdump ioref
    b_app_tcpdump statet
    ;;
  *)
    b_${bench/-/ }
    ;;
esac
exit 0
