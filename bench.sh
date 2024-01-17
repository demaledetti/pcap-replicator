#!/usr/bin/env bash

set -o errexit
set -o pipefail



usage() { echo "Usage: $0 <all|tcpdump|app-ioref|app-statet|tcpdump_tcpdump|app_tcpdump-ioref|app_tcpdump-statet> [-s <1|10|100>] [-q] [-b <benchmark binary path>] [-- <app options>]" 1>&2; exit 1; }

bench=$1
[ -z "$bench" ] && usage
[ "$bench" == all -o "$bench" == tcpdump -o "$bench" == app-ioref -o "$bench" == app-statet -o "$bench" == tcpdump_tcpdump -o "$bench" == app_tcpdump-ioref -o "$bench" == app_tcpdump-statet ] || usage
shift

while getopts "s:b:qh-" o; do
    case "${o}" in
        s)
            len=${OPTARG}
            ((len == 1 || len == 10 || len == 100)) || usage
            ;;
        b)
            bbpath=${OPTARG}
            ;;
        q)
            quiet="-q"
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

[ -z "$len" ] && len=10
[ -z "$bbpath" ] && bbpath=$(cabal list-bin bench:pcap-replicator)

# echo "bench = ${bench}"
# echo "len = ${len}"
# echo "quiet = ${quiet}"
# echo "bbpath = ${bbpath}"
# echo "rest = $@"
# exit 0


if [ "$quiet" = "-q" ]
then
  exec > /tmp/debug.log
  exec 2>&1
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

function waitport() { cx=0; while ! ss -ntHl '( sport = :8091 )' | grep -q . -a $cx -ne 1000 ]; do sleep .000001; cx=$(($cx +1)); done; }

appbin() {
  local app=$1
  app_bin_rel_path_template="/x/pcap-replicator-_the_app_/opt/build/pcap-replicator-_the_app_/pcap-replicator-_the_app_"
  app_bin_rel_path="${app_bin_rel_path_template//_the_app_/$app}"
  echo $bbpath | sed "s,/b/.*,$app_bin_rel_path,"
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
/usr/bin/time -v $(appbin $app) -1 $app_args "pv -rt $the_pcap"
}

b_tcpdump_tcpdump() {
par "tcpdump + tcpdump" <<EOF
pv -rtc -N server $the_pcap | tcpdump -nnr - -w - | nc -Nl 8091
cx=0; while ! ss -ntHl \'( sport = :8091 )\' | grep -q . && [ \$cx -ne 1000 ]; do sleep .000001; cx=\$\(\(\$cx +1)); done; time nc -d localhost 8091 | pv -rtc -N client | tcpdump -nnr - --count
EOF
}

b_app_tcpdump() {
local app=$1
par "$app + tcpdump" <<EOF
$(appbin $app) -1 "$app_args" \"sleep .1; time pv -rtc -N server $the_pcap\"
cx=0; while ! ss -ntHl \'( sport = :8091 )\' | grep -q . && [ \$cx -ne 1000 ]; do sleep .000001; cx=\$\(\(\$cx +1)); done; time nc -d localhost 8091 | pv -rtc -N client | tcpdump -nnr - --count
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
