#!/usr/bin/env bash

set -o errexit
set -o pipefail



usage() { echo "Usage: $0 <all|tcpdump|app|tcpdump_tcpdump|app_tcpdump> [-s <1|10|100>] [-q] [-b <benchmark binary path>] [-- <app options>]" 1>&2; exit 1; }

bench=$1
[ -z "$bench" ] && usage
[ "$bench" == all -o "$bench" == tcpdump -o "$bench" == app -o "$bench" == tcpdump_tcpdump -o "$bench" == app_tcpdump ] || usage
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
[ -z "$bbpath" ] && bbpath=$(cabal list-bin bench:pcap-replicator-bench)

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


the_pcap="${XDG_RUNTIME_DIR}/test${len}.pcap"
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
  app_bin_rel_path="/x/pcap-replicator/opt/build/pcap-replicator/pcap-replicator"
  echo $bbpath | sed "s,/b/.*,$app_bin_rel_path,"
}

#title "cat"
#time cat $the_pcap | pv -rt > /dev/null

#title "pv"
#time pv -rt $the_pcap > /dev/null

b_tcpdump() {
title "tcpdump"
pv -rt $the_pcap | /usr/bin/time -v tcpdump -nnr - --count
}

b_app() {
title "app"
/usr/bin/time -v $(appbin) -1 $app_args "pv -rt $the_pcap"
}

b_tcpdump_tcpdump() {
par "tcpdump + tcpdump" <<EOF
pv -rtc -N server $the_pcap | tcpdump -nnr - -w - | nc -Nl 8091
cx=0; while ! ss -ntHl \'( sport = :8091 )\' | grep -q . && [ \$cx -ne 1000 ]; do sleep .000001; cx=\$\(\(\$cx +1)); done; time nc -d localhost 8091 | pv -rtc -N client | tcpdump -nnr - --count
EOF
}

b_app_tcpdump() {
par "app + tcpdump" <<EOF
$(appbin) -1 "$app_args" \"while ! ss -ntH \'( sport = :8091 )\' | grep -q .; do sleep .000001; done; time pv -rtc -N server $the_pcap\"
cx=0; while ! ss -ntHl \'( sport = :8091 )\' | grep -q . && [ \$cx -ne 1000 ]; do sleep .000001; cx=\$\(\(\$cx +1)); done; time nc -d localhost 8091 | pv -rtc -N client | tcpdump -nnr - --count
EOF
if [ "$quiet" = "-q" ]
then
cat /tmp/debug.log | grep "^$packets packets$"
fi
}

case $bench in
  all)
    b_tcpdump
    b_app
    b_tcpdump_tcpdump
    b_app_tcpdump
    ;;
  *)
    b_${bench}
    ;;
esac
exit 0
