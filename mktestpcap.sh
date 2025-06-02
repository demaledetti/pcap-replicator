#!/usr/bin/env bash

packetMillions=$1

[ -z "$packetMillions" ] && packetMillions=1

testpcap="${XDG_RUNTIME_DIR}/test${packetMillions}.pcap"

# generate a pcap file with one single packet in it
nix run github:input-output-hk/haskell.nix#hix -- run github:karknu/rws#rws:exe:rws --projectArgs '{configureArgs = "--allow-newer=binary --disable-tests";}' -- -i <(curl https://raw.githubusercontent.com/karknu/rws/refs/heads/master/samples/udp.pkt) -o udp.pcap

# copy the pcap file header into the test pcap file
head -c 24 udp.pcap > "$testpcap"

# copy the pcap packet $packetMillions million times into the test pcap file
yes udp.pcap | head -n $((packetMillions * 1000 * 1000)) | xargs tail -qc 66 >> "$testpcap"

# show the size of the test pcap file
ls -lh "$testpcap"

# show that it does contain $packetMillions million packets in pcap format
nix run nixpkgs#tcpdump -- --count -r "$testpcap"
