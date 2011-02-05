#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Usage: $0 file_extension"
    exit 1
fi

# create data files
for syst in "erlangcrest$1" "mochiwebcrest$1"; do
    rm -f /tmp/$syst.data
    awk -f summarize.awk $syst | tail -n +1 >> /tmp/$syst.data
done

# plot response time
cat <<EOF >/tmp/plotcmd
set terminal postscript eps monochrome enhanced linewidth 2 dashlength 3 "Helvetica" 24
#set style line 1 linetype 1 linecolor rgb "red" linewidth 2
#set style line 2 linetype 1 linecolor rgb "blue" linewidth 2
set yrange [0:150]
set xrange [0:250]
#set log y
set ylabel "Response time (ms)"
set xlabel "Test time (s)"
set key left top Left
set output "restime$1.eps"
plot "/tmp/erlangcrest$1.data" using 1:3 title "CREST-Erlang" with lines ls 1,\
     "/tmp/mochiwebcrest$1.data" using 1:3 title "MochiWeb" with lines ls 2
EOF

gnuplot /tmp/plotcmd
convert -density 150x150 restime$1.eps restime$1.png

# plot bandwidth
cat <<EOF >/tmp/plotcmd
set terminal postscript eps monochrome enhanced linewidth 2 dashlength 3 "Helvetica" 24
#set style line 1 linetype 1 linecolor rgb "red" linewidth 2
#set style line 2 linetype 1 linecolor rgb "blue" linewidth 2
set yrange [0:50]
set xrange [0:250]
#set log y
set ylabel "Sent data (kByte/s)"
set xlabel "Test time (s)"
set key left top Left
set output "bandwidth$1.eps"
plot "/tmp/erlangcrest$1.data" using 1:4 title "CREST-Erlang" with lines ls 1,\
     "/tmp/mochiwebcrest$1.data" using 1:4 title "MochiWeb" with lines ls 2
EOF

gnuplot /tmp/plotcmd
convert -density 150x150 bandwidth$1.eps bandwidth$1.png
