#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Usage: $0 file_extension"
    exit 1
fi

# create data files
for syst in "erlangfull$1" "mochiwebfull$1" "schemefull$1"; do
    rm -f /tmp/$syst.data
    awk -f summarize.awk $syst | tail -n +1 >> /tmp/$syst.data
done

# plot response number
cat <<EOF >/tmp/plotcmd
set terminal postscript eps monochrome enhanced linewidth 2 dashlength 3 "Helvetica" 24
#set style line 1 linetype 1 linecolor rgb "red" linewidth 2
#set style line 2 linetype 1 linecolor rgb "blue" linewidth 2
set yrange [0:6500]
set xrange [0:250]
#set log y
set ylabel "Response number"
set xlabel "Test time (s)"
set key left top Left
set output "resnumber$1.eps"
plot "/tmp/mochiwebfull$1.data" using 1:2 title "MochiWeb" with lines ls 1,\
     "/tmp/erlangfull$1.data" using 1:2 title "CREST-Erlang" with lines ls 2,\
     "/tmp/schemefull$1.data" using 1:2 title "CREST-Scheme" with lines ls 4

EOF

gnuplot /tmp/plotcmd
convert -density 150x150 resnumber$1.eps -flatten resnumber$1.png

# plot response time
cat <<EOF >/tmp/plotcmd
set terminal postscript eps monochrome enhanced linewidth 2 dashlength 3 "Helvetica" 24
#set style line 1 linetype 1 linecolor rgb "red" linewidth 2
#set style line 2 linetype 1 linecolor rgb "blue" linewidth 2
set yrange [1:60000]
set xrange [0:250]
set log y
set ylabel "Response time (ms)"
set xlabel "Test time (s)"
set key right bottom Left
set output "restime$1.eps"
plot "/tmp/mochiwebfull$1.data" using 1:3 title "MochiWeb" with lines ls 1,\
     "/tmp/erlangfull$1.data" using 1:3 title "CREST-Erlang" with lines ls 2,\
     "/tmp/schemefull$1.data" using 1:3 title "CREST-Scheme" with lines ls 4
EOF

gnuplot /tmp/plotcmd
convert -density 150x150 restime$1.eps -flatten restime$1.png

# plot bandwidth
cat <<EOF >/tmp/plotcmd
set terminal postscript eps monochrome enhanced linewidth 2 dashlength 3 "Helvetica" 24
#set style line 1 linetype 1 linecolor rgb "red" linewidth 2
#set style line 2 linetype 1 linecolor rgb "blue" linewidth 2
set yrange [0:7200]
set xrange [0:250]
#set log y
set ylabel "Sent data (kByte/s)"
set xlabel "Test time (s)"
set key left top Left
set output "bandwidth$1.eps"
plot "/tmp/mochiwebfull$1.data" using 1:4 title "MochiWeb" with lines ls 1,\
     "/tmp/erlangfull$1.data" using 1:4 title "CREST-Erlang" with lines ls 2,\
     "/tmp/schemefull$1.data" using 1:4 title "CREST-Scheme" with lines ls 4
EOF

gnuplot /tmp/plotcmd
convert -density 150x150 bandwidth$1.eps -flatten bandwidth$1.png
