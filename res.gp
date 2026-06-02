set datafile separator ","

# set terminal cairolatex pdf color size 5.0in, 3.5in
# set output "res2.tex"

set terminal svg size 600,400 dynamic enhanced background 'white'
set output 'res2.svg'

set xlabel "Size of E-graph with saturation under AC"
set ylabel "Size of EMT with AC-completion"
set key at graph 0.95, 0.95 right top box samplen 2

set grid lt 0 lc rgb "#E0E0E0"

f(x) = x

plot f(x) title "Line of comparison" with lines lc rgb "#555555" lw 2 dt 2, \
     "res2.csv" using 1:($2 < $1 ? $2 : NaN) title "EMT size less than or equal to E-graph size" with points lc rgb "#228B22" pt 7 ps 0.4, \
     "res2.csv" using 1:($2 > $1 ? $2 : NaN) title "EMT size greater than E-graph size" with points lc rgb "#DC143C" pt 7 ps 0.4