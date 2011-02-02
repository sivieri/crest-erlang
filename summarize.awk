#!/usr/bin/awk -f 
BEGIN { 
    CONVFMT="%.2f"
    numRounds=0;
}

/^  0.+/ {
    numRounds++;
    line=0;
}

/^[ 0123456789].+/ {
    time[line]=$1;
    nTr[line]+=$2;
    tTr[line]+=$3;
    bWidth[line]+=$4;
    line++;
}

END {
    print "Time\tN_Tr\tT_Tr\tBWidth"
    for(i=0; i<line; i++) {
	print time[i] "\t" nTr[i]/numRounds "\t" tTr[i]/numRounds "\t" bWidth[i]/numRounds "\t" 
    }
}
