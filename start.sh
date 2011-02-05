#!/bin/sh

if [ $# -lt 1 ]; then
    echo "Usage: $0 node_name"
    exit 1
fi

cd `dirname $0`
exec erl +P 1000000 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -config config/sasl-logger.config -sname $1 -s crest
