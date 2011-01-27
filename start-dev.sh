#!/bin/sh
cd `dirname $0`
exec erl +P 1000000 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -sname crest_dev -s crest -s reloader
