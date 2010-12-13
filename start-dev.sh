#!/bin/sh
cd `dirname $0`
exec erl +P 1000000 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s crest
