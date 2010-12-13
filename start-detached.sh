#!/bin/sh
cd `dirname $0`
exec erl +P 1000000 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -config config/sasl-logger.config -s reloader -s crest -detached
