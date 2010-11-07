#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -s test_remote main
