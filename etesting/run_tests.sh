#!/bin/bash
SOURCE="${BASH_SOURCE[0]}"
    while [ -h "$SOURCE" ]; do
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

erl -pa $DIR -noshell -s jarlang main -extra -escodegen $DIR/codegen.js $@ -mode eunit
