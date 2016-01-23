. ./config.sh
SRCROOT="static/style"
SOURCES="$SRCROOT/pure-min.css
$SRCROOT/grid.css
$SRCROOT/local.css"
redo-ifchange $SOURCES ./config.sh
cat $SOURCES |if test -z "$YUICOMP"; then cat; else java -jar "$YUICOMP" --type css; fi
