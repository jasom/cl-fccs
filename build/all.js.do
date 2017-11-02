. ./config.sh
SOURCES="$MITHRILJS
    ./static/js/lz-string.js
    ./in.js
    ./exports.js"
redo-ifchange $SOURCES config.sh
case "$OPT" in
  DEBUG)    cat $SOURCES | $PRETTY > $3 ;;
  NORMAL)   java -jar "$CLOSURE" \
      $SOURCES \
      --js_output_file $3 ;;
  ADVANCED) java -jar "$CLOSURE" \
      $SOURCES \
      --compilation_level ADVANCED_OPTIMIZATIONS \
      --externs extern.js \
      --js_output_file $3 ;;
  *) exit 1 ;;
      esac
