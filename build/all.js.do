SOURCES="$REACTJS
    ./static/js/lz-string.js
    ./out.js
    ./exports.js"
redo-ifchange $SOURCES config.sh
. ./config.sh
case "$OPT" in
  DEBUG)    cat $SOURCES ;;
  NORMAL)   java -jar "$CLOSURE" \
      $SOURCES \
      --js_output_file $3 ;;
  ADVANCED) java -jar "$CLOSURE" \
      $SOURCES \
      --compilation_level ADVANCED_OPTIMIZATIONS \
      --js_output_file $3 ;;
  *) exit 1 ;;
      esac
