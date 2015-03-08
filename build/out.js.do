redo-ifchange config.sh in.js
. ./config.sh
exec >&2
rm -rf tmpin tmpout
mkdir tmpin tmpout
cp in.js tmpin
$JSX tmpin tmpout
cp tmpout/in.js $3
rm -rf tmpin tmpout
