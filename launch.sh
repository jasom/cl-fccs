set -e
if ! test -e "$(echo ~/quicklisp/local-projects/cl-fccs*/data)/data.lisp"; then
sbcl <<EOF
(ql:quickload :fccgparse)
(fccgparse::write-data-file
  "$(echo ~/quicklisp/local-projects/cl-fccs*/data)/data.lisp"
  "/data/Fantasy_Craft_Second_Printing.pdf")
EOF
fi
#/root/fccgparsec /data/Fantasy_Craft_Second_Printing.pdf "$(echo ~/quicklisp/local-projects/cl-fccs*/data)/data.lisp"
cp /data/config.lisp ~/quicklisp/local-projects/cl-fccs*
export CFLAGS=-I/include
export LDFLAGS=-L/lib
exec sbcl <<EOF
(ql:quickload :cffi-grovel)
(push "-I/include" cffi-grovel::*cc-flags*)
(ql:quickload :cl-fccs)
(in-package :cl-fccs)
(restart-server)
(with-db-connection ()
  (unless (get-usernames)
    (make-user "admin" "admin")
    (add-user-role "admin" :administrator)))
(loop (sleep 60))
EOF
