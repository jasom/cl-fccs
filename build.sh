set -e
#export LANG="en_US.UTF-8"
#export LD_LIBRARY_PATH="/lib/"
#export LOCALE_ARCHIVE="/lib/locale/locale-archive"
sbcl <<END
(ql:quickload :fccgparse)
(ql:quickload :cffi-grovel)
(push "-I/include" cffi-grovel::*cc-flags*)
(handler-bind
  ((asdf:missing-dependency
    (lambda (e)
      (ql:quickload (slot-value e 'asdf/find-system::requires))
      (invoke-restart 'asdf:retry))))
  (asdf:operate 'asdf:prepare-op :cl-fccs))
(quit)
END
