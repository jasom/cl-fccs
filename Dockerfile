#FROM nixos/nix:1.10
#RUN nix-env --install -A nixpkgs.sbcl nixpkgs.openssl nixpkgs.curl nixpkgs.glibcLocales nixpkgs.gcc nixpkgs.zeromq
#COPY custom-nix /tmp/custom-nix
#RUN nix-env -I nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs -if /tmp/custom-nix/custom-packages.nix -A mongrel2
FROM nixfccsbase
COPY quicklisp.lisp /tmp/
RUN sbcl --load /tmp/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file))' --eval '(quit)'
COPY fetch.sh /tmp/
RUN sh /tmp/fetch.sh
COPY build.sh /tmp/
RUN sh /tmp/build.sh
COPY launch.sh /tmp/
CMD sh /tmp/launch.sh
