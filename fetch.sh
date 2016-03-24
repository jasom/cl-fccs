set -e
cd ~/quicklisp/local-projects
mkdir /data
curl -L https://github.com/jasom/mymongrel2/archive/2bb2868f24fdd82530563b9544edbc7d53919893.tar.gz|tar zx
curl -L https://github.com/jasom/clack-handler-mongrel2/archive/5158fd22b8abc2ee38f976bb3e42fa5ead486405.tar.gz|tar zx
curl -L https://github.com/jasom/parenscriptx/archive/3f320f8cfd267b492d846b2ecf79349355b3ad40.tar.gz|tar zx
curl -L https://github.com/jasom/pdfparse/archive/80d0628bb6c94857dbaa1c7b8241b9fcbeb62122.tar.gz|tar zx
curl -L https://github.com/jasom/tnetstring/archive/bde2ced154e232e2f6f2f5b7ce442067e67f16e6.tar.gz|tar zx
curl -L https://github.com/jasom/cl-fccs/archive/dde48bbab57779b6c6e6621a8986297fa37564a6.tar.gz|tar zx
curl -L https://github.com/jasom/fccgparse/archive/753ee1134404268373e0637bc4c8e701eb27c577.tar.gz|tar zx

