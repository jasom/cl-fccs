with import <nixpkgs> {};

rec {
    nixbusybox = dockerTools.buildImage {
      name = "nixbusybox";
      contents = busybox;
    };
    nixsbcl = dockerTools.buildImage {
      fromImage = nixbusybox;
      name = "nixsbcl";
      contents = sbcl;
    };
    nixfccsbase = dockerTools.buildImage {
      fromImage = nixsbcl;
      name = "nixfccsbase";
      contents = [openssl curl glibcLocales gcc zeromq cacert mupdf];
      config = {
          Env = [ "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
                  "LANG=en_US.UTF-8"
                  "LD_LIBRARY_PATH=/lib"
                  "LOCALE_ARCHIVE=/lib/locale/locale-archive"
          ];
      };
    };
}
