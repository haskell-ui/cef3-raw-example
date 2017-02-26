{ mkDerivation, base, cef3-raw, stdenv, libcef3
}:
mkDerivation {
  pname = "cef3-raw-example";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cef3-raw ];
  postInstall = ''
    ln -s ${libcef3}/bin/locales $out/bin/
  '';
  description = "CEF3 bindings usage example";
  license = stdenv.lib.licenses.bsd3;
}
