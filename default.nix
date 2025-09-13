{ mkDerivation, aeson, async, base, bytestring, containers
, data-default, http-client, http-client-tls, iCalendar, lib
, optparse-applicative, parallel, safe, swiss-ephemeris, tasty
, tasty-hunit, text, time, uuid
}:
mkDerivation {
  pname = "astro-calendar";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring containers data-default http-client
    http-client-tls iCalendar parallel swiss-ephemeris text time uuid
  ];
  executableHaskellDepends = [
    aeson base bytestring data-default iCalendar optparse-applicative
    safe swiss-ephemeris text time
  ];
  testHaskellDepends = [ tasty tasty-hunit ];
  doHaddock = false;
  license = lib.licenses.agpl3Plus;
  mainProgram = "astro-calendar-cli";
}
