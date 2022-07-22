let
  a.b.${c}."d${e}" = 1;
  inherit a ${b} "c";
  ${dyn}.a = 1;
  inherit (a + b) c;
in {
  inherit c;
  d.e = rec { f = 1; };
  ${f} = let { body = 42; };
}
