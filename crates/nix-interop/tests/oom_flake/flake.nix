{
  outputs = { self }:
  # Allocates a list with ~1G elements.
  # NB. This evaluates to false, so it will never be cached.
  assert builtins.length (builtins.head (builtins.genList (x: x) 1000000000)) == 42;
  { };
}
