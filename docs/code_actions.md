## Code actions

Code actions are applicable code transformations.

- In [coc.nvim], they can be triggered on cursor position via `<Plug>(coc-codeaction-cursor)`,
which by default binds to `<leader>ac`.

[coc.nvim]: https://github.com/neoclide/coc.nvim

- In [VSCode], they can be triggered by clicking the lightbulb :bulb: icon
  at the beginning of the cursor line.

[VSCode]: https://code.visualstudio.com/

Here is the list of all implemented code actions, sorted by their `mod` name in code
`crates/ide/src/ide/assists`.
Currently documentations below are simply copied from doc-comments of their `mod`s.

### `add_to_top_level_lambda_param`

Add an undefined name to the top-level lambda.

```nix
{ foo }: foo + bar
```
=>
```nix
{ foo, bar }: foo + bar
```

### `convert_to_inherit`

Convert `path = value;` into `inherit key;`.

This covers,
- `prefix.key = key;` => `prefix = { inherit key; };`
Here the `prefix` set mut not be `rec`. The code before is actually
an infinite recursion while the code after is not.
- `prefix.key = from.key;` => `prefix = [rec] { inherit (from) key; };`
Since the `from` is resolved in the `prefix` scope thus
it is allowed to have recursive references (but may not be infinite recursion).

### `flatten_attrset`

Flatten binding with Attrset RHS into multiple bindings of outer level.
FIXME: Indentations are not reformated well.

```nix
{
foo = {
    bar = 1;
    baz = 2;
};
}
```
=>
```nix
{
foo.bar = 1;
foo.baz = 2;
}
```

### `pack_bindings`

Pack multiple bindings with the same prefix into nested one.
FIXME: Indentations are not reformated well.

```nix
{
foo.bar = 1;
foo.baz = 2;
}
```
=>
```nix
{
foo = {
    bar = 1;
    baz = 2;
};
}
```

### `remove_empty_inherit`

Remove empty `inherit;` or `inherit (...);`.

```nix
{ foo = "bar"; inherit; }
```
=>
```nix
{ foo = "bar"; }
```

### `convert_let_attrset`

Convert `let {...}` to `let ... in ...`.

```nix
let { foo = "bar"; body = foo; }
```
=>
```nix
let foo = "bar"; in foo
```
