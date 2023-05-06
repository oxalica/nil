## LSP Configuration

There are some tunable options and settings for nil.
They are retrieved via LSP and support runtime modification.

All settings are nested under a key `"nil"`.
For example, `formatting.command` means to write
`"nil": { "formatting": { "command": ["your-command"] } }`
in JSON, not `"nil.formatting.command": ["wrong"]`.

The place to write LSP configurations differs between clients (i.e. editors and editor plugins).
Please refer to their corresponding documentation.
There are some examples for common editor/plugins in [README](../README.md).

### Reference

Default configuration:

```jsonc
{
  "nil": {
    "formatting": {
      // External formatter command (with arguments).
      // It should accepts file content in stdin and print the formatted code into stdout.
      // Type: [string] | null
      // Example: ["nixpkgs-fmt"]
      "command": null,
    },
    "diagnostics": {
      // Ignored diagnostic kinds.
      // The kind identifier is a snake_cased_string usually shown together
      // with the diagnostic message.
      // Type: [string]
      // Example: ["unused_binding", "unused_with"]
      "ignored": [],
      // Files to exclude from showing diagnostics. Useful for generated files.
      // It accepts an array of paths. Relative paths are joint to the workspace root.
      // Glob patterns are currently not supported.
      // Type: [string]
      // Example: ["Cargo.nix"]
      "excludedFiles": [],
    },
    "nix": {
      // The path to the `nix` binary.
      // Type: string
      // Example: "/run/current-system/sw/bin/nix"
      "binary": "nix",
      "flake": {
        // Auto-archiving behavior which may use network.
        //
        // - null: Ask every time.
        // - true: Automaticaly run `nix flake archive` when necessary.
        // - false: Do not archive. Only load inputs that are already on disk.
        // Type: null | boolean
        // Example: true,
        "autoArchive": null,
      },
    },
  },
}
```
