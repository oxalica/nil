use super::{AttrSource, Attrset, Ty};
use once_cell::sync::Lazy;

pub static DERIVATION: Lazy<Ty> = Lazy::new(|| {
    ty!({
        "name": string,
        "system": stringish,
        "builder": stringish,
        "args": [stringish],
    })
});

pub static FETCH_TREE_ARG: Lazy<Ty> = Lazy::new(|| {
    ty!({
        "url": string,

        "type": string,
        "owner": string,
        "repo": string,
        "id": string,

        "dir": string,
        "narHash": string,
        "rev": string,
        "ref": string,
        "submodules": bool,
    })
});

pub static FETCH_TREE_RET: Lazy<Ty> = Lazy::new(|| {
    ty!({
        "lastModified": int,
        "lastModifiedDate": string,
        "narHash": string,
        "outPath": string,
        "rev": string,
        "shortRev": string,

        "revCount": int,
        "submodules": bool,
    })
});

pub static GENERIC_FLAKE: Lazy<Ty> = Lazy::new(|| {
    merge_attrset(
        &FETCH_TREE_RET,
        &ty!({
            "sourceInfo": (#FETCH_TREE_ARG.clone()),
            "inputs": { },
            "outputs": { },
        }),
    )
});

fn merge_attrset(lhs: &Ty, rhs: &Ty) -> Ty {
    let lhs = lhs.as_attrset().unwrap();
    let rhs = rhs.as_attrset().unwrap();
    // Put the RHS on the front and ...
    let mut xs = rhs
        .fields
        .iter()
        .chain(lhs.fields.iter())
        .map(|(name, ty, src)| (name.clone(), ty.clone(), *src))
        .collect::<Vec<_>>();
    // ... run stable sort to prefer RHS when duplicated.
    xs.sort_by(|(lhs, ..), (rhs, ..)| lhs.cmp(rhs));
    xs.dedup_by(|(lhs, ..), (rhs, ..)| lhs == rhs);
    Ty::Attrset(Attrset {
        fields: xs.into(),
        rest: rhs.rest.clone(),
    })
}

// https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html#flake-references
static GENERIC_INPUT_DECL: Lazy<Ty> = Lazy::new(|| {
    merge_attrset(
        &FETCH_TREE_ARG,
        &ty!({
            "inputs": {
                _: {
                    "follows": string,
                }
            },
        }),
    )
});

/// <https://wiki.nixos.org/wiki/Flakes>
pub fn flake(inputs: &[(&str, Ty)]) -> Ty {
    let inputs_decl_ty = Ty::Attrset(Attrset::from_internal(
        inputs.iter().map(|(name, ty)| {
            let ty = merge_attrset(ty, &GENERIC_INPUT_DECL);
            (*name, ty, AttrSource::Unknown)
        }),
        None,
    ));

    let outputs_param_ty = Ty::Attrset(Attrset::from_internal(
        inputs
            .iter()
            // Don't duplicate.
            .filter(|(name, _)| *name != "self")
            .map(|(name, output_ty)| {
                let ty = merge_attrset(output_ty, &GENERIC_FLAKE);
                let ty = merge_attrset(
                    &ty,
                    &ty!({
                        "outputs": (#output_ty.clone()),
                    }),
                );
                (*name, ty, AttrSource::Unknown)
            })
            .chain(Some(("self", ty!({}), AttrSource::Unknown))),
        None,
    ));

    ty!({
        "description": string,
        "nixConfig": { _: ? },
        "inputs": (#inputs_decl_ty),
        // https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-develop.html?highlight=flake#flake-output-attributes
        "outputs": ((#outputs_param_ty) -> {
            "apps": {
                // System
                _: {
                    "default": { "type": string, "program": string },
                    // Name
                    _: { "type": string, "program": string }
                }
            },
            "checks": {
                // System
                _: {
                    // Name
                    _: derivation
                }
            },
            "devShells": {
                // System
                _: {
                    "default": derivation,
                    // Name
                    _: derivation
                }
            },
            "formatter": {
                // System
                _: derivation
            },
            "hydraJobs": {
                // Name
                _: {
                    // System
                    _: derivation
                }
            },
            "legacyPackages": {
                // System
                _: {
                    _: ?
                }
            },
            "nixosConfigurations": {
                // Name
                _: {
                    "config": { },
                    "extendModules":
                        ({
                            "modules": [?],
                            "specialArgs": { },
                            "prefix": [string]
                        } -> { }),
                    "extraArgs": { },
                    "options": { },
                    "pkgs": { },
                    "type": { }
                }
            },
            "nixosModules": {
                "default": ({ "config": { } } -> { "options": { }, "config": { } }),
                // Name
                _: ({ "config": { } } -> { "options": { }, "config": { } })
            },
            "overlays": {
                "default": ({ } -> { } -> { }),
                // Name
                _: ({ } -> { } -> { })
            },
            "packages": {
                // System
                _: {
                    "default": derivation,
                    // Name
                    _: derivation
                }
            },
            "templates": {
                "default": { "description": string, "path": string },
                // Name
                _: { "description": string, "path": string }
            },
        }),
    })
}

/// Flake output fields which are generic over systems.
///
/// This is used to spread one fields (current system) into Attrset's rest type, so that
/// `packages.${nonStaticSystem}` can fallback to a specific field type for any non-static
/// attributes.
///
/// The first `&str` is the field name of flake output attrset, the second `usize` is the depth
/// of the system field under that the former field. Eg.
/// ```text
/// `("packages", 0)`: "outputs.packages.<system>"
///                                      ^ 0
/// `("hydraJobs", 1)`: "outputs.hydraJobs.<name>.<system>".
///                                        ^0     ^1
/// ```
pub const FLAKE_OUTPUT_GENERIC_SYSTEM_FIELDS: &[(&str, usize)] = &[
    ("apps", 0),
    ("checks", 0),
    ("devShells", 0),
    ("formatter", 0),
    ("hydraJobs", 1),
    ("legacyPackages", 0),
    ("packages", 0),
];

pub static BUILTINS: Lazy<Ty> = Lazy::new(|| {
    // Unfold one layer.
    // This is necessary since the top-level `builtins` is accessed via
    // the field of `BUILTINS`.
    let b = builtins();
    merge_attrset(
        &b,
        &ty!({(AttrSource::Builtin)
            "builtins": (#b.clone()),
        }),
    )
});

fn builtins() -> Ty {
    ty!({(AttrSource::Builtin)
        "abort": (stringish -> !),
        "add": (number -> number -> number),
        "addErrorContext": (forall a, stringish -> a -> a),
        "all": (forall a, (a -> bool) -> [a] -> bool),
        "any": (forall a, (a -> bool) -> [a] -> bool),
        "appendContext": (forall a, (a -> bool) -> { _: { "outputs": [string] } }),
        "attrNames": ({ } -> [string]),
        "attrValues": (forall a, { _: a } -> [a]),
        "baseNameOf": (stringish -> string),
        "bitAnd": (int -> int -> int),
        "bitOr": (int -> int -> int),
        "bitXor": (int -> int -> int),
        "break": (forall a, a -> a),
        // TODO: Recursive types.
        "builtins": ?,
        "catAttrs": (forall a, string -> [{ _: a }] -> a),
        "ceil": (float -> int),
        "compareVersions": (string -> string -> bool),
        "concatLists": (forall a, [[a]] -> [a]),
        "concatMap": (forall a b, (a -> [b]) -> [a] -> [b]),
        "concatStringsSep": (string -> [stringish] -> string),
        "currentSystem": string,
        "currentTime": int,
        "deepSeq": (forall a b, a -> b -> b),
        "derivation": (derivation -> derivation),
        "derivationStrict": (derivation -> derivation),
        "dirOf": (stringish -> string),
        "div": (number -> number),
        "elem": (forall a, a -> [a] -> bool),
        "elemAt": (forall a, [a] -> int -> a),
        "false": bool,
        // https://github.com/NixOS/nix/blob/2.13.2/src/libfetchers/git.cc#L285
        "fetchGit": (({
            "allRefs": bool,
            "lastModified": int,
            "name": string,
            "narHash": string,
            "ref": string,
            "rev": string,
            "revCount": int,
            "shallow": bool,
            "submodules": bool,
            "url": string,
        } | string) -> (#FETCH_TREE_RET.clone())),
        // https://github.com/NixOS/nix/blob/2.13.2/src/libfetchers/mercurial.cc#L77
        "fetchMercurial": (({
            "name": string,
            "narHash": string,
            "ref": string,
            "rev": string,
            "revCount": string,
            "url": string,
        } | string) -> (#FETCH_TREE_RET.clone())),
        // https://github.com/NixOS/nix/blob/2.13.2/src/libfetchers/tarball.cc#L211
        "fetchTarball": (({
            "name": string,
            "narHash": string,
            "url": string,
        } | string) -> (#FETCH_TREE_RET.clone())),
        "fetchTree": (((#FETCH_TREE_ARG.clone()) | string) -> (#FETCH_TREE_RET.clone())),
        "fetchurl": (string -> string),
        "filter": (forall a, (a -> bool) -> [a] -> a),
        "filterSource": ((string -> string -> bool) -> path -> path),
        "findFile": ([{ "prefix": string, "path": string }] -> string -> string),
        "floor": (number -> int),
        "foldl'": (forall a b, (a -> b -> a) -> a -> [b] -> a),
        "fromJSON": (forall a, string -> a),
        "fromTOML": (forall a, string -> a),
        "functionArgs": (forall a b, (a -> b) -> { _: bool }),
        "genList": (forall a, (int -> a) -> int -> [a]),
        "genericClosure": (forall a, {
            "startSet": [{ "key": a }],
            "operator": ({ "key": a } -> [{ "key": a }]),
        } -> [{ "key": a }]),
        "getAttr": (forall a, string -> { _: a } -> a),
        "getContext": (string -> { _: { "outputs": [string] } }),
        "getEnv": (string -> string),
        "getFlake": (string -> (#GENERIC_FLAKE.clone())),
        "groupBy": (forall a, (a -> string) -> [a] -> { _: [a] }),
        "hasAttr": (string -> { } -> bool),
        "hasContext": (string -> bool),
        "hashFile": (string -> stringish -> string),
        "hashString": (string -> string -> string),
        "head": (forall a, [a] -> a),
        "import": (forall a, stringish -> a),
        "intersectAttrs": ({ _: a } -> { _: a } -> { _: a }),
        "isAttrs": (forall a, a -> bool),
        "isBool": (forall a, a -> bool),
        "isFloat": (forall a, a -> bool),
        "isFunction": (forall a, a -> bool),
        "isInt": (forall a, a -> bool),
        "isList": (forall a, a -> bool),
        "isNull": (forall a, a -> bool),
        "isPath": (forall a, a -> bool),
        "isString": (forall a, a -> bool),
        "langVersion": int,
        "length": (forall a, [a] -> int),
        "lessThan": (forall a, a -> a -> bool),
        "listToAttrs": (forall a, [{ "name": string, "value": a }] -> { _: a }),
        "map": (forall a b, (a -> b) -> [a] -> [b]),
        "mapAttrs": (forall a b, (string -> a -> b) -> { _: a } -> { _: b }),
        "match": (regex -> string -> [string]),
        "mul": (number -> number -> number),
        "nixPath": [{ "path": string, "prefix": string }],
        "nixVersion": string,
        "null": (forall a, a),
        "parseDrvName": (string -> { "name": string, "version": string }),
        "partition": (forall a, (a -> bool) -> [a] -> { "right": [a], "wrong": [a] }),
        "path": ({
            "path": stringish,
            "name": string,
            "filter": (string -> string -> bool),
            "recursive": bool,
            "sha256": string,
        } -> path),
        "pathExists": (stringish -> bool),
        "placeholder": (string -> string),
        "readDir": (path -> { _: string }),
        "readFile": (path -> string),
        "removeAttrs": ({ } -> [string] -> { }),
        "replaceStrings": ([string] -> [string] -> string -> string),
        "scopedImport": (forall a, { } -> path -> a),
        "seq": (forall a b, a -> b -> b),
        "sort": (forall a, (a -> a -> bool) -> [a] -> [a]),
        "split": (forall a, regex -> string -> [(string | [string])]),
        "splitVersion": (string -> [string]),
        "storeDir": string,
        "storePath": (path -> string),
        "stringLength": (string -> int),
        "sub": (number -> number -> number),
        "substring": (int -> int -> stringish -> string),
        "tail": (forall a, [a] -> [a]),
        "throw": (stringish -> !),
        "toFile": (string -> string -> string),
        "toJSON": (forall a, a -> string),
        "toPath": (stringish -> string),
        "toString": (forall a, a -> string),
        "toXML": (forall a, a -> string),
        "trace": (forall a b, a -> b -> b),
        "traceVerbose": (forall a b, a -> b -> b),
        "true": bool,
        "tryEval": (forall a, a -> { "success": bool, "value": a }),
        "typeOf": (forall a, a -> string),
        "unsafeDiscardOutputDependency": (stringish -> string),
        "unsafeDiscardStringContext": (stringish -> string),
        "unsafeGetAttrPos": (string -> { } -> string),
        "zipAttrsWith": (forall a b, (string -> [a] -> b) -> [{ _: a }] -> { _: b }),
    })
}

pub static PACKAGE: Lazy<Ty> = Lazy::new(|| {
    ty!({
        // TODO: nixpkgs.lib
        "lib": { },
        // TODO: Packages.
        "pkgs": { },
    } -> derivation)
});

pub fn config_module(config: Ty) -> Ty {
    ty!({
        "lib": { },
        "config": (#config.clone()),
        "pkgs": { },
    } -> {
        // https://github.com/NixOS/nixpkgs/blob/fcb7bdf46213eac1a9cb573d2737620e93e46bfb/nixos/modules/misc/meta.nix#L33
        "meta": {
            "maintainers": [?],
            "doc": path,
            "buildDocsInSandbox": bool,
        },
        // TODO
        "options": { },
        "config": (#config),
    })
}

pub fn config(config: Ty) -> Ty {
    ty!({
        "lib": { },
        "config": (#config.clone()),
        "pkgs": { },
    } -> (#config))
}
