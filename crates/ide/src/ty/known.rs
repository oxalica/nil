use super::{Attrset, Ty};

/// https://nixos.wiki/wiki/Flakes
pub fn flake(inputs: &[&str]) -> Ty {
    // https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html#flake-references
    let input_ty = ty!({
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

        "inputs": {
            _: {
                "follows": string,
            }
        },
    });
    let inputs_ty = Ty::Attrset(Attrset::from_internal(
        inputs.iter().copied().map(|name| (name, input_ty.clone())),
    ));

    let input_param_ty = ty!({
        "rev": string,
        "submodules": bool,

        "outPath": string,
        "narHash": string,
        "shortRev": string,
        "revCount": int,
        "lastModified": int,
        "lastModifiedDate": string,

        "sourceInfo": {
            "rev": string,
            "submodules": bool,

            "outPath": string,
            "narHash": string,
            "shortRev": string,
            "revCount": int,
            "lastModified": int,
            "lastModifiedDate": string,
        },
        "inputs": { },
        "outputs": { },
    });
    let outputs_param_ty = Ty::Attrset(Attrset::from_internal(
        inputs
            .iter()
            .copied()
            .chain(Some("self"))
            .map(|name| (name, input_param_ty.clone())),
    ));

    ty!({
        "description": string,
        "nixConfig": { _: ? },
        "inputs": (#inputs_ty),
        // https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-develop.html?highlight=flake#flake-output-attributes
        "outputs": ((#outputs_param_ty) -> {
            "apps": {
                _: {
                    "default": { "type": string, "program": string },
                    _: { "type": string, "program": string }
                }
            },
            "checks": {
                _: {
                    _: derivation
                }
            },
            "devShells": {
                "default": derivation,
                _: derivation
            },
            "formatter": {
                _: derivation
            },
            "hydraJobs": {
                _: {
                    _: derivation
                }
            },
            "legacyPackages": {
                _: {
                    _: ?
                }
            },
            "nixosConfigurations": {
                _: derivation
            },
            "nixosModules": {
                "default": ({ "config": { } } -> { "options": { }, "config": { } }),
                _: ({ "config": { } } -> { "options": { }, "config": { } })
            },
            "overlays": {
                "default": ({ } -> { } -> { }),
                _: ({ } -> { } -> { })
            },
            "packages": {
                "default": derivation,
                _: derivation
            },
            "templates": {
                "default": { "description": string, "path": string },
                _: { "description": string, "path": string }
            },
        }),
    })
}
