# References:
# - nixos/lib/eval-cacheable-options.nix
# - nixos/lib/make-options-doc/default.nix
nixpkgs:
let
  lib = import (nixpkgs + "/lib");
  modulePath = nixpkgs + "/nixos/modules";

  inherit (builtins) filter mapAttrs isPath isFunction functionArgs addErrorContext;
  inherit (lib) evalModules trivial optionalAttrs optionals filterAttrs;
  inherit (lib.options) unknownModule renderOptionValue showOption;

  # Dummy `pkgs`.
  pkgs = import (nixpkgs + "/pkgs/pkgs-lib") {
    inherit lib;
    pkgs = null;
  };
  utils = import (nixpkgs + "/nixos/lib/utils.nix") {
    inherit config lib;
    pkgs = null;
  };

  modules =
    (filter canCacheDocs
      (import (modulePath + "/module-list.nix")));

  # From `nixos/modules/misc/documentation.nix`.
  canCacheDocs = m:
    let
      f = import m;
      instance = f (mapAttrs (n: _: abort "evaluating ${n} for `meta` failed") (functionArgs f));
    in
      isPath m
        && isFunction f
        && instance ? options
        && instance.meta.buildDocsInSandbox or true;

  config = {
    _module.check = false;
    _module.args = {};
    system.stateVersion = trivial.release;
  };
  eval = evalModules {
    modules = modules ++ [ config ];
    specialArgs = {
      inherit config pkgs utils;
    };
  };

  # Modified from `lib.optionAttrSetToDocList`.
  normalizeOptions = opt: let
    # visible: true | false | "shallow"
    visible = (opt.visible or true != false) && !(opt.internal or false);

    opt' = {
      description = opt.description or null;
      declarations = filter (x: x != unknownModule) opt.declarations;
      readOnly = opt.readOnly or false;
      type = opt.type.description or "unspecified";
      example =
        if opt ? example then
          renderOptionValue opt.example
        else
          null;
      default =
        if opt ? default then
          renderOptionValue (opt.defaultText or opt.default)
        else
          null;
      relatedPackages =
        optionals (opt.relatedPackages or null != null)
          opt.relatedPackages;

      # TODO: Submodules.
    };
  in
    if visible then
      opt'
    else
      null;

  normalizeOptionOrSet = opts:
    if opts._type or null == "option" then
      normalizeOptions opts
    else {
      children =
        filterAttrs (k: v: !isNull v)
          (mapAttrs (_: normalizeOptionOrSet) opts);
    };

in
  normalizeOptionOrSet
    eval.options
