# References:
# - nixos/lib/eval-cacheable-options.nix
# - nixos/lib/make-options-doc/default.nix
nixpkgs:
let
  libPath = nixpkgs + "/lib";
  lib = import libPath;
  modulePath = nixpkgs + "/nixos/modules";
  moduleListPath = modulePath + "/module-list.nix";

  inherit (builtins) isString filter mapAttrs isPath isFunction functionArgs pathExists;
  inherit (lib) evalModules trivial optionals filterAttrs;
  inherit (lib.options) unknownModule literalExpression;

  # Polyfill for < 23.05
  renderOptionValue = lib.options.renderOptionValue or (v:
    if v ? _type && v ? text then v
    else literalExpression (lib.generators.toPretty {
      multiline = true;
      allowPrettyValues = true;
    } v));

  # Special tranfrom for < 23.05
  renderDoc = v:
    if isString v
    then { _type = "mdDoc"; text = v; }
    else v;

  # Dummy `pkgs`.
  pkgs = import (nixpkgs + "/pkgs/pkgs-lib") {
    inherit lib;
    pkgs = null;
  };
  utils = import (nixpkgs + "/nixos/lib/utils.nix") {
    inherit config lib;
    pkgs = null;
  };

  modules = filter canCacheDocs (import moduleListPath);

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

  # https://github.com/NixOS/nixpkgs/blob/28c1aac72e3aef70b8c898ea9c16d5907f9eae22/lib/types.nix#L212
  normalizeType = submoduleVisible: ty: let
    elem = normalizeType submoduleVisible ty.nestedTypes.elemType;
    ty' = rec {
      anything.name = "any";
      raw = anything;
      unspecified = anything;

      bool.name = "bool";

      int.name = "int";
      intBetween = int;
      unsignedInt = int;
      positiveInt = int;
      signedInt8 = int;
      signedInt16 = int;
      signedInt32 = int;
      unsignedInt8 = int;
      unsignedInt16 = int;
      unsignedInt32 = int;

      float.name = "float";
      number = float;
      numberBetween = float;
      numberNonnegative = float;
      numberPositive = float;

      str.name = "string";
      nonEmptyStr = str;
      singleLineStr = str;
      # strMatching<regex>
      separatedString = str;
      string = str;
      # passwdEntry<name>

      attrs = { name = "attrset"; rest = anything; };
      package.name = "derivation";
      shellPackage.name = "derivation";

      path.name = "path";

      listOf = { name = "list"; inherit elem; };

      attrsOf = { name = "attrset"; rest = elem; };
      lazyAttrsOf = { name = "attrset"; rest = elem; };

      uniq = elem;
      unique = elem;

      # FIXME: Union and null type.
      nullOr = elem;

      functionTo = { name = "lambda"; from = anything; to = elem; };

      submodule = if submoduleVisible then {
        name = "attrset";
        fields = normalizeOptionSet (ty.getSubOptions [ ]);
      } else {
        name = "any";
      };
      deferredModule = submodule;

      optionType = { name = "attrset"; rest = anything; };

      # enum
      # either
      # oneOf
      # coerceTo
    }.${ty.name} or { name = "any"; };
  in
    assert ty._type or null == "option-type";
    ty';

  # Modified from `lib.optionAttrSetToDocList`.
  normalizeOptions = opt: let
    # visible: true | false | "shallow"
    visible = (opt.visible or true != false) && !(opt.internal or false);
    submoduleVisible = visible && (opt.visible or true == true);

    opt' = {
      description = renderDoc (opt.description or null);
      declarations = filter (x: x != unknownModule) opt.declarations;
      readOnly = opt.readOnly or false;
      type = normalizeType submoduleVisible opt.type;
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
    };
  in
    if opt._type or null == "option" then
      if visible then
        opt'
      else
        null
    else {
      type = {
        name = "attrset";
        fields = normalizeOptionSet opt;
      };
    };

  normalizeOptionSet = opts:
    filterAttrs (k: v: k != "_module" && k != "_freeformOptions" && !isNull v)
      (mapAttrs (_: normalizeOptions) opts);

in
  if pathExists libPath && pathExists moduleListPath
    && builtins.compareVersions trivial.release "22.11" >= 0
  then normalizeOptionSet eval.options
  else { }
