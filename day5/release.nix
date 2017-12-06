let config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {

                day5 = self.callPackage ./default.nix {};

            };
        };
    };
};

pkgs = import <nixpkgs> {inherit config;};

in {
    day5 = pkgs.haskellPackages.day5;
}
