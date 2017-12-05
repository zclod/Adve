let config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {

                day3 = self.callPackage ./default.nix {};

            };
        };
    };
};

pkgs = import <nixpkgs> {inherit config;};

in {
    day3 = pkgs.haskellPackages.day3;
}
