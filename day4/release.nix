let config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {

                day4 = self.callPackage ./default.nix {};

            };
        };
    };
};

pkgs = import <nixpkgs> {inherit config;};

in {
    day4 = pkgs.haskellPackages.day4;
}
