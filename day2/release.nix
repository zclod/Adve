let config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {

                day2 = self.callPackage ./default.nix {};
                machines-bytestring = self.callPackage ./machines-bytestring.nix {};

            };
        };
    };
};

pkgs = import <nixpkgs> {inherit config;};

in {
    day2 = pkgs.haskellPackages.day2;
}
