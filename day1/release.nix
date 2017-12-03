let config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {

                day1 = self.callPackage ./default.nix {};
                machines-bytestring = self.callPackage ./machines-bytestring.nix {};

            };
        };
    };
};

pkgs = import <nixpkgs> {inherit config;};

in {
    day1 = pkgs.haskellPackages.day1;
}
