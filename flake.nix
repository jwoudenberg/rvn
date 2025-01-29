{
  description = "Roc values as a serialization format";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    roc.url = "github:roc-lang/roc/0.0.0-alpha2-rolling";
  };

  outputs =
    {
      self,
      nixpkgs,
      roc,
    }:
    {
      devShell."x86_64-linux" =
        let
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
        in
        pkgs.mkShell {
          packages = [ roc.packages."x86_64-linux".cli ];
        };
    };
}
