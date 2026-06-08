let pkgs = import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/tarball/d7a713c0b7e47c908258e71cba7a2d77cc8d71d5";
  sha256 = "sha256-6xWoytx8jFW4PF1GjRm/i/53trbpKGfz6zjzQGBr4cI=";
}) {};
in
pkgs.mkShellNoCC {
  nativeBuildInputs = with pkgs; [
    llvmPackages_21.lld
    qemu
  ];

  shellHook = "export RUSTC_BOOTSTRAP=1";
}
