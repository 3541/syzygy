{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  hardeningDisable = [ "format" ];
  packages = with pkgs; [
    rustup
    rust-analyzer
    nasm
    parted
    qemu
    (haskellPackages.ghcWithPackages (pkgs: [ pkgs.shake ]))
  ];
}
