{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  hardeningDisable = [ "format" ];
  packages = with pkgs; [
    rustup
    rust-analyzer
    cargo-outdated
    nasm
    parted
    qemu
    (haskellPackages.ghcWithPackages (pkgs: [ pkgs.shake ]))
  ];
}
