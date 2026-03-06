let pkgs = import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/tarball/fabb8c9deee281e50b1065002c9828f2cf7b2239";
  sha256 = "sha256-YaHht/C35INEX3DeJQNWjNaTcPjYmBwwjFJ2jdtr+5U=";
}) {};
in
pkgs.mkShellNoCC {
  nativeBuildInputs = with pkgs; [
    llvmPackages_21.lld
    qemu
  ];
}
