#!/bin/sh

set -e

if [ -z "$MESON_SOURCE_ROOT" ]; then
    cd "$MESON_SOURCE_ROOT"
fi

cleanup() {
    echo "Cleaning up."

    if [ -d mnt ]; then
        umount mnt
        rmdir mnt
        echo "Unmounted ${image}."
    fi

    if [ -e "$loop" ]; then
        losetup -d "$loop"
        echo "Destroyed ${loop}."
    fi
}

trap cleanup EXIT

files="$1"
image="$2"

image_size=$(($(du -sh "$files" | cut -d'.' -f1) + 20))

if [ -e "$image" ]; then
    rm "$image"
fi

dd if=/dev/zero of="$image" bs=1M count=$image_size
echo "Created image."

loop=$(losetup --find --show "$image")

parted -s "$loop" mklabel msdos mkpart primary fat32 32k 100% set 1 boot on
echo "Created partition table."

loop_partition="${loop}p1"

mkfs.vfat "$loop_partition"
echo "Formatted partition."

mkdir -p mnt
mount "$loop_partition" mnt
echo "Mounted ${image} (${loop_partition})."

cp -r "$files"/* mnt/
echo "Copied ${files} to image."

grub-install --target=i386-pc --boot-directory=mnt/boot --install-modules="normal multiboot2 part_msdos all_video" "$loop"
echo "Installed grub."

chown 1000:1000 "$image"
