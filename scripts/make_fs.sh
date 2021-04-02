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
        case $(uname) in
            FreeBSD)
                mdconfig -d -u "$loop"
                ;;
            *)
                losetup -d "$loop"
                ;;
        esac
        echo "Destroyed ${loop}."
    fi
}

trap cleanup EXIT

limine_dir="$1"
files="$2"
image="$3"

image_size=200

if [ -e "$image" ]; then
    rm "$image"
fi

dd if=/dev/zero of="$image" bs=1M count=0 seek=$image_size
echo "Created image."

mount_params=""
case $(uname) in
    FreeBSD)
        echo "System is FreeBSD."
        loop="/dev/$(mdconfig -a -t vnode -f "$image")"

        gpart create -s MBR "$loop"
        gpart add -t fat32 -a 32k "$loop"
        echo "Created partition table."

        loop_partition="${loop}s1"
        newfs_msdos "$loop_partition"

        mount_params="-t msdosfs"
        ;;
    *)
        if [ $(uname) != "Linux" ]; then
            echo "System is unknown -- trying Linux."
        else
            echo "System is Linux."
        fi

        loop=$(losetup --find --show "$image")

        parted -s "$loop" mklabel msdos mkpart primary fat32 32768B 100% set 1 boot on
        echo "Created partition table."

        loop_partition="${loop}p1"
        mkfs.fat -F 32 "$loop_partition"
        ;;
esac
echo "Formatted partition."

mkdir -p mnt
mount $mount_params "$loop_partition" mnt
echo "Mounted ${image} (${loop_partition})."

cp -r "$files"/* mnt/
echo "Copied ${files} to image."

"${limine_dir}/bin/limine-install" "$loop"
echo "Installed limine."

if [ $(uname) == "FreeBSD" ]; then
    chown 1001:1001 "$image"
else
    chown 1000:1000 "$image"
fi
