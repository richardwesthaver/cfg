#!/usr/bin/env bash
PKG="${1}"
M="${2}"
A="${3}"
if [[ $PKG && $M && $A ]] && [ -x "$(command -v guzuta)" ]; then
    mkdir -p chroot-$M
    mkdir -p repo/$M/$A/$PKG
    mkdir -p log/$M
    guzuta build $M/PKGBUILDS/$PKG \
	   --arch $A --chroot-dir chroot \
	   --repo-name $PKG --repo-dir repo/$M/$A/$PKG \
	   --srcdest $M/PKGBUILDS/$PKG --logdest log/$M
else
    if ! [ -x "$(command -v guzuta)" ]; then
	echo "Error: guzuta is not installed."
	echo ">> cargo install guzuta --force"
    fi
    echo "build.sh MACHINE ARCH"
    exit 1
fi
