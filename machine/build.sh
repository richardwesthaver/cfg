#!/usr/bin/env bash
M="${!1}"
A="${!2}"
if [[ $M && $A ]]; then
    guzuta build $M \
	   --arch $A --chroot-dir ./chroot-$M \
	   --repo-name $M --repo-dir ./repo/$M/$A \
	   --srcdest $M --logdest ./logs
else
    if ! [ -x "$(command -v guzuta)" ]; then
	echo "Error: guzuta is not installed."
	echo ">> cargo install guzuta --force"
    fi
    echo "build.sh MACHINE ARCH"
    exit 1
fi
