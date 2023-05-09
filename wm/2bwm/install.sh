#!/usr/bin/env sh
CFG=$PWD/config.h
[-n "$1"] && CFG="$1"
git clone https://github.com/venam/2bwm /tmp/2bwm
pushd /tmp/2bwm
cp $CFG config.h
sudo make install
popd
rm -rf /tmp/2bwm
