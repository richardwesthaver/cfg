#!/bin/sh
xrdb .Xresources &
xset r rate 200 40 &
xset s off -dpms &
sh /home/ellis/zor-screenlayout-default.sh &
exec sbcl --load /home/ellis/.stumpwm.d/start
