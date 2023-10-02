xrdb .Xresources &
xset r rate 200 40 &
xset s off -dpms &
sh bin/sh/zor-screenlayout-default.sh &
exec sbcl --load ~/.stumpwm.d/start
