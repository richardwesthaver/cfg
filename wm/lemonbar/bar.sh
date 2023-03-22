#!/bin/sh

datetime() {
        DATETIME=$(date "+%a %b %d, %T")

        printf "$DATETIME"
}

# Print the clock

while true; do
        echo "%{c}%{F#FFFF00}%{B#0000FF} $(datetime) %{F-}%{B-}"
        sleep 1
done
