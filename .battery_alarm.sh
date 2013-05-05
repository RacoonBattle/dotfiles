#!/bin/bash

#low battery in %
LOW_BATTERY="10"
#critical battery in % (execute action)
CRITICAL_BATTERY="3"
#sleep time 4 script
SLEEP="120"
#acpi battery name
BAT="BAT0"
#action
ACTION="sudo shutdown -h now"

MAX_BATTERY=$(cat /proc/acpi/battery/$BAT/info | grep 'last full' | awk '{print$4}')

while [ true ]; do
	if [ -e "/proc/acpi/battery/$BAT/state" ]; then
		PRESENT=$(grep "present:" /proc/acpi/battery/$BAT/state | awk '{print $2}')
		if [ "$PRESENT" = "yes" ]; then

			STATE=$(grep "charging state" /proc/acpi/battery/$BAT/state | awk '{print $3}')
			CAPACITY=$(grep "remaining capacity" /proc/acpi/battery/$BAT/state | awk '{print $3}')
			PERCENT=$(($CAPACITY*100/$MAX_BATTERY))

			if [ "$PERCENT" -lt "$LOW_BATTERY" ] && [ "$STATE" = "discharging" ]; then
				notify-send -u critical -t 5000 "Battery is LOW." "remaining $PERCENT% "
			fi

			if [ "$PERCENT" -lt "$CRITICAL_BATTERY" ] && [ "$STATE" = "discharging" ]; then
				notify-send -u critical -t 9000 "Battery is less than $CRITICAL_BATTERY." "System will shutdown in 30s"
				sleep 30
				notify-send -u critical -t 9000 "LOW Battery" "Power Off" 
				sleep 3
				$($ACTION)
			fi
		fi
	fi
	sleep $SLEEP
done
