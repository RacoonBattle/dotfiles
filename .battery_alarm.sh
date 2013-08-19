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

function get_current_info() {
	PRESENT=$(grep "present:" /proc/acpi/battery/$BAT/state | awk '{print $2}')
	STATE=$(grep "charging state" /proc/acpi/battery/$BAT/state | awk '{print $3}')
	CAPACITY=$(grep "remaining capacity" /proc/acpi/battery/$BAT/state | awk '{print $3}')
	PERCENT=$(($CAPACITY*100/$MAX_BATTERY))
}

while [ true ]; do
	get_current_info
	if [ -e "/proc/acpi/battery/$BAT/state" ]; then
		if [ "$PRESENT" = "yes" ]; then

			if [ $(($PERCENT % 10)) -eq 0 ] && [ "$STATE" = "discharging" ]; then
				notify-send  -t 5000 "Battery Information" "remaining $PERCENT% "
				logger "Battery information: remaining capacity $PERCENT%"
				while [ $(($PERCENT % 10)) -eq 0 ] && [ "$STATE" = "discharging" ]; do
					get_current_info
					sleep 60
					get_current_info
				done
			fi

			if [ $PERCENT -lt $LOW_BATTERY ] && [ "$STATE" = "discharging" ]; then
				notify-send -u critical -t 10000 "Battery is LOW." "remaining $PERCENT% "
				logger "Battery warning: remaining capacity $PERCENT%"
			fi

			if [ $PERCENT -lt $CRITICAL_BATTERY ] && [ "$STATE" = "discharging" ]; then
				notify-send -u critical -t 15000 "Battery is less than $CRITICAL_BATTERY." "System have to shutdown"
				sleep 30
				notify-send -u critical -t 9000 "LOW Battery" "Power Off"
				logger "Battery alarm: remaining capacity $PERCENT%, critical low, pow off system"
				sleep 9
				$($ACTION)
			fi
		fi
	fi
	sleep $SLEEP
done
