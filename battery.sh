#/bin/sh
acpi |
  sed 's/Battery 0: Full, //' |
  sed 's/Battery 0: Discharging, //' |
  sed 's/Battery 0: Charging, //' |
  sed 's/Battery 0: Unknown, //' |
  sed 's/remaining/left/' |
  sed 's/until charged/to full/'

