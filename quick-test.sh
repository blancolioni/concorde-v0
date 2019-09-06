#!/bin/sh
./build/bin/concorde-driver --create
#./build/bin/concorde-driver --add-faction --account-name=root --faction-name=Republic --faction-adjective=Republican --faction-plural=Republicans --faction-color=#003153 --faction-setup-path=capital
./build/bin/concorde-driver --add-faction --faction-names-file=faction-names.config --faction-colors-file=faction-colors.config
./build/bin/concorde-driver --command-line
