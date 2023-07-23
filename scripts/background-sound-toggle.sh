currentValue=$(defaults read com.apple.ComfortSounds "comfortSoundsEnabled")

if [[ $currentValue == "0" ]]; then
  newValue="true"
else
  newValue="false"
fi

defaults write com.apple.ComfortSounds "comfortSoundsEnabled" -bool "$newValue"
launchctl kill SIGHUP gui/501/com.apple.accessibility.heard