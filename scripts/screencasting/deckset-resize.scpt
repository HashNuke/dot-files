tell application "System Events"
    tell process "Deckset"
        set frontmost to true
        perform action "AXRaise" of window 1
    end tell
end tell

tell application "SizeUp"
  resize to {1300, 740}
  do action Center
end tell
