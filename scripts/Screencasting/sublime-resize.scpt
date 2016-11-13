tell application "System Events"
    tell process "Sublime Text"
        set frontmost to true
        perform action "AXRaise" of window 1
    end tell
end tell

tell application "SizeUp"
  resize to {1380, 770}
  do action Center
end tell
