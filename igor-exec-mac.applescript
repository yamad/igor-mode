to run argv
  tell application "Igor Pro"
    repeat with i from 1 to (count argv)
      Do Script (item i of argv)
    end repeat
  end tell
end run
