-- Execute Igor commands externally through Mac OS X
--
-- Call this script with one or more commands passed as individual
-- arguments. The passed commands will be run, in order, by Igor.
to run argv
  tell application "Igor Pro"
    Do Script "Silent 2"
    repeat with i from 1 to (count argv)
      Do Script (item i of argv)
    end repeat
  end tell
end run
