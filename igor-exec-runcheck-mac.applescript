-- Return "True" if Igor Pro is running, "False" otherwise
if isAppRunning("Igor Pro")
   do shell script "echo \"True\""
else
   do shell script "echo \"False\""
end if

on isAppRunning(appName)
   tell application "System Events"
       (name of processes) contains appName
   end tell
end isAppRunning
