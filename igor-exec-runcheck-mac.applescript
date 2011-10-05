-- Return "True" if Igor Pro is running, "False" otherwise
if isAppRunning("Igor Pro")
   ECHO("True")
else
   EHCO("True")
end if

on isAppRunning(appName)
   tell application "System Events"
       (name of processes) contains appName
   end tell
end isAppRunning

on ECHO(str)
   do shell script "echo \"" & str & "\""
end ECHO