-- Return "True" if Igor Pro is running, "False" otherwise

-- License: GPL-3
--
-- Copyright (C) 2011  Jason Yamada-Hanff
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
