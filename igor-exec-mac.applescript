-- Execute Igor commands externally through Mac OS X
--
-- Call this script with one or more commands passed as individual
-- arguments. The passed commands will be run, in order, by Igor.


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


to run argv
  tell application "Igor Pro"
    Do Script "Silent 2"
    repeat with i from 1 to (count argv)
      Do Script (item i of argv)
    end repeat
  end tell
end run
