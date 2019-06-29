# Course Scheduling Tool

This was made so it would be easier for me to determine the courses I want.
For some ungodly reason, my school's course scheduler doesn't let you 
scan through classes to see which ones are compatible with your current 
schedule and try out different schedules. This does.

Right now, it works with strings.
Eventually, I will make it all pretty when I learn enough GUI stuff.
    (Update: Python's GUI stuff is stupid, so most likely, I will re do this in haskell)

In general, you give it a file you make that holds courses and it lets you
play around with potential schedules. 
You can test it by running

$python3 simple_string_version.py

with the path /My-Online-Workspace/Python/Projects/SchedulingClassesWithoutConflicts/input

to see what it is like.



Note: the current file has support for input of classes, but I have yet to map a consistent save and read format so that you can just input classes painlessly from terminal. Right now, you need to make a input file in the format like that of the 'input' file in this directory.



