# Revision history for StartingFramework

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.



## Notes from teams: 
Testing functions can be done through: https://putridparrot.com/blog/the-haskell-repl-basic-commands/ --> cabal repl

- Using a library to deal with time / leap years is totally fine
- Using a library for prettyprinting the calendar is highly encouraged
- Using a library (other than uu-tc) for parsing is generally not allowed 

- Question: Are we allowed to move the instance Show of DateTime from Main.hs to DateTime.hs?
Answer: By having the instance Show inside DateTime, it is reusable in Calendar.hs when deriving Show on other datatypes. Main.hs can't be imported here because of the cyclic dependency.

- Question: Should we take leap years into account? 
A: Note that there is also a Time library that has a function that does this for you

- Q: Hi, I have a question about exercise 11. And in particular bullet point 4: "How much time (in minutes) is spent in total for events with a given summary?"  Is it possible that these events are longer than a few hours? For example a few days or even a month? Because this will make the logic a lot more difficult 

A: Good question: It is not a central part of this assignment, so if you do all your time calculations yourself I think it is completely fine to add a comment "this function assumes that events stay within one day" for example.
If you use a library for dealing with time calculations (which is allowed!), then I hope that that library has a simple 'time difference' function 