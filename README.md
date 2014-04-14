cl-space-action
---------------

To start the game simply type in REPL.

```lisp
CL-USER> (cl-space-action:launch)
```
This is a port to CL of a game I've participated in writing when I was in high school.

TODOs:
* colliding routine for ships
  * first write in such a way, that T is returned when objects are currently being collided
  * second, write in sucha way, that T is returned if they would be collided during
    current tick of time
  * when ships collide, they become colored inside