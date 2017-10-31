# project-manager

Project Manager is a haskell-based command-line tool for dealing with the
headache of switching between common tasks in the terminal.

For example:
When switching over to working on a web-related task, a common workflow might
be:

1)  move to the directory of the server files
2)  check git to make sure you are on the most recent branch
3)  start up and background a locally hosted server
4)  start up and background a listener to check for file edits and rebuild
    the server
5)  open a browser and navigate to localhost on the proper port
6)  open a text editor or IDE and start coding.

While not horrendously complicated, this gets time consuming and boring. It
could be automated with a shell script, but these would quickly begin to
clutter your system as you lose track of what projects you are working on and
what their associated scripts are.

This project appears quite similar to eivind88's prm, but project-manager
offers (eventually) more comprehensive functionality and platform support, as
well as making the active project global, which allows for more interesting
scripting.

## Current Progress

at the moment, project-manager is roughly speaking usable, offering the
following commands:

+   __pm list__: list all existing projects
+   __pm start <name>__: activate the given project
+   __pm stop__: stop the currently active project
+   __pm create <name> <blurb>__: create a new project with a name and
    a blurb, and the rest default values
+   __pm edit <name|desc|start|stop> <project>__: edit the values of the
    given field in `$EDITOR`

Information about projects is stored in an XML file whose location is
determined by the `PM_PROJECT_FILE`

## Future Goals

project-manager is still in its very early stages, so there's a lot of things
that I want to add.

+   Ability to delete projects
+   project statuses - mark a project as active, suspended, or finished,
    and filter the `pm list` command by status
+   todos: each project can currently have a list of todo items, but these
    cannot be interacted with in any meaningful way at the moment.
+   Better error handling and usage/help messages
+   Ability to rename a project without opening an editor (perhaps
    a separate rename command?)
+   REFACTORING!! I hate working with ugly code, and a lot of it is in fact
    quite hideous, but done is better than perfect so this is at the bottom
    of the list.
