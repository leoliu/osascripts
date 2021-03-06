=======================
 OSA Scripts for Emacs
=======================
 
A collection of OSA scripts. This repo was moved from `AppleScripts
<https://github.com/leoliu/applescripts>`_.

OSA Scripts
~~~~~~~~~~~

A dumping ground for smallish AppleScripts:

#. ``osx-notify``
#. ``osx-emacs-selected-p``
#. ``osx-say``
#. ``osx-summarize``
#. ``osx-empty-trash``
#. ``osx-choose-color``
#. ``osx-finder-or-terminal``

Reminders
~~~~~~~~~

Pull reminders from `Reminders.app` into an ``org-mode`` buffer. The
following commands are provided:

#. ``M-x Reminders`` to list all reminders containing some string
#. ``M-x Reminders-new-reminder`` to make a new reminder
#. ``C-k`` at the beginning of line on a heading deletes the reminder

TODO:

#. make new reminders using ``org-capture``

Notes
~~~~~

Pull notes from `Notes.app` into an ``org-mode`` buffer. Commands:

#. ``M-x Notes``
#. ``M-x Notes-new-note``
#. ``C-k`` at the beginning of line on a heading deletes the note

TODO:

#. make new notes using ``org-capture``

Contacts
~~~~~~~~

Allow `BBDB <http://savannah.nongnu.org/projects/bbdb>`_ to push and
pull from `Contacts.app`.
