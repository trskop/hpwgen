Hpwgen
======

Library and command line tool for generating random passwords. Library API is
very generic allowing to use arbitrary string/text and pseudo random number
generator implementation.


Command line utility
--------------------

Usage:

    hpwgen [PASSWORD_LENGTH [NUMBER_OF_PASSWORDS]]

If `PASSWORD_LENGTH` is not specified it generates passwords with 8 characters.
By default it prints as many passwords as it can fit in to 20 lines of output.
If the stdout is not a terminal, then it prints only one password, but if
`NUMBER_OF_PASSWORDS` is specified, then it overrides the default behaviour.
Only difference between cases when stdout is and isn't TTY is that if it is
then it tries to print passwords in columns.
