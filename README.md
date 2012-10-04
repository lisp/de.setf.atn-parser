

# DE.SETF.ATN-PARSER: an atn-based BNF -> Common Lisp LR(*) parser compiler


 `de.setf.atn-parser` generates the core of the `de.setf.xml` parser.It can be used independently. At the moment, to generate a SPARQL parsr from the w3c BNF.That's why it is here.

## Status

In active use.
### Downloading

The core library and all extensions are available from [GitHub](http://github.com/lisp/de.setf.atn-parser).

### Building

`de.setf.atn-parser` is built with [`asdf`](http://www.common-lisp.net/projects/asdf).

 
## Licensing

This version is released under version 3 of the GNU Lesser General Public License ([LGPL](http://www.gnu.org/licenses/gpl.html)).
The core library has no external dependencies. `de.set.utility.walker` depends on runtime-specific introspection
extensions. `de.setf.utility.tst.rspec` depends on posix extensions for access to syslog.

--------
![made with mcl](http://www.digitool.com/img/mcl-made-1.gif "Made With MCL")


