;;;; “Files” access for JSCL in web browsers
;;;;
;;;; (feel free to add local filesystem via node.js as well)
;;;;
;;;; Bruce-Robert Fenn Pocock <brfennpocock@star-hope.org>

#|

Overview:

This is meant to  handle a few different types of  streams that can have
“names” assigned to them, but none of them are actually “filesystems” in
the traditional Unix sense.

Mapping from a PATHNAME follows:

“Device” is being  used to mean “Protocol” of a  URI.

When “Device” is a  string that does not contain a “:” it  is meant to be
handled as a URI by the  containing web browser. Typically, this will be
HTTP or HTTPS, or perhaps FTP.

For typical URI's  of this type: “Host” is the  remote host and optional
port   number;  eg,   “www.star-hope.org”  or   “www.star-hope.org:555”;
“Directory” is the URI up to the last “/”; “Name” is the part of the URI
after the  last “/”, including  any dots  or “extensions” and  the like;
“Type”  is the  MIME media-type,  including any  optional parts  such as
charset; and “Version”  can be defined in terms of  HTTP headers such as
“If-Modified-Since” in constructing a  PATHNAME object, or returned (eg,
from a DIRECTORY query or TRUENAME) with headers from the host.

Some  file operations  are redefined  slightly to  make better  sense in
this context.

PROBE-FILE returns  true if an HTTP  query returns any response  code in
the  100 or  200 series,  and  false for  400 or  500 series;  following
redirections in the 300 series where possible to get a final response.

DIRECTORY  performs  approximately the  same  function  when a  non-WILD
PATHNAME is  passed in, but returns  a list of possible  variants if the
server  presents  one.  Due  to redirections,  the  results  of  calling
a DIRECTORY operation on a non-WILD PATHNAME could return multiple files
on differrent hosts, &c.

When OPENing a stream with :DIRECTION :INPUT, reading 

|#

