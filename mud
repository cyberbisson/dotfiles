# .Mud
#
# Example .Mud, as used by emacs' mud mode.
# Note that mud mode is not the best way to connect to LPmuds. 
#
# To automatically connect to a MOO w/o typing your password:
#   Create this file in your home directory, called .mud
#   chmod 600 .mud    (i.e. make it NON-readable)
#   Put your password in it for that server
#   In emacs, do:  C-u M-x mud <server-name>
#
#
# Format of this file:
#   server  <server-name>  <mud-type>  <host-name>  <port>
#   default-server  <server-name>
#   include  <file-name>
#   <server-name>  <character-name>  <password>
#   default  <character-name>  <password>


#
# A list of servers that you know and love.
#
server upaya            MOO     upaya.soc.neu.edu               8888
server InfoCity         MOO     infocity.ccs.neu.edu            1999
#
# Your favorite one (emacs will make this the default)
#
default-server upaya

#
# A list of characters for those servers and your password for each.
# Only fill this section in if your file is not publicly readable.
#
ServerName      Charactername   Password
#   <server-name>  <character-name>  <password>
#   default  <character-name>  <password>


#
# A list of servers that you know and love.
#
server upaya            MOO     upaya.soc.neu.edu               8888
server InfoCity         MOO     infocity.ccs.neu.edu            1999
#
# Your favorite one (emacs will make this the default)
#
default-server upaya

#
# A list of characters for those servers and your password for each.
# Only fill this section in if your file is not publicly readable.
#
ServerName      Charactername   Password

