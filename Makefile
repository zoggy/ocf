#################################################################################
#                Ocf                                                            #
#                                                                               #
#    Copyright (C) 2015 INRIA. All rights reserved.                             #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License as             #
#    published by the Free Software Foundation, version 3 of the License.       #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU Lesser General Public           #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    As a special exception, you have permission to link this program           #
#    with the OCaml compiler and distribute executables, as long as you         #
#    follow the requirements of the GNU GPL in regard to all of the             #
#    software in the executable aside from the OCaml compiler.                  #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

include master.Makefile

# Compilation
#############

all: src

src: dummy
	cd src && $(MAKE) all

re : depend clean all

# Documentation :
#################
doc: dummy
	cd src && $(MAKE) doc

# myself

master.Makefile: master.Makefile.in config.status src/META.in
	./config.status

config.status: configure
	./config.status --recheck

configure: configure.ac
	autoconf

# backup, clean and depend :
############################

distclean: clean
	cd src && $(MAKE) distclean
	$(RM) config.cache config.log config.status master.Makefile \
	src/META

clean:: dummy
	$(RM) *~ \#*\#
	cd src && $(MAKE) clean

depend: dummy
	cd src && $(MAKE) depend

dummy:

###########
# Headers
###########
HEADFILES=configure.ac configure \
	master.Makefile.in Makefile src/Makefile web/Makefile \
	checkocaml.ml src/*.ml src/*.mli

headers: dummy
	headache -h header -c .headache_config $(HEADFILES)

noheaders: dummy
	headache -r -c .headache_config $(HEADFILES)


############

webdoc:
	(cd src && $(MAKE) docstog)
	cd web && $(MAKE)



#################
# installation
#################

install: dummy
	cd src && $(MAKE) install

uninstall: dummy
	cd src && $(MAKE) uninstall


###########
# web site
###########
#WEBDEST=zoggy@forge.ocamlcore.org:/home/groups/ocaml-rdf/htdocs/
#installweb:
#	scp -r web/index.html web/style.css src/ocamldoc $(WEBDEST)
###########
# archive
###########
archive:
	git archive --prefix=ocf-$(VERSION)/ HEAD | gzip > ../ocf-gh-pages/ocf-$(VERSION).tar.gz

###########################
# additional dependencies
###########################

# DO NOT DELETE
