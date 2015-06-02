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

MKSTOG=mk-stog
MKSTOG_OCAML_SESSION=mk-stog-ocaml-session
STOG=./mystog
STOG_OCAML_SESSION=./mystog-ocaml-session
STOG_SERVER=./mystog-server
DEST_DIR=../../ocf-gh-pages
BASE_URL_OPTION=
PACKAGES=stog.markdown
STOG_OPTIONS=--stog-ocaml-session "$(STOG_OCAML_SESSION) -package yojson -I ../src -ppx ../src/ppx_ocf" -d $(DEST_DIR) $(BASE_URL_OPTION)

build:
	$(MAKE) site

site: stog
	$(STOG) $(STOG_OPTIONS) .

ocamldoc:
	rm -f refdoc/*html ref-doc/style.css
	(cd ../src && make docstog)

test:
	$(MAKE) BASE_URL_OPTION="--local" site

stog:$(STOG) $(STOG_OCAML_SESSION)
server: $(STOG_SERVER) $(STOG_OCAML_SESSION)
	$(STOG_SERVER) $(STOG_OPTIONS) .

.PHONY:$(STOG) $(STOG_OCAML_SESSION)

$(STOG):
	$(MKSTOG) -o $@ -package $(PACKAGES) -linkall

$(STOG_SERVER):
	$(MKSTOG) -o $@ -package stog.server,$(PACKAGES) -thread -linkall

$(STOG_OCAML_SESSION):
	$(MKSTOG_OCAML_SESSION) -package yojson ../src/ocf.cma -o $@

clean:
	rm -fr .stog/cache

cleanall: clean
	rm -f $(STOG) $(STOG_OCAML_SESSION) $(STOG_SERVER)


