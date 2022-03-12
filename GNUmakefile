## GNUmakefile --- make file for Hare

# Copyright (C) 2022 Ralph Schleicher

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General
# Public License along with this program.  If not,
# see <https://www.gnu.org/licenses/>.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Code:

PACKAGE := hare
VERSION := $(shell grep -h ';;;* *Version:' hare.el | sed 's/.*: *//')
TARNAME := $(PACKAGE)-$(VERSION)

EMACS = emacs

### Rules

.PHONY: all
all: autoloads

.PHONY: autoloads
autoloads: hare-autoloads.el
hare-autoloads.el: generate-autoloads.el hare.el
	$(EMACS) --batch --load generate-autoloads.el

.PHONY: check
check: all
	$(EMACS) --batch --funcall batch-byte-compile hare.el

.PHONY: clean
clean:
	rm -f *.elc

.PHONY: icons
icons: all-icons

.PHONY: all-icons
all-icons:
	$(MAKE) -C icons all

.PHONY: clean-icons
clean-icons:
	$(MAKE) -C icons clean

### Maintenance

.PHONY: tag
tag: all
	@if test 0 != `svn status -q | grep -v "^ " | wc -l` ; then \
	    echo "Working copy is not clean" >&2 ; \
	    exit 1 ; \
	fi
	@if svn info "^/tags/$(TARNAME)" > /dev/null 2>&1 ; then \
	    echo "Tag already exists" >&2 ; \
	    exit 1 ; \
	fi
	svn copy "^/trunk" "^/tags/$(TARNAME)" -m "Version $(VERSION)."

.PHONY: sync
sync: all
	~/sync/github/push.sh emacs-hare

## GNUmakefile ends here
