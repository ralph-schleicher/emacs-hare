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

vc_states = \
up-to-date \
edited \
locked \
needs-update \
needs-merge \
unlocked-changes \
added \
removed \
conflict \
missing \
ignored \
unregistered \
nil

# Perceptually uniform colors in the sRGB color space.
red = \#D82F00
green = \#007C00
blue = \#2754FF
yellow = \#D8AB00
# Similar lightness than the primary colors.
gray1 = \#6C6C6C

color_theme = color
ifeq ($(color_theme),color)
relax = $(green)
confirm = $(red)
resolve = $(yellow)
ignore = $(gray1)
else # not color
$(error unknown color theme ‘$(color_theme)’)
endif # not color

icon_set = outline
ifeq ($(icon_set),outline)
up_to_date = check-circle
edited = pencil
locked = lock
needs_update = arrow-repeat
needs_merge = intersect
unlocked_changes = unlock
added = plus-circle
removed = x-circle
conflict = exclamation-triangle
missing = dash-circle
ignored = slash-circle
unregistered = question-circle
nil = dash
else # not outline
ifeq ($(icon_set),fill)
up_to_date = check-circle-fill
edited = pencil-square
locked = lock-fill
needs_update = arrow-repeat
needs_merge = intersect
unlocked_changes = unlock-fill
added = plus-circle-fill
removed = x-circle-fill
conflict = exclamation-triangle-fill
missing = dash-circle-fill
ignored = slash-circle-fill
unregistered = question-circle-fill
nil = dash
else # not fill
$(error unknown icon set ‘$(icon_set)’)
endif # not fill
endif # not outline

icons = \
$(addsuffix .svg,$(vc_states)) \
$(addsuffix .png,$(vc_states)) \
$(addsuffix .xpm,$(vc_states))

.PHONY: all
all: $(icons)

.PHONY: clean
clean:
	rm -f *.svg *.png *.xpm

up-to-date.svg: bootstrap.git/icons/$(up_to_date).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(relax)"/ }' $< > $@

edited.svg: bootstrap.git/icons/$(edited).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(confirm)"/ }' $< > $@

locked.svg: bootstrap.git/icons/$(locked).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(resolve)"/ }' $< > $@

needs-update.svg: bootstrap.git/icons/$(needs_update).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(resolve)"/ }' $< > $@

needs-merge.svg: bootstrap.git/icons/$(needs_merge).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(resolve)"/ }' $< > $@

unlocked-changes.svg: bootstrap.git/icons/$(unlocked_changes).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(resolve)"/ }' $< > $@

added.svg: bootstrap.git/icons/$(added).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(confirm)"/ }' $< > $@

removed.svg: bootstrap.git/icons/$(removed).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(confirm)"/ }' $< > $@

conflict.svg: bootstrap.git/icons/$(conflict).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(resolve)"/ }' $< > $@

missing.svg: bootstrap.git/icons/$(missing).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(resolve)"/ }' $< > $@

ignored.svg: bootstrap.git/icons/$(ignored).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(ignore)"/ }' $< > $@

unregistered.svg: bootstrap.git/icons/$(unregistered).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(ignore)"/ }' $< > $@

nil.svg: bootstrap.git/icons/$(nil).svg
	sed -Ee '1 { s/[ 	]+(width|height|class)="[^"]*"//g; s/fill="currentColor"/fill="$(ignore)"/ }' $< > $@

%.png: %.svg
	inkscape -w 256 -h 256 -o $@ $< 2> /dev/null

%.xpm: %.png
	convert $< $@

bootstrap.git:
	git clone https://github.com/twbs/icons.git $@

## GNUmakefile ends here
