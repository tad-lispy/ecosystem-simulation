# This makefile is primarily responsible for building the website with description of the package and some demos.

demos_sources = $(wildcard src/Demos/*.elm)
demos_targets = $(patsubst src/Demos/%.elm, src/demos/%.html, ${demos_sources})

.PHONY: all
all: .installed clean-cache dist

.PHONY: install
install:
	rm -rf .installed
	make .installed

.installed: package.json package-lock.json
	@echo "Dependencies files are newer than .installed; (re)installing."
	npm clean-install
	@echo "This file is used by 'make' for keeping track of last install time. If package.json or package-lock.json are newer then this file (.installed) then all 'make *' commands that depend on '.installed' know they need to run 'npm install first'." \
		> .installed

# Run development server
.PHONY: develop
develop: .installed clean-cache demos
	# TODO: Pass the list of demos to html or js code somehow to allow dynamic switching of the demo.
	npx parcel src/index.pug 

# Build distribution files and place them where they are expected
.PHONY: dist
dist: .installed dist/index.html

dist/index.html: .installed demos src/index.pug
	npx parcel build --public-url . src/index.pug

.PHONY: demos
demos: .installed ${demos_targets} 

src/demos/%.html: src/Demos/%.elm
	npx elm make --output $@ $<

.PHONY: serve
serve: .installed dist
	npx serve dist/

.PHONY: doc-preview
doc-preview: .installed
	npx elm-doc-preview

# Nuke from orbit
clean:
	rm -rf elm-stuff/ dist/ node_modules/ .cache/ src/demos/
	rm -f .installed

clean-cache:
	rm -rf .cache/ elm-stuff/ dist/ src/demos/

