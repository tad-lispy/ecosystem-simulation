entries := src/index.html
demos := src/Demos/*.elm

.PHONY: all
all: .installed dist

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
develop: .installed
	# TODO: Pass the list of demos to html or js code somehow to allow dynamic switching of the demo.
	npx parcel ${entries} ${demos}

# Build distribution files and place them where they are expected
.PHONY: dist
dist: .installed
	npx parcel build --public-url . ${entries} ${demos}

.PHONY: serve
serve: .installed dist
	npx serve dist/

# Nuke from orbit
clean:
	rm -rf elm-stuff/ dist/ node_modules/
	rm -f .installed
