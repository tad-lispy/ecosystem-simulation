entries := src/index.html

.PHONY: all
all: .installed dist

.PHONY: install
install:
	@rm -rf .installed
	@make .installed

.installed: package.json package-lock.json
	@echo "Dependencies files are newer than .installed; (re)installing."
	@npm clean-install
	@echo "This file is used by 'make' for keeping track of last install time. If package.json, package-lock.json or elm.json are newer then this file (.installed) then all 'make *' commands that depend on '.installed' know they need to run npm install first." \
		> .installed

# Run development server
.PHONY: run
run: .installed
	@npx parcel ${entries}

# Build distribution files and place them where they are expected
.PHONY: dist
dist: .installed
	@npx parcel build --public-url . ${entries}

# Nuke from orbit
clean:
	@rm -rf elm-stuff/ dist/ node_modules/
	@rm -f .installed
