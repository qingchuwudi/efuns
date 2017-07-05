# See LICENSE for licensing information.
PROJECT = efuns
VERSION = 0.2.3
CHMOD = $(shell chmod +x ./rebar3)
REBAR = ./rebar3

.PHONY: build doc clean version release console clean

all: clean version release

build:
	@$(CHMOD)
	@$(REBAR) compile

#Generate a release 
release:
	@$(CHMOD)
	@$(REBAR) release

doc:
	@$(REBAR) edoc

version:
	@echo "Setting version:$(VERSION)"
	perl -p -i -e "s/^\s*{vsn,.*/  {vsn, \"$(VERSION)\"},/g" src/${PROJECT}.app.src
	perl -p -i -e "s/^{relx,.*/{relx, [{release, { ${PROJECT}, \"$(VERSION)\" },/g" rebar.config
	perl -p -i -e "s/^{.*/{\"$(VERSION)\",/g" src/${PROJECT}.appup.src
	@echo "Version Changed Done!"


console:
	./_build/default/rel/${PROJECT}/bin/${PROJECT} console

clean:
	@$(REBAR) clean

.PHONY: pack upgrade

pack:
	@$(REBAR) as ${VERSION} tar -i true

upgrade:
	@$(REBAR) upgrade

test:
	@$(REBAR) ct
	@$(REBAR) eunit