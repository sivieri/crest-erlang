include ../support/include.mk

APPLICATION=crest
DOC_OPTS={dir,\"../doc\"}

all: $(EBIN_FILES_NO_DOCS)

edoc:
	$(ERL) -noshell -pa ../ebin \
		 -eval "edoc:application($(APPLICATION), \".\", [$(DOC_OPTS)])" \
		 -s init stop

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
