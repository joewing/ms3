
SELECT_PROG=$(if $(shell which $(1)),$(1),$(2))

PYTHON=$(call SELECT_PROG,pypy,python)
FLAKES=pyflakes
PEP8=pep8
NOSETESTS=nosetests

all: lint
	$(MAKE) test

test:
	cd tests && nosetests --with-coverage

lint:
	$(PEP8) . ;\
	if [ $$? -eq 0 ] ; then \
		$(FLAKES) . ;\
	fi

clean:
	find . -name "*.py[oc]" -exec rm {} \;
	find . -name "__pycache__" -exec rm -fr {} \;
	rm -f .coverage

