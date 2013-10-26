
SELECT_PROG=$(if $(shell which $(1)),$(1),$(2))

PYTHON=$(call SELECT_PROG,pypy,python)
COVERAGE=$(call SELECT_PROG,coverage,python-coverage)
FLAKES=pyflakes
PEP8=pep8

all:
	$(PYTHON) -O memsim.py

test:
	$(PYTHON) test.py

lint:
	$(FLAKES) .

pep8:
	$(PEP8) .

coverage:
	$(COVERAGE) run test.py
	$(COVERAGE) report -m

clean:
	find . -name "*.py[oc]" -exec rm {} \;
	find . -name "__pycache__" -exec rm -fr {} \;
	rm -f .coverage

