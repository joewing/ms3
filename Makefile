
SELECT_PROG=$(if $(shell which $(1)),$(1),$(2))

PYTHON=$(call SELECT_PROG,pypy,python)
COVERAGE=$(call SELECT_PROG,coverage,python-coverage)
FLAKES=pyflakes

all:
	$(PYTHON) -O memsim.py

test:
	$(PYTHON) test.py

lint:
	$(FLAKES) .

coverage:
	$(COVERAGE) run test.py
	$(COVERAGE) report -m

clean:
	find . -name "*.py[oc]" -exec rm {} \;
	find . -name "__pycache__" -exec rm -fr {} \;
	rm -f .coverage

