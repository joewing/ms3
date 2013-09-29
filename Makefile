
PYTHON=pypy
COVERAGE=coverage

all:
	$(PYTHON) -O memsim.py

test:
	$(PYTHON) test.py

coverage:
	$(COVERAGE) run test.py
	$(COVERAGE) report -m

clean:
	find . -name "*.py[oc]" -exec rm {} \;
	find . -name "__pycache__" -exec rm -fr {} \;
	rm -f .coverage

