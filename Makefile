
all:
	python -O memsim.py

test:
	python test.py

coverage:
	coverage run test.py
	coverage report -m

clean:
	find . -name "*.py[oc]" -exec rm {} \;
	find . -name "__pycache__" -exec rm -fr {} \;

