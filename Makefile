
all:
	python -O memsim.py

test:
	python test.py

clean:
	find . -name "*.py[oc]" -exec rm {} \;
	find . -name "__pycache__" -exec rm -fr {} \;

