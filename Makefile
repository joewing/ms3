
all:
	python -O memsim.py

test:
	python test.py

clean:
	find . -name "*.py[oc]" -exec rm {} \;

