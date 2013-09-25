
all:
	python -O memsim.py

test:
	python -O test.py

clean:
	find . -name "*.py[oc]" -exec rm {} \;

