
all:
	python memsim.py

test:
	python test.py

clean:
	find . -name "*.pyc" -exec rm {} \;

