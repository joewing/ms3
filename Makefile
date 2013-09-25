
all:
	python memsim.py

clean:
	find . -name "*.pyc" -exec rm {} \;

